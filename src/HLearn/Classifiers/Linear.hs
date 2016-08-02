{-# LANGUAGE DoAndIfThenElse #-}
module HLearn.Classifiers.Linear
    where

import SubHask
import SubHask.Category.Trans.Derivative
import SubHask.Compatibility.Containers
import SubHask.Algebra.Vector
import SubHask.Algebra.Array

import HLearn.History
import HLearn.Optimization.Univariate
import HLearn.Optimization.Multivariate

import qualified Prelude as P
import qualified Data.List as L
import Debug.Trace

import Control.Monad.Trans (liftIO)

--------------------------------------------------------------------------------

-- | The data type to represent arbitrary <https://en.wikipedia.org/wiki/Linear_classifier linear classifiers>.
-- Important examples include least squares regression, logistic regression, and support vector machines.
-- In statistics, these models are called <https://en.wikipedia.org/wiki/Generalized_linear_model Generalized Linear Models>.
data GLM x y = GLM
    { weights :: Map' y x
    , numdp :: Scalar x
    }

deriving instance (Show x, Show y, Show (Scalar x)) => Show (GLM x y)

type instance Scalar (GLM x y) = Scalar x


-- | The data type to represent a SVM is similar to the GLM - but in an SVM the weights of the model are just a linear
-- combination of some vectors (the so-called support-vectors).
-- \sum_i \alpha_i = 1; w = \sum_i \alpha_i x_i

data SVM x = SVM
    { alphas :: UVector "datapoints" (Scalar x)
    , svmdata :: UArray (Labeled' x Bool)
    , kernel :: x -> x -> Scalar x
    , numa :: Int
    }

instance (Show x, Show (Scalar x), Show (UVector "datapoints" (Scalar x)), Show (UArray (Labeled' x Bool))) => Show (SVM x) where
        show (SVM a v k numa) = "SVM { alphas = " ++ show a ++ ", svmdata = " ++ show v ++ ", numa = " ++ show numa ++ " }"

type instance Scalar (SVM x) = Scalar x

--------------------------------------------------------------------------------

-- type Foldable' xs x = (Foldable xs, Elem xs~x, Scalar xs~Int)

type IsFoldable xs x = {-forall xs.-} (Foldable xs, Elem xs~x, Scalar xs~Int)

{-# INLINEABLE trainLogisticRegression #-}
trainLogisticRegression ::
    ( Ord y
    , Show y
    , Hilbert x
    , BoundedField (Scalar x)
    ) => Scalar x                                           -- ^ regularization parameter
      -> IsFoldable xys (Labeled' x y) => xys               -- ^ dataset
      -> ( cxt (LineBracket (Scalar x))
         , cxt (Iterator_cgd x)
         , cxt (Iterator_cgd (Scalar x))
         , cxt (Iterator_brent (Scalar x))
         , cxt (Backtracking x)
         , cxt (Map' y x)
         , cxt Int
         ) => History cxt (GLM x y)
trainLogisticRegression lambda xs = trainGLM_
    ( fminunc_cgd_
--         hestenesStiefel
--         polakRibiere
--         steepestDescent
        fletcherReeves
        (lineSearch_brent (stop_brent 1e-6 || maxIterations 50 || noProgress || fx1grows))
--         (backtracking (strongCurvature 1e-10))
--         (backtracking fx1grows)
        (mulTolerance 1e-9 || maxIterations 50 || noProgress {- || fx1grows-})
    )
    loss_logistic
    lambda
    (toList xs)

{-# INLINEABLE trainGLM_ #-}
trainGLM_ :: forall xys x y cxt opt.
    ( Ord y
    , Show y
    , Hilbert x
    ) => Has_x1 opt x => (x -> C1 (x -> Scalar x) -> forall s. History_ cxt s (opt x))   -- ^ optimization method
      -> (y -> Labeled' x y -> C2 (x -> Scalar x))          -- ^ loss function
      -> Scalar x                                           -- ^ regularization parameter
      -> [Labeled' x y]                                     -- ^ dataset
      -> History cxt (GLM x y)
trainGLM_ optmethod loss lambda xs = do
--     let ys = fromList $ map yLabeled' $ toList xs :: Set y
--     ws <- fmap sum $ mapM go (toList ys)
--     return $ GLM
--         { weights = ws
--         , numdp = fromIntegral $ length xs
--         }
    let Just (y0,ys) = uncons (fromList $ map yLabeled' $ toList xs :: Set y)
    ws <- fmap sum $ mapM go (toList ys)
    return $ GLM
        { weights = insertAt y0 zero ws
        , numdp = fromIntegral $ length xs
        }
    where
        go :: Show y => y -> forall s. History_ cxt s (Map' y x)
        go y0 = beginFunction ("trainGLM("++show y0++")") $ do
            w <- fmap x1 $ optmethod zero (totalLoss y0)
            return $ singletonAt y0 w

        totalLoss :: y -> C1 (x -> Scalar x)
        totalLoss y0 = unsafeProveC1 f f'
            where
                g = foldMap (loss y0) xs

                f  w = (           g $ w) + lambda*size w
                f' w = (derivative g $ w) + lambda*.w

--------------------------------------------------------------------------------


-- FIXME: Types won't work out ..
{-# INLINEABLE trainSVM_ #-}
trainSVM_ :: forall xys x y cxt.
        ( Hilbert x
        , Bounded (Scalar x)
        , BoundedField (Scalar x)
        , Show (Scalar x)
        , Scalar x ~ Double
        , x ~ UVector "dim" (Scalar x)
        ) => (x -> x -> Scalar x)                               -- ^ Kernel
        -> Scalar x                                             -- ^ C-Parameter
        -> Scalar x                                             -- ^ Convergence-difference
        -> Scalar x                                             -- ^ Learning rate
        -> [Labeled' x Bool]                                    -- ^ Dataset with 2 classes
        -> ( cxt (Iterator_cgd (UVector "datapoints" (Scalar x)))
           , cxt (Iterator_cgd (Scalar x))
           , cxt (LineBracket (Scalar x))
           , cxt (Iterator_brent (Scalar x))
           ) => History cxt (SVM x)
trainSVM_ k c crit eta xs = do
        let init_alphas = (unsafeToModule $ L.take (length xs) $ L.repeat zero) :: UVector "datapoints" (Scalar x)
            sdata = fromList xs
        alphas' <- fmap x1 $ gcd k c sdata init_alphas --untilConvergence c crit eta init_alphas
        return $ SVM
            { alphas = alphas'
            , svmdata = sdata
            , kernel = k
            , numa = length xs
            }
        where
                -- summed squared error
                --sse :: (Bounded (Scalar x), IsScalar x)=> Scalar x -> [Labeled' x Bool] -> IntMap' (Scalar x,x,Bool) -> Scalar (Scalar x,x,Bool)
                -- sse :: (x -> x -> Scalar x) -> Scalar x -> UArray (Labeled' x Bool) -> UVector "datapoints" (Scalar x) -> Scalar x
                sse k c xys alpha = err --(err + aiequilibrium + regularization) :: Scalar x
                        where
                                -- err :: Scalar x
                                err = doublemap (\i j -> (a i) * (a j) * (bool2num (y i)) * (bool2num (y j)) * (k (x i) (x j))) alpha
                                -- aiequilibrium :: Scalar x
                                aiequilibrium = doublemap (\i j -> (a i) * (a j) * bool2num (y i) * bool2num (y j)) alpha
                                -- regularization :: Scalar x
                                regularization = alpha <> alpha / c
                                doublemap f d = sum $ imap (\ki _ -> sum $ imap (\kj _ -> f ki kj) d) d
                                -- a :: Integral a => a -> Scalar x
                                a i = alpha ! (fromIntegral i)
                                -- y :: Integral a => a -> Bool
                                y i = yLabeled' $ xys ! (fromIntegral i)
                                -- x :: Integral a => a -> x
                                x i = xLabeled' $ xs ! (fromIntegral i)
                -- sse' :: (x -> x -> Scalar x) -> Scalar x -> UArray (Labeled' x Bool) -> UVector "datapoints" (Scalar x) -> UVector "datapoints" (Scalar x)
                sse' k c xys alpha = imap (\i _ -> err i + aiequilibrium i + regularization i) alpha
                        where
                                -- err :: Integral a => a -> Scalar x
                                err i = sum $ imap (\j _ -> (a i) * bool2num (y i) * bool2num (y j) * k (x i) (x j)) alpha
                                -- aiequilibrium :: Integral a => a -> Scalar x
                                aiequilibrium i = sum $ imap (\j _ -> (a j) * bool2num (y i) * bool2num (y j)) alpha
                                -- regularization :: Integral a => a -> Scalar x
                                regularization i = (a i)*(a i) / (2*c)
                                -- a :: Integral a => a -> Scalar x
                                a i = alpha ! (fromIntegral i)
                                -- y :: Integral a => a -> Bool
                                y i = yLabeled' $ xys ! (fromIntegral i)
                                -- x :: Integral a => a -> x
                                x i = xLabeled' $ xys ! (fromIntegral i)
                -- minFunc :: (x -> x -> Scalar x) -> Scalar x -> UArray (Labeled' x Bool) -> C1 (UVector "datapoints" (Scalar x) -> Scalar x)
                minFunc k c sdata = unsafeProveC1 f f'
                    where
                        -- f :: UVector "datapoints" (Scalar x) -> Scalar x
                        f  w = sse  k c sdata w
                        -- f' :: UVector "datapoints" (Scalar x) -> UVector "datapoints" (Scalar x)
                        f' w = sse' k c sdata w
                -- gcd :: (x -> x -> Scalar x) -> Scalar x -> UArray (Labeled' x Bool) -> UVector "datapoints" (Scalar x) -> History cxt (Iterator_cgd (UVector "datapoints" (Scalar x)))
                gcd k c sdata xs = fminunc_cgd_
                             fletcherReeves
                             (lineSearch_brent (stop_brent 1e-6 || maxIterations 50 || noProgress || fx1grows))
                             (mulTolerance 1e-9 || maxIterations 50 || noProgress {- || fx1grows-})
                             xs
                             (minFunc k c sdata)

                -- untilConvergence  c crit eta al = untilConvergence' c crit eta al maxBound
                -- untilConvergence' c crit eta al err = beginFunction ("trainSVM()") $ do
                --         let da = imap (diff al) al
                --             al' = imap (opt al da (maximum $ values da)) al
                --             sa = sum $ (\(a,_,_) -> a) P.<$> values al'
                --             al'' = trace (show sa) $ imap (\_ (a,x,y) -> (a/sa,x,y)) al'
                --             err' = sse c al''
                --         if abs (err' - err) < crit then
                --             do
                --                 trace (show $ "Error: " ++ show err ++ " -> " ++show err') $ return al
                --         else
                --             do
                --                 trace (show $ "Error: " ++ show err ++ " -> " ++show err') $ untilConvergence' c crit eta al'' err'
                -- FIXME: ai with 0 can safely be removed from the map
                --opt al _ (ai,xi,yi) = (min c $ max zero $ ai + eta * (one - bool2num yi * (sum $ (\(aj,xj,yj) -> aj * bool2num yj * k xj xi) P.<$> values al)), xi, yi)
                --opt al da etainv key (ai,xi,yi) = (min c $ max zero $ trace (show ai ++ " -> " ++ show (ai + (1/etainv) * da!key)) $ ai + (1/etainv) * da!key, xi, yi)
                --diff al _ a@(ai,xi,yi) = (-ai / (2*c) - bool2num yi * (sum $ (\(aj,xj,yj) -> aj * bool2num yj * k xj xi + aj * ai) P.<$> L.filter (/= a) (values al)))


--------------------------------------------------------------------------------
-- loss functions

classify :: (Ord y, Hilbert x) => GLM x y -> x -> y
classify (GLM ws _) x
    = fst
    $ P.head
    $ L.sortBy (\(_,wx1) (_,wx2) -> compare wx2 wx1)
    $ toIxList
    $ imap (\_ w -> w<>x) ws

validate ::
    ( Ord y
    , Hilbert x
    , cat <: (->)
    ) => (y -> Labeled' x y -> (x `cat` Scalar x)) -> [Labeled' x y] -> GLM x y -> Scalar x
validate loss xs model@(GLM ws _) = sum $ map go xs -- /(fromIntegral $ length xs)
    where
        go xy@(Labeled' x y) = loss y' xy $ ws!y'
            where
                y' = classify model x

-- decide :: (Hilbert x) => SVM x -> x -> Scalar x
-- decide (SVM al sv sl k _) x = sum $ (\key -> bool2num (sl!key) * (al!key) * k (sv!key) x) P.<$> indices sv

{-# INLINEABLE loss_01 #-}
loss_01 :: (HasScalar x, Eq y) => y -> Labeled' x y -> x -> Scalar x
loss_01 y0 (Labeled' x y) = indicator $ y0/=y

{-# INLINEABLE loss_squared #-}
loss_squared :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_squared y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = 0.5 * (w<>x-labelscore)**2
        f'  w = x.*(w<>x-labelscore)
        f'' w = x><x

{-# INLINEABLE loss_logistic #-}
loss_logistic :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_logistic y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = logSumOfExp2 zero $ -labelscore*w<>x
        f'  w = -labelscore*(1-invlogit (labelscore*w<>x)) *. x
        f'' w = x><x ./ (1+exp (-labelscore*w<>x))

{-# INLINEABLE loss_hinge #-}
loss_hinge :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_hinge y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = max 0 $ 1 -labelscore*w<>x
        f'  w = if x<>w > 1 then zero else -labelscore*.x
        f'' w = zero

----------------------------------------
-- helpers

bool2num True = 1
bool2num False = -1

invlogit x = 1 / (1 + exp (-x))

-- | calculates log . sum . map exp in a numerically stable way
logSumOfExp xs = m + log (sum [ exp $ x-m | x <- xs ] )
    where
        m = maximum xs

-- | calculates log $ exp x1 + exp x2 in a numerically stable way
logSumOfExp2 x1 x2 = big + log ( exp (small-big) + 1 )
    where
        big = max x1 x2
        small = min x1 x2

-- logSumOfExp2 x1 x2 = m + log ( exp (x1-m) + exp (x2-m) )
--     where
--         m = max x1 x2
