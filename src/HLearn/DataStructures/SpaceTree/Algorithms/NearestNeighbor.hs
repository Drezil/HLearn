{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    ( 

    -- * data types
    Neighbor (..)
    
    , NeighborList (..)
    , mkNeighborList
    , getknnL
    , nl_maxdist

    , NeighborMap (..)
    , nm2list

--     , NeighborList (..)

    -- * functions
    , findNeighborMap
    , parFindNeighborMap
    , parFindNeighborMapWith
    , parFindEpsilonNeighborMap
    , parFindEpsilonNeighborMapWith
    , findNeighborList
    , findNeighborListWith 
    , findEpsilonNeighborList
    , findEpsilonNeighborListWith 
    , findNeighborList_batch


--     , knn_vector
--     , knn2_single
--     , knn2_single_parallel
--     , knn_batch 
-- 
--     -- * tmp
    )
    where

import Debug.Trace

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.DeepSeq
import Data.Int
import Data.List
import Data.Maybe 
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import Data.Monoid
import Data.Proxy
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Function.Memoize

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..),strictlist2list)

-------------------------------------------------------------------------------
-- data types 

data Neighbor dp = Neighbor
    { neighbor         :: !dp
--     , weight           :: !(Scalar dp)
    , neighborDistance :: !(Scalar dp)
    }

deriving instance (Read dp, Read (Scalar dp)) => Read (Neighbor dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (Neighbor dp)

instance Eq (Scalar dp) => Eq (Neighbor dp) where
    a == b = neighborDistance a == neighborDistance b

instance Ord (Scalar dp) => Ord (Neighbor dp) where
    compare a b = compare (neighborDistance a) (neighborDistance b)

instance (NFData dp, NFData (Scalar dp)) => NFData (Neighbor dp) where
    rnf n = deepseq (neighbor n) $ rnf (neighborDistance n)

---------------------------------------

newtype NeighborList (k::Nat) dp = NeighborList { getknn :: Strict.List (Neighbor dp) }

mkNeighborList :: Num (Scalar dp) => dp -> Scalar dp -> NeighborList k dp
mkNeighborList dp dist = NeighborList $ Neighbor dp dist :. Strict.Nil

getknnL :: NeighborList k dp -> [Neighbor dp]
getknnL = strictlist2list . getknn

deriving instance (Read dp, Read (Scalar dp)) => Read (NeighborList k dp)
deriving instance (Show dp, Show (Scalar dp)) => Show (NeighborList k dp)
deriving instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborList k dp)

nl_maxdist :: forall k dp. (KnownNat k, Fractional (Scalar dp)) => NeighborList k dp -> Scalar dp
nl_maxdist (NeighborList Strict.Nil) = infinity
nl_maxdist (NeighborList (x:.Strict.Nil)) = neighborDistance x
nl_maxdist (NeighborList xs ) = neighborDistance $ Strict.last xs

instance CanError (NeighborList k dp) where
    {-# INLINE errorVal #-}
    errorVal = NeighborList Strict.Nil

    {-# INLINE isError #-}
    isError (NeighborList Strict.Nil) = True
    isError _ = False

---------------------------------------

newtype NeighborMap (k::Nat) dp = NeighborMap 
    { nm2map :: Map.Map dp (NeighborList k dp)
    }

deriving instance (Read dp, Read (Scalar dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
deriving instance (Show dp, Show (Scalar dp), Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
deriving instance (NFData dp, NFData (Scalar dp)) => NFData (NeighborMap k dp)

nm2list :: NeighborMap k dp -> [(dp,NeighborList k dp)]
nm2list (NeighborMap nm) = Map.assocs nm

-------------------------------------------------------------------------------
-- algebra

instance (KnownNat k, MetricSpace dp, Eq dp) => Monoid (NeighborList k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = NeighborList Strict.Nil 
    mappend nl1 (NeighborList Strict.Nil) = nl1
    mappend (NeighborList Strict.Nil) nl2 = nl2
    mappend (NeighborList (x:.xs)  ) (NeighborList (y:.ys)  ) = {-# SCC mappend_NeighborList #-} case k of
        1 -> if x < y then NeighborList (x:.Strict.Nil) else NeighborList (y:.Strict.Nil)
        otherwise -> NeighborList $ Strict.take k $ interleave (x:.xs) (y:.ys)
        where
            k=fromIntegral $ natVal (Proxy :: Proxy k)

            interleave !xs Strict.Nil = xs
            interleave Strict.Nil !ys = ys
            interleave (x:.xs) (y:.ys) = case compare x y of
                LT -> x:.(interleave xs (y:.ys))
                GT -> y:.(interleave (x:.xs) ys)
                EQ -> if neighbor x == neighbor y
                    then x:.interleave xs ys
                    else x:.(y:.(interleave xs ys))

instance (KnownNat k, MetricSpace dp, Ord dp) => Monoid (NeighborMap k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = NeighborMap mempty
    mappend (NeighborMap x) (NeighborMap y) = 
        {-# SCC mappend_NeighborMap #-} NeighborMap $ Map.unionWith (<>) x y

-------------------------------------------------------------------------------
-- single tree

{-# INLINABLE findNeighborList  #-}
findNeighborList !t !query = findNeighborListWith mempty t query
-- findNeighborList :: (KnownNat k, SpaceTree t dp, Eq dp) => t dp -> dp -> NeighborList k dp

{-# INLINABLE findNeighborListWith #-}
findNeighborListWith !nl !t !q = findEpsilonNeighborListWith nl 0 t q 
-- findNeighborListWith :: 
--     ( KnownNat k
--     , SpaceTree t dp
--     , Eq dp
--     ) => NeighborList k dp -> t dp -> dp -> NeighborList k dp
-- findNeighborListWith !knn !t !query = prunefoldB (knn_catadp 1 query) (knn_cata 1 query) knn t

{-# INLINABLE findEpsilonNeighborList #-}
findEpsilonNeighborList !e !t !q = findEpsilonNeighborListWith mempty e t q

{-# INLINABLE findEpsilonNeighborListWith #-}
findEpsilonNeighborListWith :: 
    ( KnownNat k
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    ) => NeighborList k dp -> Scalar dp -> t dp -> dp -> NeighborList k dp
findEpsilonNeighborListWith !knn !epsilon !t !query = 
    {-# SCC findEpsilonNeighborListWith #-} prunefoldB_CanError (knn_catadp smudge query) (knn_cata smudge query) knn t
    where
        !smudge = 1/(1+epsilon)

{-# INLINABLE findNeighborList_batch #-}
-- findNeighborList_batch :: (KnownNat k, SpaceTree t dp, Eq dp, CanError (Scalar dp)) => V.Vector dp -> t dp -> V.Vector (NeighborList k dp)
findNeighborList_batch v st = fmap (findNeighborList st) v

-- {-# INLINABLE knn_catadp #-}
{-# INLINE knn_catadp #-}
knn_catadp :: forall k dp.
    ( KnownNat k
    , MetricSpace dp
    , Eq dp
    , CanError (Scalar dp)
    ) => Scalar dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp #-}
    if isError dist 
        then knn
        else if dp==query 
            then knn
            else knn <> (NeighborList $ (Neighbor dp dist):.Strict.Nil)
    where
        !dist = isFartherThanWithDistanceCanError dp query (nl_maxdist knn * smudge)

-- {-# INLINABLE knn_cata #-}
{-# INLINE knn_cata #-}
knn_cata :: forall k t dp. 
    ( KnownNat k
    , SpaceTree t dp
    , Floating (Scalar dp)
    , Eq dp
    , CanError (Scalar dp)
    ) => Scalar dp -> dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-} 
    if stNode t==query 
        then if isError knn
            then NeighborList $ (Neighbor (stNode t) infinity):.Strict.Nil
            else knn
        else if isError dist 
            then errorVal
            else knn <> (NeighborList $ (Neighbor (stNode t) dist):.Strict.Nil)
--     if isError dist 
--         then errorVal
--         else if stNode t==query 
--             then if isError knn
--                 then NeighborList $ (Neighbor (stNode t) infinity):.Strict.Nil
--                 else knn
--             then knn
--             else knn <> (NeighborList $ (Neighbor (stNode t) dist):.Strict.Nil)
    where
        !dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query (nl_maxdist knn * smudge)

-- {-# INLINABLE knn_catadp #-}
-- knn_catadp :: forall k dp.
--     ( KnownNat k
--     , MetricSpace dp
--     , Eq dp
--     ) => Scalar dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
-- knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp2 #-}
--     case isFartherThanWithDistance dp query (nl_maxdist knn * smudge) of
--         Strict.Nothing -> knn
--         Strict.Just dist -> if dp==query 
--             then knn
--             else knn <> (NeighborList $ (Neighbor dp dist):.Strict.Nil)

-- {-# INLINABLE knn_cata #-}
-- knn_cata :: forall k t dp. 
--     ( KnownNat k
--     , SpaceTree t dp
--     , Eq dp
--     ) => Scalar dp -> dp -> t dp -> NeighborList k dp -> Strict.Maybe (NeighborList k dp)
-- knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-} 
--     case stIsMinDistanceDpFartherThanWithDistance t query (nl_maxdist knn * smudge) of
--         Strict.Nothing -> Strict.Nothing
--         Strict.Just dist -> if stNode t==query 
--             then Strict.Just knn
--             else Strict.Just $ knn <> (NeighborList $ (Neighbor (stNode t) dist):.Strict.Nil)

---------------------------------------

{-# INLINABLE findNeighborMap #-}
findNeighborMap :: 
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    ) => DualTree (t dp) -> NeighborMap k dp
findNeighborMap dual = {-# SCC knn2_single_parallel #-} reduce $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMap #-}
parFindNeighborMap :: 
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    ) => DualTree (t dp) -> NeighborMap k dp
parFindNeighborMap dual = {-# SCC knn2_single_parallel #-} (parallel reduce) $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMapWith #-}
parFindNeighborMapWith ::
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    ) => NeighborMap k dp -> DualTree (t dp) -> NeighborMap k dp
parFindNeighborMapWith (NeighborMap nm) dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ Map.singleton dp $ findNeighborListWith (Map.findWithDefault mempty dp nm) (reference dual) dp) 
        (stToList $ query dual)

{-# INLINABLE parFindEpsilonNeighborMap #-}
parFindEpsilonNeighborMap e d = parFindEpsilonNeighborMapWith mempty e d

{-# INLINABLE parFindEpsilonNeighborMapWith #-}
parFindEpsilonNeighborMapWith ::
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    ) => NeighborMap k dp -> Scalar dp -> DualTree (t dp) -> NeighborMap k dp
parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ Map.singleton dp $ findEpsilonNeighborListWith (Map.findWithDefault mempty dp nm) epsilon (reference dual) dp) 
        (stToList $ query dual)
