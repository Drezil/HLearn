{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Vector
import SubHask.Algebra.Container

import HLearn.Data.LoadData
import HLearn.Classifiers.Linear
import HLearn.History

import qualified Prelude as P
import System.IO

--------------------------------------------------------------------------------

main = do
    xs :: BArray (Labeled' (SVector "dyn" Double) (Lexical String))
       <- loadCSVLabeled' 0 "datasets/csv/uci/wine.csv"
       -- <- loadCSVLabeled' 8 "datasets/csv/uci/pima-indians-diabetes.csv"

    glm <- runHistory
        ( (displayFilter (maxReportLevel 2) dispIteration)
        + summaryTable
        )
        $ trainLogisticRegression 1e-3 xs

    putStrLn $ "loss_01       = "++show (validate loss_01       (toList xs) glm)
    putStrLn $ "loss_logistic = "++show (validate loss_logistic (toList xs) glm)
    putStrLn $ "loss_hinge    = "++show (validate loss_hinge    (toList xs) glm)

    let twoclassdata = (\(Labeled' x y) -> Labeled' x (y == Lexical "1")) P.<$> toList xs
    svm <- runHistory
        ( (displayFilter (maxReportLevel 2) dispIteration)
        + summaryTable
        )
        $ trainSVM_ (\x y -> let d = x-y in exp $ - (d<>d)) 10 (0.001) 0.5 twoclassdata
    
    print $ show $ (\(a,_,_) -> a) P.<$> (values $ alphas svm)
    print $ show $ P.zip (yLabeled' P.<$> twoclassdata) $ (>= 0) . decide svm . xLabeled' P.<$> twoclassdata

--     putStrLn ""
--     print $ show $ weights glm!Lexical "1"
--     print $ show $ weights glm!Lexical "2"
--     print $ show $ weights glm!Lexical "3"

    putStrLn "done."
