module Main
  ( main
  ) where

import qualified Criterion.Main as Criterion
import qualified Rampart

main :: IO ()
main = do
  let interval = Rampart.toInterval (3, 7 :: Int)
  Criterion.defaultMain
    [ Criterion.bench "Before"
      $ let this = Rampart.toInterval (1, 2 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "Meets"
      $ let this = Rampart.toInterval (2, 3 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "Overlaps"
      $ let this = Rampart.toInterval (2, 4 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "FinishedBy"
      $ let this = Rampart.toInterval (2, 7 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "Contains"
      $ let this = Rampart.toInterval (2, 8 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "Starts"
      $ let this = Rampart.toInterval (3, 4 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "Equal"
      $ let this = Rampart.toInterval (3, 7 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "StartedBy"
      $ let this = Rampart.toInterval (3, 8 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "During"
      $ let this = Rampart.toInterval (4, 6 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "Finishes"
      $ let this = Rampart.toInterval (6, 7 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "OverlappedBy"
      $ let this = Rampart.toInterval (6, 8 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "MetBy"
      $ let this = Rampart.toInterval (7, 8 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    , Criterion.bench "After"
      $ let this = Rampart.toInterval (8, 9 :: Int)
        in Criterion.whnf (Rampart.relate this) interval
    ]
