module Main where
import Test.HUnit(Counts(..),Test(..),runTestTT,(~?=),(~:))
import Control.Monad (unless)
import Data.Vector.V2
import System.Exit (exitFailure)

-- modules under test
import Graphics.Triangulation.Delaunay (triangulate)

-- TODO: use test-framework
success :: Counts -> Bool
success cs = errors cs == 0 && failures cs == 0

tests ::  IO Counts
tests = runTestTT $ TestList 
  [ fewPointsTests
  , triPointsAreDistinctTests
  ]

fewPointsTests = TestList 
  [ "triangulate no points"  ~: triangulate []                     ~?= []
  , "triangulate one point"  ~: triangulate [v2 1 1]               ~?= []
  , "triangulate two points" ~: triangulate [v2 1 1, v2 2 2]       ~?= []
  , "triangulate many dups"  ~: triangulate (replicate 5 (v2 1 1)) ~?= []
  ]
  where
    v2 = Vector2 

triPointsAreDistinctTests = TestList 
  [ "each tri has distinct pts" ~: filter (not . hasDistinctPoints) (triangulate pts) ~?= []
  ]
  where 
    pts = [v 0 7, v 24 33, v 10 13, v 20 0, v 22 11] where v = Vector2

hasDistinctPoints :: (Vector2, Vector2, Vector2) -> Bool
hasDistinctPoints (p1,p2,p3) = p1/=p2 && p1/=p3 && p2/=p3

main = do
  result <- tests
  unless (success result) exitFailure

--
-- TODO: make test case for
--  >>> triangulate [Vector2 1.418439095146873 38.89932965930318,Vector2 1.418500758183686 38.89930499410096,Vector2 1.418520885169773 38.89929697682122,Vector2 1.418545038944989 38.89933091302727,Vector2 1.418529476515102 38.89933070239795,Vector2 1.418529587791777 38.89933088411472,Vector2 1.418553977399164 38.89936977930514,Vector2 1.418567795412766 38.89936546105435,Vector2 1.418632404600587 38.89947815621338,Vector2 1.4185497042005761 38.89951028581766,Vector2 1.418430729171164 38.89933251954334,Vector2 1.418439095146873 38.89932965930318] 
