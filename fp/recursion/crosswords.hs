import Control.Monad (replicateM)
import Data.Array
import Data.List (intercalate, intersect, permutations, transpose)
import Data.List.Split (splitOn)
import Text.Regex.Posix (MatchArray, Regex, matchAll, makeRegex)

import Test.HUnit

---- top level

dimension = 10

main :: IO ()
main = replicateM (dimension+1) getLine >>= (putStrLn . solveCrosswords)

type Coordinates = (Int, Int)

data Blank = Blank
  { start       :: Coordinates
  , end         :: Coordinates
  , len         :: Int
  , orientation :: Char                 -- 'h' or 'v'
  } deriving (Eq, Show)

solveCrosswords :: [String] -> String
solveCrosswords lines = (printOutput . reallySolveCrosswords . parseInput) lines

tests = TestList
  [ TestLabel "parseInput" testParseInput
  , TestLabel "reallySolveCrosswords" testReallySolveCrosswords
  , TestLabel "printOutput" testPrintOutput
  ]

---- parseInput

parseInput :: [String] -> ([Blank], [String])
parseInput input =
  let grid :: [String]
      grid = take dimension input
      words :: [String]
      words = (splitOn ";" . head . drop dimension) input
      hBlanks = (concat . map (constructBlanksFromList 'h') . getIndiceAndRows) grid
      vBlanks = (concat . map (constructBlanksFromList 'v') . getIndiceAndCols) grid
  in
    (hBlanks ++ vBlanks, words)

getIndiceAndRows :: [[a]] -> [(Int, [a])]
getIndiceAndRows m = zip [1..dimension] m

getIndiceAndCols :: [[a]] -> [(Int, [a])]
getIndiceAndCols m = getIndiceAndRows (transpose m)

constructBlanksFromList :: Char -> (Int, String) -> [Blank]
constructBlanksFromList hOrV (index, v) =
  let segments :: [MatchArray]
      segments = matchAll (makeRegex "--+" :: Regex) v -- at least two minuses
  in
    map (constructBlank hOrV index) segments

constructBlank :: Char -> Int -> MatchArray -> Blank
constructBlank hOrV index segment =
  let matchOffsetAndLength = segment ! 0
      matchOffset = fst matchOffsetAndLength + 1
      matchLength = snd matchOffsetAndLength
      startCoord = if hOrV == 'h' then (index,matchOffset) else (matchOffset,index)
      endCoord = if hOrV == 'h' then (index,matchOffset+matchLength-1) else (matchOffset+matchLength-1,index)
  in
    Blank { start = startCoord , end = endCoord , len = matchLength , orientation = hOrV }

testParseInput = TestCase (assertEqual "parseInput" expected (parseInput input))
  where
    expected = ([ Blank {start=(4,2),end=(4,6),len=5,orientation='h'}
                , Blank {start=(8,3),end=(8,8),len=6,orientation='h'}
                , Blank {start=(1,2),end=(6,2),len=6,orientation='v'}
                , Blank {start=(4,6),end=(10,6),len=7,orientation='v'}
                ],
                ["LONDON","DELHI","ICELAND","ANKARA"])
    input = [ "+-++++++++"
            , "+-++++++++"
            , "+-++++++++"
            , "+-----++++"
            , "+-+++-++++"
            , "+-+++-++++"
            , "+++++-++++"
            , "++------++"
            , "+++++-++++"
            , "+++++-++++"
            , "LONDON;DELHI;ICELAND;ANKARA"
            ]

---- reallySolveCrosswords
-- the idea is that we just simply pair up Blanks with words and filter out the illegal ones
reallySolveCrosswords :: ([Blank], [String]) -> [(Blank, String)]
reallySolveCrosswords (blanks, words) =
  let solutions = filter validateCrosswords (zipWithPermutation blanks words)
  in
    case solutions of
      [] -> error "no solution found"
      otherwise -> head solutions

validateCrosswords :: [(Blank, String)] -> Bool
validateCrosswords xs = all validatePair (choose2 xs)

validatePair ((b1@Blank {start=_,end=_,len=len1,orientation=o1}, s1), (b2@Blank {start=_,end=_,len=len2,orientation=o2}, s2)) =
  len1 == length s1 && len2 == length s2 &&
  (o1 == o2 || joinProperly b1 s1 b2 s2)

joinProperly :: Blank -> String -> Blank -> String -> Bool
joinProperly b1 s1 b2 s2 =
  let coords1 = getAllCoords b1
      coords2 = getAllCoords b2
      junction = intersect coords1 coords2
  in
    null junction || getCharAtCoords b1 (head junction) s1 == getCharAtCoords b2 (head junction) s2

-- get all the coordinates a Blank occupies
getAllCoords :: Blank -> [Coordinates]
getAllCoords b@Blank {start=(x,y),end=_,len=l,orientation=o} =
  case o of
  'h' -> map (\n -> (x,y+n)) [0..l-1]
  'v' -> map (\n -> (x+n,y)) [0..l-1]
  otherwise -> error "this is just impossible"

getCharAtCoords :: Blank -> Coordinates -> String -> Char
getCharAtCoords b@Blank {start=(x,y),end=_,len=_,orientation=o} (jx,jy) word =
  case o of
  'h' -> word !! (jy-y)
  'v' -> word !! (jx-x)
  otherwise -> error "this is just impossible"

-- zipWithPermutation ['a','b'] [1,2] yields [[('a',1), ('b',2)], [('a',2), ('b',1)]]
zipWithPermutation :: [a] -> [b] -> [[(a,b)]]
zipWithPermutation [] [] = []
zipWithPermutation (x:[]) (y:[]) = [[(x, y)]]
zipWithPermutation xs ys =
  if length xs /= length ys
  then error "lists must be of the same length"
  else map (zip xs) (permutations ys)

choose2 :: [a] -> [(a,a)]
choose2 [] = []
choose2 (x:[]) = []
choose2 (x:xs) = map (\x' -> (x, x')) xs ++ choose2 xs

testReallySolveCrosswords = TestCase (assertEqual "reallySolveCrosswords" expected (reallySolveCrosswords input))
  where
    expected = [ (Blank {start=(4,2),end=(4,6),len=5,orientation='h'}, "DELHI")
               , (Blank {start=(8,3),end=(8,8),len=6,orientation='h'}, "ANKARA")
               , (Blank {start=(1,2),end=(6,2),len=6,orientation='v'}, "LONDON")
               , (Blank {start=(4,6),end=(10,6),len=7,orientation='v'}, "ICELAND")
               ]
    input = ([ Blank {start=(4,2),end=(4,6),len=5,orientation='h'}
             , Blank {start=(8,3),end=(8,8),len=6,orientation='h'}
             , Blank {start=(1,2),end=(6,2),len=6,orientation='v'}
             , Blank {start=(4,6),end=(10,6),len=7,orientation='v'}
             ],
             ["LONDON","DELHI","ICELAND","ANKARA"])

---- printOutput

printOutput :: [(Blank, String)] -> String
printOutput = undefined

testPrintOutput = TestCase (assertEqual "printOutput" expected (printOutput input))
  where
    expected = intercalate "\n" [ "+L++++++++"
                                , "+O++++++++"
                                , "+N++++++++"
                                , "+DELHI++++"
                                , "+O+++C++++"
                                , "+N+++E++++"
                                , "+++++L++++"
                                , "++ANKARA++"
                                , "+++++N++++"
                                , "+++++D++++"
                                ]
    input = [ (Blank {start=(4,2),end=(4,6),len=5,orientation='h'}, "DELHI")
            , (Blank {start=(8,3),end=(8,8),len=6,orientation='h'}, "ANKARA")
            , (Blank {start=(1,2),end=(6,2),len=6,orientation='v'}, "LONDON")
            , (Blank {start=(4,6),end=(10,6),len=7,orientation='v'}, "ICELAND")
            ]
