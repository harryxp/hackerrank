import Control.Monad (replicateM)
import Data.Array
import Data.List (intercalate, intersect, partition, permutations, transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Text.Regex.Posix (MatchArray, Regex, matchAll, makeRegex)

import Test.HUnit

---- top level

dimension = 10

main :: IO ()
main = replicateM (dimension+1) getLine >>= putStrLn . showCrosswords . solveCrosswords . parseInput

type Coordinates = (Int, Int)
data Orientation = H | V
  deriving (Eq, Show)

data Blank = Blank
  { start       :: Coordinates
  , end         :: Coordinates
  , len         :: Int
  , orientation :: Orientation
  } deriving (Eq, Show)

type FilledBlank = (Blank, String)

tests = TestList
  [ TestLabel "parseInput" testParseInput
  , TestLabel "solveCrosswords" testSolveCrosswords
  , TestLabel "showCrosswords" testShowCrosswords
  , TestLabel "showOneLine horizontal" testShowOneLineHorizontal
  , TestLabel "showOneLine vertical" testShowOneLineVertical
  ]

---- parseInput

parseInput :: [String] -> ([Blank], [String])
parseInput input =
  let grid :: [String]
      grid = take dimension input
      words :: [String]
      words = (splitOn ";" . head . drop dimension) input
      hBlanks = (concat . map (constructBlanks H) . getIndiceAndRows) grid
      vBlanks = (concat . map (constructBlanks V) . getIndiceAndCols) grid
  in
    (hBlanks ++ vBlanks, words)

getIndiceAndRows :: [[a]] -> [(Int, [a])]
getIndiceAndRows m = zip [1..dimension] m

getIndiceAndCols :: [[a]] -> [(Int, [a])]
getIndiceAndCols m = getIndiceAndRows (transpose m)

constructBlanks :: Orientation -> (Int, String) -> [Blank]
constructBlanks o (index, line) =
  let segments :: [MatchArray]
      segments = matchAll (makeRegex "--+" :: Regex) line -- at least two minuses
  in
    map (constructBlank o index) segments

constructBlank :: Orientation -> Int -> MatchArray -> Blank
constructBlank o index segment =
  let matchOffsetAndLength = segment ! 0
      matchOffset = fst matchOffsetAndLength + 1
      matchLength = snd matchOffsetAndLength
      startCoord = if o == H then (index,matchOffset) else (matchOffset,index)
      endCoord = if o == H then (index,matchOffset+matchLength-1) else (matchOffset+matchLength-1,index)
  in
    Blank { start = startCoord , end = endCoord , len = matchLength , orientation = o }

testParseInput = TestCase (assertEqual "parseInput" expected (parseInput input))
  where
    expected = ([ Blank {start=(4,2),end=(4,6),len=5,orientation=H}
                , Blank {start=(8,3),end=(8,8),len=6,orientation=H}
                , Blank {start=(1,2),end=(6,2),len=6,orientation=V}
                , Blank {start=(4,6),end=(10,6),len=7,orientation=V}
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

---- solveCrosswords
-- the idea is that we just simply pair up Blanks with words and filter out the illegal ones
solveCrosswords :: ([Blank], [String]) -> [FilledBlank]
solveCrosswords (blanks, words) =
  let solutions = filter validateCrosswords (zipWithPermutation blanks words)
  in
    case solutions of
      []        -> error "no solution found"
      otherwise -> head solutions

validateCrosswords :: [FilledBlank] -> Bool
validateCrosswords xs = all validatePair (choose2 xs)

validatePair :: (FilledBlank, FilledBlank) -> Bool
validatePair ((b1, s1), (b2, s2)) =
  len b1 == length s1 && len b2 == length s2 &&
  (orientation b1 == orientation b2 || joinProperly (b1, s1) (b2, s2))

joinProperly :: FilledBlank -> FilledBlank -> Bool
joinProperly (b1, s1) (b2, s2) =
  let coords1 = getAllCoords b1
      coords2 = getAllCoords b2
      junction = intersect coords1 coords2
  in
    null junction || getCharAtCoords (b1, s1) (head junction) == getCharAtCoords (b2, s2) (head junction)

-- get all the coordinates a Blank occupies
getAllCoords :: Blank -> [Coordinates]
getAllCoords b@Blank {start=(x,y),end=_,len=l,orientation=o} =
  case o of
    H -> map (\n -> (x,y+n)) [0..l-1]
    V -> map (\n -> (x+n,y)) [0..l-1]

getCharAtCoords :: FilledBlank -> Coordinates -> Char
getCharAtCoords (b@Blank {start=(x,y),end=_,len=_,orientation=o}, word) (jx,jy) =
  case o of
    H -> word !! (jy-y)
    V -> word !! (jx-x)

-- zipWithPermutation ['a','b'] [1,2] yields [[('a',1), ('b',2)], [('a',2), ('b',1)]]
zipWithPermutation :: [a] -> [b] -> [[(a,b)]]
zipWithPermutation [] [] = []
zipWithPermutation (x:[]) (y:[]) = [[(x, y)]]
zipWithPermutation xs ys =
  if length xs /= length ys
  then error "lists must be of the same length"
  else map (zip xs) (permutations ys)

-- choose2 [1,2,3] yields [(1,2), (1,3), (2,3)]
choose2 :: [a] -> [(a,a)]
choose2 [] = []
choose2 (x:[]) = []
choose2 (x:xs) = map (\x' -> (x, x')) xs ++ choose2 xs

testSolveCrosswords = TestCase (assertEqual "solveCrosswords" expected (solveCrosswords input))
  where
    expected = [ (Blank {start=(4,2),end=(4,6),len=5,orientation=H}, "DELHI")
               , (Blank {start=(8,3),end=(8,8),len=6,orientation=H}, "ANKARA")
               , (Blank {start=(1,2),end=(6,2),len=6,orientation=V}, "LONDON")
               , (Blank {start=(4,6),end=(10,6),len=7,orientation=V}, "ICELAND")
               ]
    input = ([ Blank {start=(4,2),end=(4,6),len=5,orientation=H}
             , Blank {start=(8,3),end=(8,8),len=6,orientation=H}
             , Blank {start=(1,2),end=(6,2),len=6,orientation=V}
             , Blank {start=(4,6),end=(10,6),len=7,orientation=V}
             ],
             ["LONDON","DELHI","ICELAND","ANKARA"])

---- showCrosswords

showCrosswords :: [FilledBlank] -> String
showCrosswords xs =
  let (horizontals, verticals) = partition (\(b, _) -> orientation b==H) xs
  in
    (intercalate "\n" . showHorizontalWords horizontals . showVerticalWords) verticals

showHorizontalWords :: [FilledBlank] -> [String] -> [String]
showHorizontalWords horizontals lines =
  let horizontalsByRow :: Map.Map Int [FilledBlank]
      horizontalsByRow = Map.fromListWith (++) (map (\p@(b, _) -> (fst (start b), [p])) horizontals)
  in map (\i -> showOneLine (lines !! (i-1)) (Map.lookup i horizontalsByRow)) [1..dimension]

showVerticalWords :: [FilledBlank] -> [String]
showVerticalWords verticals =
  let verticalsByColumn :: Map.Map Int [FilledBlank]
      verticalsByColumn = Map.fromListWith (++) (map (\p@(b, _) -> (snd (start b), [p])) verticals)
  in transpose (map (\i -> showOneLine (replicate dimension '+') (Map.lookup i verticalsByColumn)) [1..dimension])

showOneLine :: String -> Maybe [FilledBlank] -> String
showOneLine background fBlanksMaybe =
  case fBlanksMaybe of
    Nothing       -> background
    Just fblanks  -> foldl showWord background fblanks

showWord :: String -> FilledBlank -> String
showWord background (Blank {start=(x,y),end=_,len=l,orientation=o}, word) =
  let onset = if o == H then y else x
      prefix = take (onset-1) background
      suffix = drop (onset-1+l) background
  in intercalate "" [prefix, word, suffix]

testShowOneLineHorizontal  = TestCase (assertEqual "showOneLine horizontal" expected (showOneLine (replicate dimension '+') input))
  where
    expected = "++GET+UP++"
    input = Just
      [ (Blank {start=(2,3),end=(2,5),len=3,orientation=H}, "GET")
      , (Blank {start=(2,7),end=(2,8),len=2,orientation=H}, "UP")
      ]

testShowOneLineVertical = TestCase (assertEqual "showOneLine vertical" expected (showOneLine (replicate dimension '+') input))
  where
    expected = "++GET+UP++"
    input = Just
      [ (Blank {start=(3,2),end=(5,2),len=3,orientation=V}, "GET")
      , (Blank {start=(7,2),end=(8,2),len=2,orientation=V}, "UP")
      ]

testShowCrosswords = TestCase (assertEqual "showCrosswords" expected (showCrosswords input))
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
    input = [ (Blank {start=(4,2),end=(4,6),len=5,orientation=H}, "DELHI")
            , (Blank {start=(8,3),end=(8,8),len=6,orientation=H}, "ANKARA")
            , (Blank {start=(1,2),end=(6,2),len=6,orientation=V}, "LONDON")
            , (Blank {start=(4,6),end=(10,6),len=7,orientation=V}, "ICELAND")
            ]

