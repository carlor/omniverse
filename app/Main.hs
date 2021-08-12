{-# LANGUAGE TypeSynonymInstances #-}

module Main where
import           Control.Applicative     hiding ( optional )
import           Data.Char
import           Data.List               hiding ( (!!) )
import           Data.Maybe
import qualified Data.Set                      as S
import           Prelude                 hiding ( (!!) )
import           Text.ParserCombinators.ReadP
import           Text.Printf

main = mapM_ print $ filterUniqueOutputs $ allProgramOutputsWithFilter
  (\s -> ops s > 25)

-- Program definition
type Program = [ProgramNode]

data ProgramNode = ProgramNode
  { on0 :: ProgramCommand
  , on1 :: ProgramCommand
  }
instance Show ProgramNode where
  show (ProgramNode on0 on1) = printf "0⇒%s 1⇒%s" (show on0) (show on1)
instance Read ProgramNode where
  readsPrec _ = readP_to_S parseNode
   where
    parseNode = do
      skipMany $ satisfy isSpace
      on0 <- option (on0 newNode) (string "0⇒" <|> string "0=>" >> parseCmd)
      skipMany $ satisfy isSpace
      on1 <- option (on1 newNode) (string "1⇒" <|> string "1=>" >> parseCmd)
      return ProgramNode { on0 = on0, on1 = on1 }
    parseCmd = do
      write <- (string "0" >>=> False) <|> (string "1" >>=> True)
      move  <-
        (string "←" <|> string "<-" >>=> L)
          <|> (string "→" <|> string "->" >>=> R)
      nextNode <-
        (fmap (Just . read) $ many1 $ satisfy isDigit)
          <|> (string "." >>=> Nothing)
      return ProgramCommand { write = write, move = move, nextNode = nextNode }

data ProgramCommand = ProgramCommand
  { write    :: Bool
  , move     :: Direction
  , nextNode :: Maybe Integer
  }
instance Show ProgramCommand where
  show (ProgramCommand w m nn) =
    printf "%s%s%s" (bd w) (show m) (maybe "." show nn)

data Direction = L | R
instance Show Direction where
  show L = "←"
  show R = "→"


-- Machine running
data MachineState = MachineState
  { tape   :: S.Set Integer
  , marker :: Integer
  , node   :: Maybe Integer
  , ops    :: Integer
  }
instance Show MachineState where
  show (MachineState t m n o) =
    let
      minIdx = foldr1 min $ maybeAppend (S.lookupMin t) [0, m]
      maxIdx = foldr1 max $ maybeAppend (S.lookupMax t) [0, m]
      renderIdx i =
        (if m == i then printf "→%s←" else id) . bd $ i `S.member` t
    in
      printf "#%s: q=%s %s"
             (show o)
             (maybe "H" show n)
             (concatMap renderIdx [minIdx .. maxIdx])


iterateMachine :: Program -> MachineState -> MachineState
iterateMachine prg st =
  let
    runCommand nodeIdx =
      let
        cmd = (if (marker st) `S.member` (tape st) then on1 else on0)
          (prg !! nodeIdx)
        updateDirection = case (move cmd) of
          L -> -1
          R -> 1
      in
        st
          { tape   = (if (write cmd) then S.insert else S.delete) (marker st)
                                                                  (tape st)
          , marker = marker st + updateDirection
          , node   = nextNode cmd
          , ops    = ops st + 1
          }
  in  maybe st runCommand (node st)

initialState :: MachineState
initialState =
  MachineState { tape = S.empty, marker = 0, node = Just 0, ops = 0 }

runMachine :: Program -> [MachineState]
runMachine prg =
  takeUntilNot (not . isHalted) $ iterate (iterateMachine prg) initialState

isHalted :: MachineState -> Bool
isHalted = isNothing . node

-- Program enumeration
allPrograms :: [Program]
allPrograms = concatMap allProgramsOfSize [1 ..]

-- Produces a large list, see https://oeis.org/A052200
allProgramsOfSize :: Integer -> [Program]
allProgramsOfSize n = (iterate (liftA2 (:) (allNodesWith n)) [[]]) !! n

allNodesWith :: Integer -> [ProgramNode]
allNodesWith n = liftA2 (\c0 c1 -> ProgramNode { on0 = c0, on1 = c1 })
                        (allCommandsWith n)
                        (allCommandsWith n)

allCommandsWith :: Integer -> [ProgramCommand]
allCommandsWith n = liftA2 (\c nn -> c { nextNode = nn })
                           cmds
                           (Nothing : (map Just [0 .. n - 1]))
 where
  cmds =
    [ ProgramCommand { write = False, move = L, nextNode = Nothing }
    , ProgramCommand { write = True, move = L, nextNode = Nothing }
    , ProgramCommand { write = False, move = R, nextNode = Nothing }
    , ProgramCommand { write = True, move = R, nextNode = Nothing }
    ]

-- Output enumeration
allProgramOutputs :: [(Program, MachineState)]
allProgramOutputs = allProgramOutputsWithFilter (const True)

allProgramOutputsWithFilter
  :: (MachineState -> Bool) -> [(Program, MachineState)]
allProgramOutputsWithFilter ft = iterateBuffer ft [] allPrograms

iterateBuffer
  :: (MachineState -> Bool)
  -> [(Program, MachineState)]
  -> [Program]
  -> [(Program, MachineState)]
iterateBuffer ft buf (p : rest) =
  let (done, notDone) = partition (\(_, s) -> isHalted s || ft s)
        $ map (\(p, s) -> (p, iterateMachine p s)) buf
  in  filter (isHalted . snd) done
        ++ iterateBuffer ft (notDone ++ [(p, initialState)]) rest

filterUniqueOutputs :: [(Program, MachineState)] -> [(Program, MachineState)]
filterUniqueOutputs = iterateSeen S.empty
 where
  iterateSeen seen ((p, o) : rest) =
    let s = finalString o
    in  if s `S.member` seen
          then iterateSeen seen rest
          else (p, o) : (iterateSeen (S.insert s seen) rest)

finalString :: MachineState -> String
finalString (MachineState tape _ _ _) = if S.null tape
  then "0"
  else
    let renderIdx i = bd $ i `S.member` tape
    in  concatMap renderIdx [S.findMin tape .. S.findMax tape]

-- Utility Functions
maybeAppend :: Maybe a -> [a] -> [a]
maybeAppend = maybe id (id . (:))

bd :: Bool -> String
bd False = "0"
bd True  = "1"

takeUntilNot :: (a -> Bool) -> [a] -> [a]
takeUntilNot _ []       = []
takeUntilNot p (x : xs) = x : (if p x then takeUntilNot p xs else [])

infixl 1 >>=>
a >>=> b = a >> return b

-- Taken from Prelude, except s/Int/Integer/
(!!) :: [a] -> Integer -> a
xs !! n | n < 0 = errorWithoutStackTrace "!!: negative index"
[]       !! _   = errorWithoutStackTrace "!!: index too large"
(x : _ ) !! 0   = x
(_ : xs) !! n   = xs !! (n - 1)

-- Example programs
write1sForever =
  [ newNode { on0 = ProgramCommand { write = True, move = R, nextNode = Just 0 }
            }
  ]

write1sForeverBackward =
  [ newNode { on0 = ProgramCommand { write = True, move = L, nextNode = Just 0 }
            }
  ]

write1sForeverBackAndForth =
  [ newNode { on0 = ProgramCommand { write = True, move = L, nextNode = Just 1 }
            , on1 = ProgramCommand { write = True, move = R, nextNode = Just 0 }
            }
  , newNode { on0 = ProgramCommand { write = True, move = R, nextNode = Just 0 }
            , on1 = ProgramCommand { write = True, move = L, nextNode = Just 1 }
            }
  ]

write111 =
  [ newNode { on0 = ProgramCommand { write = True, move = R, nextNode = Just 1 }
            }
  , newNode { on0 = ProgramCommand { write = True, move = R, nextNode = Just 2 }
            }
  , newNode { on0 = ProgramCommand { write = True, move = R, nextNode = Just 3 }
            }
  , newNode
  ]

-- Source: https://mathworld.wolfram.com/BusyBeaver.html
busyBeaver4 =
  [ newNode { on0 = ProgramCommand { write = True, move = R, nextNode = Just 1 }
            , on1 = ProgramCommand { write = True, move = L, nextNode = Just 1 }
            }
  , newNode
    { on0 = ProgramCommand { write = True, move = L, nextNode = Just 0 }
    , on1 = ProgramCommand { write = False, move = L, nextNode = Just 2 }
    }
  , newNode
    { on0 = ProgramCommand { write = True, move = R, nextNode = Nothing }
    , on1 = ProgramCommand { write = True, move = L, nextNode = Just 3 }
    }
  , newNode
    { on0 = ProgramCommand { write = True, move = R, nextNode = Just 3 }
    , on1 = ProgramCommand { write = False, move = R, nextNode = Just 0 }
    }
  ]

-- default node: writes what it reads and halts
newNode = ProgramNode
  { on0 = ProgramCommand { write = False, move = R, nextNode = Nothing }
  , on1 = ProgramCommand { write = True, move = R, nextNode = Nothing }
  }
