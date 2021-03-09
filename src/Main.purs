module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Lazy (Lazy, defer, force)
import Data.List.Lazy (List, nil, cons, step)
import Data.List.Lazy.Types (Step(..))
import Data.Traversable (traverse_)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

foreign import write :: String -> Effect Unit

-- | Haskell: data Data = MkData Int
--
-- Note that the other is always implicitly lazy, and Int is lazy.
type Data = Lazy Data'
data Data' = MkData (Lazy Int)

-- | Haskell: data StrictData = MkStrict !Int
--
-- Note that "strict" means we can store just Int rather than Lazy Int.
type StrictData = Lazy StrictData'
data StrictData' = MkStrict Int

-- | Smart constructor that forces. Haskell implicitly calls a smart
-- constructor for any constructor call that has "strict" fields.
mkStrict :: Lazy Int -> StrictData'
mkStrict lazyN = MkStrict $ force lazyN

-- | Haskell: newtype Newtype = MkNewtype Int
--
-- Note that all that newtype does is omit the outer Lazy that always
-- wraps "data" constructors.
newtype Newtype = MkNewtype (Lazy Int)

-- | Simulate Haskell. This thunk throws a JS exception.
undefined :: forall a. Partial => Unit -> a
undefined = \_ -> crashWith "undefined"

-- Pattern matching on a "data" constructor always forces.
--
-- Haskell: fData (MkData n) = [1, n]
fData :: Data -> List (Lazy Int)
fData lazyX = case force lazyX of
  MkData lazyN -> cons (defer \_ -> 1) $ cons lazyN nil

-- | Pattern matching on a "data" constructor always forces.
--
-- Haskell: fStrict (MkStrict n) = [1, n]
fStrict :: StrictData -> List (Lazy Int)
fStrict lazyX = case force lazyX of
  MkStrict strictN ->
    -- Need to recreate Lazy Int.
    let lazyN = defer \_ -> strictN
    in cons (defer \_ -> 1) $ cons lazyN nil

-- | Pattern matching on a newtype constructor does no forcing.
--
-- Haskell: fNewtype (MkNewtype n) = [1, n]
fNewtype :: Newtype -> List (Lazy Int)
fNewtype (MkNewtype lazyN) = cons (defer \_ -> 1) $ cons lazyN nil

-- | Fails: traverse_ ends up forcing right to left!
-- Intent: output one line per element.
wrongLazyListLazyLogShow :: forall a. (Show a) => List (Lazy a) -> Effect Unit
wrongLazyListLazyLogShow = traverse_ (logShow <<< force)

-- | Had to write the loop by hand to make work left to right.
lazyListLazyPrintLine :: forall a. (Show a) => List (Lazy a) -> Effect Unit
lazyListLazyPrintLine lazyList = do
  write "["
  case step lazyList of
    Nil -> pure unit
    Cons lazyX rest -> do
      write $ show $ force lazyX
      printRest rest
  write "]\n"

printRest :: forall a. (Show a) => List (Lazy a) -> Effect Unit
printRest lazyList = case step lazyList of
  Nil -> pure unit
  Cons lazyX rest -> do
    write ", "
    write $ show $ force lazyX
    printRest rest

main :: Effect Unit
main = unsafePartial do
  -- Outputs nothing before crashing.
  --lazyListLazyPrintLine $ fData $ defer undefined

  -- Outputs "[1," before crashing.
  --lazyListLazyPrintLine $ fData (defer \_ -> MkData $ defer undefined)

  -- Outputs nothing before crashing, because smart constructor mkStrict,
  -- when called, immediately forces the undefined thunk.
  --lazyListLazyPrintLine $ fStrict (defer \_ -> mkStrict $ defer undefined)

  -- Outputs "[1," before crashing.
  lazyListLazyPrintLine $ fNewtype (MkNewtype $ defer undefined)
