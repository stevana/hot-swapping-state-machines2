{-# LANGUAGE PatternSynonyms #-}

module Example.Counter where

import Control.Category

import Syntax.StateMachine.Typed hiding (Copy, First)
import Syntax.StateMachine.Untyped
import Syntax.Types

------------------------------------------------------------------------

type InputV1  = Either () ()
type OutputV1 = Either Int ()

pattern ReadCountV1 :: InputV1
pattern ReadCountV1 = Left ()

pattern IncrCountV1 :: InputV1
pattern IncrCountV1 = Right ()

------------------------------------------------------------------------

counterV1' :: T Int InputV1 OutputV1
counterV1' = Get `Case` (Get >>> Incr >>> Put)

counterV1 :: T Int String String
counterV1 = Read >>> counterV1' >>> Show

inputV1 :: Ty_
inputV1 = UTEither UTUnit UTUnit

outputV1 :: Ty_
outputV1 = UTEither UTInt UTUnit

counterV1U :: U
counterV1U =
  ReadU inputV1 .>> (GetU `CaseU` (GetU .>> IncrU .>> PutU)) .>> ShowU outputV1

------------------------------------------------------------------------

type InputV2  = Either () InputV1
type OutputV2 = Either () OutputV1

counterV2' :: T Int InputV2 OutputV2
counterV2' =
  (Int 0 >>> Put) `Case`
  Get `Case`
  (Get >>> Incr >>> Put)

counterV2 :: T Int String String
counterV2 = Read >>> counterV2' >>> Show

pattern ResetCountV2 :: InputV2
pattern ResetCountV2 = Left ()

pattern ReadCountV2 :: InputV2
pattern ReadCountV2  = Right ReadCountV1

pattern IncrCountV2 :: InputV2
pattern IncrCountV2  = Right IncrCountV1

inputV2 :: Ty_
inputV2 = UTEither UTUnit inputV1

outputV2 :: Ty_
outputV2 = UTEither UTUnit outputV1

counterV2U :: U
counterV2U =
  ReadU inputV2 .>> ((IntU 0 .>> PutU) `CaseU` GetU `CaseU` (GetU .>> IncrU .>> PutU)) .>> ShowU outputV2

------------------------------------------------------------------------

type InputV3  = Either () InputV2
type OutputV3 = Either () OutputV2

counterV3' :: T (Int, Bool) InputV3 OutputV3
counterV3' =
  (Get >>> Second Not >>> Put) `Case`
  (Int 0 :&&& Bool False >>> Put) `Case`
  (Get >>> Fst) `Case`
  (Get >>> If Decr Incr >>> (Id :&&& (Consume >>> Get >>> Snd)) >>> Put)

counterV3 :: T (Int, Bool) String String
counterV3 = Read >>> counterV3' >>> Show

pattern ToggleCountV3 :: InputV3
pattern ToggleCountV3 = Left ()

pattern ResetCountV3 :: InputV3
pattern ResetCountV3 = Right ResetCountV2

pattern ReadCountV3 :: InputV3
pattern ReadCountV3  = Right ReadCountV2

pattern IncrCountV3 :: InputV3
pattern IncrCountV3  = Right IncrCountV2

inputV3 :: Ty_
inputV3 = UTEither UTUnit inputV2

outputV3 :: Ty_
outputV3 = UTEither UTUnit outputV2

counterV3U :: U
counterV3U =
  ReadU inputV3 .>>
    ((GetU .>> SecondU NotU .>> PutU) `CaseU`
    ((IntU 0 :.&& BoolU False) .>> PutU) `CaseU`
    (GetU .>> FstU) `CaseU`
    (GetU .>> IfU DecrU IncrU .>> (IdU :.&& (ConsumeU .>> GetU .>> SndU)) .>> PutU))
  .>> ShowU outputV3
