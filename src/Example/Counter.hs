module Example.Counter where

import Unsafe.Coerce

import Syntax.Typed
import Syntax.Untyped

---------------------------

tcounter :: T (Either () ()) (Either Int ())
tcounter = Loop (Second (Delay 0) >>> Distr >>> ((Snd >>> Copy) :+++ (Snd >>> (Consume :&&& Incr))) >>> Distr')


tcounterU :: U
tcounterU = LoopU UTInt (SecondU (DelayU (Opaque (unsafeCoerce 0))) .>> DistrU .>> ((SndU .>> CopyU) :.++ (SndU .>> (ConsumeU :.&& IncrU))) .>> DistrU')

  --         _ :: T (Either (Int, Int) ((), Int)) (Either Int (), Int)
tcounter2 :: T (Either () ()) (Either Int ())
tcounter2 = Loop (Second (Delay 0) >>> Distr >>> ((Snd >>> Copy) :+++ (Snd >>> (Consume :&&& (Incr >>> Incr)))) >>> Distr')

readCount :: Either () ()
readCount = Left ()

incrCount :: Either () ()
incrCount = Right ()
