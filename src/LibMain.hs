module LibMain where

import Deployment
import Example.Counter
import Syntax.Pipeline.Typed

------------------------------------------------------------------------

test :: IO ()
test = run (FromList xs) (SM "counter" 0 counterV1) StdOut
  where
    xs :: [Msg String]
    xs =
      [ Item (show ReadCountV1)
      , Item (show IncrCountV1)
      , Item (show IncrCountV1)
      , Item (show ReadCountV1)
      , Upgrade "counter" counterV2 (Nothing :: Maybe (Int -> Int))
      , Item (show ReadCountV2)
      , Item (show ResetCountV2)
      , Item (show ReadCountV2)
      ]
-- >>> test
-- Item "Left 0"
-- Item "Right ()"
-- Item "Right ()"
-- Item "Left 2"
-- UpgradeSucceeded counter
-- Item "Left (Left 2)"
-- Item "Right ()"
-- Item "Left (Left 0)"
