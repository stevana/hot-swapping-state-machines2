{-# LANGUAGE GADTs #-}

module Message where

import Data.Typeable
import Data.ByteString (ByteString)
import Text.Read
import Network.Socket (Socket)

import Syntax.Pipeline.Typed
import Syntax.Pipeline.Untyped
import Syntax.StateMachine.Typed
import Syntax.StateMachine.Untyped
import Syntax.Types

------------------------------------------------------------------------

data Msg a where
  Item    :: Maybe Socket -> a -> Msg a
  Upgrade :: Maybe Socket -> Name -> Ty_ -> Ty_ -> Ty_ -> Ty_ -> U -> U -> Msg a
  UpgradeSucceeded :: Maybe Socket -> Name -> Msg a
  UpgradeFailed :: Maybe Socket -> Name -> Msg a

  Done :: Msg a

instance Show a => Show (Msg a) where
  show (Item _msock x) = "Item \"" ++ show x ++ "\""
  show (Upgrade _msock name s s' a b f g) = unwords ["Upgrade", show name, show s, show s', show a, show b, "(" ++ show f ++ ")", show g]
  show (UpgradeSucceeded _msock name) = "UpgradeSucceeded " ++ name
  show (UpgradeFailed _msock name) = "UpgradeFailed " ++ name
  show Done = "Done"

instance Read a => Read (Msg a) where
  readPrec = choice [ itemP, upgradeP ]
    where
      itemP = do
        Ident "Item" <- lexP
        Item Nothing <$> readPrec

      upgradeP = do
        Ident "Upgrade" <- lexP
        Upgrade Nothing <$> readPrec <*> readPrec <*> readPrec
          <*> readPrec <*> readPrec <*> parens readPrec <*> readPrec

messageSocket :: Msg a -> Maybe Socket
messageSocket (Item msock _) = msock
messageSocket (Upgrade msock _ _ _ _ _ _ _) = msock
messageSocket (UpgradeSucceeded msock _) = msock
messageSocket (UpgradeFailed msock _) = msock
messageSocket _ = error "messageSocket"

setMessageSocket :: Msg a -> Socket -> Msg a
setMessageSocket (Item _msock x) sock = Item (Just sock) x
setMessageSocket (Upgrade _msock name s s' a b f g) sock = Upgrade (Just sock) name s s' a b f g
setMessageSocket _ _ = error "setMessageSocket"

------------------------------------------------------------------------

-- import Text.Parsec as Parsec
-- import Text.Parsec.ByteString
--
-- parseMessage :: Parser a -> Parser (Msg a)
-- parseMessage p = Parsec.choice
--   [ pItem
--   , pUpgrade
--   ]
--   where
--     pItem = do
--       _ <- string "Item"
--       spaces
--       msg <- p
--       return (Item msg)
--
--     pUpgrade = do
--       error "we need an untyped Upgrade_ here"
--
-- runParseMessage :: Parser a -> ByteString -> Either ParseError (Msg a)
-- runParseMessage p = runParser (parseMessage p) () "message"
