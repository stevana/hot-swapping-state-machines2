{-# LANGUAGE GADTs #-}

module Message where

import Data.Typeable
import Data.ByteString (ByteString)
import Text.Read
import Network.Socket (Socket)

import Syntax.Pipeline.Typed
import Syntax.Pipeline.Untyped
import Syntax.StateMachine.Typed
import Syntax.Types

------------------------------------------------------------------------

data Msg a where
  Item    :: Maybe Socket -> a -> Msg a
  Upgrade :: (Typeable s, Typeable s', Typeable i, Typeable o) => Maybe Socket -> Name -> T s' i o -> Maybe (s -> s') -> Msg a
  UpgradeSucceeded :: Name -> Msg a
  UpgradeFailed :: Name -> Msg a

  Done :: Msg a

instance Show a => Show (Msg a) where
  show (Item _msocket x) = "Item " ++ show x
  show (Upgrade _msocket name _sm _g) = "Upgrade " ++ name
  show (UpgradeSucceeded name) = "UpgradeSucceeded " ++ name
  show (UpgradeFailed name) = "UpgradeFailed " ++ name
  show Done = "Done"

instance Read a => Read (Msg a) where
  readPrec = choice [ itemP ]
    where
      itemP = do
        Ident "Item" <- lexP
        Item Nothing <$> readPrec

messageSocket :: Msg a -> Maybe Socket
messageSocket (Item msock _) = msock
messageSocket (Upgrade msock _ _ _) = msock
messageSocket _ = error "messageSocket"

setMessageSocket :: Msg a -> Socket -> Msg a
setMessageSocket (Item _msock x) sock = Item (Just sock) x
setMessageSocket (Upgrade _msock name f g) sock = Upgrade (Just sock) name f g
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
