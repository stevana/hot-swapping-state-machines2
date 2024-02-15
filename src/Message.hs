{-# LANGUAGE PatternSynonyms #-}

module Message where

import GHC.Read
import Text.Read
import Network.Socket (Socket)

import Syntax.Pipeline.Typed
import Upgrade

------------------------------------------------------------------------

newtype MaybeSocket = MaybeSocket (Maybe Socket)
  deriving Show

instance Read MaybeSocket where
  readPrec = parens $ do
    Ident "MaybeSocket" <- lexP
    Ident "Nothing" <- lexP
    return (MaybeSocket Nothing)

data Msg a
  = Item MaybeSocket a
  | Upgrade MaybeSocket Name UpgradeData_
  | UpgradeSucceeded MaybeSocket Name
  | UpgradeFailed MaybeSocket Name
  | Done
  deriving (Show, Read)

pattern Item_ :: a -> Msg a
pattern Item_ x = Item (MaybeSocket Nothing) x

pattern Upgrade_ :: Name -> UpgradeData_ -> Msg a
pattern Upgrade_ n ud = Upgrade (MaybeSocket Nothing) n ud

messageSocket :: Msg a -> Maybe Socket
messageSocket (Item (MaybeSocket msock) _) = msock
messageSocket (Upgrade (MaybeSocket msock) _ _) = msock
messageSocket (UpgradeSucceeded (MaybeSocket msock) _) = msock
messageSocket (UpgradeFailed (MaybeSocket msock) _) = msock
messageSocket _ = error "messageSocket"

setMessageSocket :: Msg a -> Socket -> Msg a
setMessageSocket (Item _msock x) sock = Item (MaybeSocket (Just sock)) x
setMessageSocket (Upgrade _msock name ud) sock = Upgrade (MaybeSocket (Just sock)) name ud
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
