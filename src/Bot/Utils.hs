{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}
module Bot.Utils
  ( BotTrigger(..)
  , BotConfig(..)
  , fromBot
  , getResponsibleTrigger
  )
where

import qualified Data.Text                     as T
import qualified Discord                       as DSC
import           Data.List                      ( find )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , decode
                                                )

data BotTrigger = BotTrigger
  { keywords :: [T.Text]
  , probability :: Double
  , responses :: [T.Text] } deriving (Show, Generic, ToJSON, FromJSON)

data BotConfig = BotConfig
  { title :: T.Text
  , description :: T.Text
  , probability :: Double
  , triggers :: [BotTrigger] } deriving (Show, Generic, ToJSON, FromJSON)

-- | Checks if a message is from a bot
fromBot :: DSC.Message -> Bool
fromBot m = case DSC.messageAuthor m of
  Right u          -> DSC.userIsBot u
  Left  _webhookid -> True

-- | Finds the trigger responsible for handling the given message
getResponsibleTrigger :: [BotTrigger] -> T.Text -> Maybe BotTrigger
getResponsibleTrigger triggers message = find checkTrigger triggers
 where
  checkTrigger trigger =
    any (`caseInsensitiveIsInfix` message) (keywords trigger)

-- | A case insensitive version of isInfixOf
caseInsensitiveIsInfix :: T.Text -> T.Text -> Bool
caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str

-- | Checks if the message contains any of the key words defined in the
-- bot triggers list
isKeyword :: [BotTrigger] -> T.Text -> Bool
isKeyword triggers message = any checkTriggerKeywords triggers
 where
  caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str
  checkTriggerKeywords trigger =
    any (`caseInsensitiveIsInfix` message) (keywords trigger)
