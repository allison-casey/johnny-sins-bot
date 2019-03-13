{-# LANGUAGE DeriveGeneric, DeriveAnyClass  #-}
module Main where

import           Control.Exception              ( finally )
import           Control.Monad                  ( when )
import           Control.Monad
import           Data.Char                      ( toLower )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Discord                       as DSC
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as B
import           Data.Either
import           GHC.Generics
import           Data.Aeson
import           Data.List
import           Data.Maybe
import           System.Random                  ( randomRIO )

data BotTrigger = BotTrigger
  { keywords :: [T.Text]
  , responses :: [T.Text] } deriving (Show, Generic, ToJSON, FromJSON)

data BotConfig = BotConfig
  { title :: T.Text
  , description :: T.Text
  , triggers :: [BotTrigger] } deriving (Show, Generic, ToJSON, FromJSON)

-- parseConfig :: BC.ByteString -> BotConfig
-- parseConfig  = A.encode

main :: IO ()
main = do
  rawJSON <- B.readFile "./config.json"
  let config = decode rawJSON :: Maybe BotConfig
  case config of
    Nothing   -> putStrLn "[ERROR] Failed to parse config"
    Just conf -> do
      token   <- T.strip <$> TIO.readFile "./auth-token.secret"
      discord <- DSC.loginRestGateway (DSC.Auth token)
      finally (loopingMain conf discord) (DSC.stopDiscord discord)

testKeywords :: [T.Text]
testKeywords = ["johnny", "sins"]

loopingMain :: BotConfig -> (DSC.RestChan, DSC.Gateway, z) -> IO ()
loopingMain conf dis = do
  e <- DSC.nextEvent dis
  case e of
    Left  er                    -> putStrLn ("Event error: " <> show er)
    Right (DSC.MessageCreate m) -> do
      unless (fromBot m) $ do
        let message            = DSC.messageText m
        let channelID          = DSC.messageChannel m
        let responsibleTrigger = getResponsibleTrigger triggersList message
        case responsibleTrigger of
          Nothing      -> return ()
          Just trigger -> do
            response <- generateResponse trigger message
            sendChannelMessage channelID response
      loopingMain conf dis
    _ -> loopingMain conf dis
 where
  triggersList = triggers conf
  sendChannelMessage channelID message = do
    _ <- DSC.restCall dis (DSC.CreateMessage channelID message)
    return ()

fromBot :: DSC.Message -> Bool
fromBot m = case DSC.messageAuthor m of
  Right u          -> DSC.userIsBot u
  Left  _webhookid -> True

getResponsibleTrigger :: [BotTrigger] -> T.Text -> Maybe BotTrigger
getResponsibleTrigger triggers message = find checkTrigger triggers
 where
  checkTrigger trigger =
    any (`caseInsensitiveIsInfix` message) (keywords trigger)

caseInsensitiveIsInfix :: T.Text -> T.Text -> Bool
caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str

isKeyword :: [BotTrigger] -> T.Text -> Bool
isKeyword triggers message = any checkTriggerKeywords triggers
 where
  caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str
  checkTriggerKeywords trigger =
    any (`caseInsensitiveIsInfix` message) (keywords trigger)

generateResponse :: BotTrigger -> T.Text -> IO T.Text
generateResponse trigger message = (responsesList !!)
  <$> randomRIO (0, length responsesList - 1)
  where responsesList = responses trigger
