{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception              ( finally )
import           Control.Monad                  ( when )
import           Data.Char                      ( toLower )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Discord                       as DSC
import           Data.Either

main :: IO ()
main = do
  token   <- T.strip <$> TIO.readFile "./auth-token.secret"
  discord <- DSC.loginRestGateway (DSC.Auth token)
  finally (loopingMain discord) (DSC.stopDiscord discord)

keywords :: [T.Text]
keywords = ["johnny", "sins"]

loopingMain :: (DSC.RestChan, DSC.Gateway, z) -> IO ()
loopingMain dis = do
  e <- DSC.nextEvent dis
  case e of
    Left  er                    -> putStrLn ("Event error: " <> show er)
    Right (DSC.MessageCreate m) -> do
      when (isKeyword (DSC.messageText m) && not (fromBot m)) $ do
        let message   = DSC.messageText m
        let channelID = DSC.messageChannel m
        let response  = generateResponse message
        _ <- sendChannelMessage channelID $ generateResponse message
        return ()
      loopingMain dis
    _ -> loopingMain dis
 where
  sendChannelMessage channelID message =
    DSC.restCall dis (DSC.CreateMessage channelID message)

fromBot :: DSC.Message -> Bool
fromBot m = case DSC.messageAuthor m of
  Right u          -> DSC.userIsBot u
  Left  _webhookid -> True

isKeyword :: T.Text -> Bool
isKeyword message = any (`caseInsensitiveIsInfix` message) keywords
  where caseInsensitiveIsInfix key str = T.isInfixOf key $ T.toLower str

generateResponse :: T.Text -> T.Text
generateResponse message = ":poop:"
