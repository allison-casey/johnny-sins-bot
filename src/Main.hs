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

loopingMain :: (DSC.RestChan, DSC.Gateway, z) -> IO ()
loopingMain dis = do
  e <- DSC.nextEvent dis
  case e of
    Left  er                    -> putStrLn ("Event error: " <> show er)
    Right (DSC.MessageCreate m) -> do
      when (isPing (DSC.messageText m)) $ do
        resp <- DSC.restCall
          dis
          (DSC.CreateMessage (DSC.messageChannel m) "Pong!")
        print resp
        putStrLn ""
      loopingMain dis
    _ -> loopingMain dis

fromBot :: DSC.Message -> Bool
fromBot m = case DSC.messageAuthor m of
  Right u          -> DSC.userIsBot u
  Left  _webhookid -> True

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
