{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Xmobar.Plugins.Calendar where

import qualified Control.Exception as X
import           Data.Default (def)
import qualified Data.Map.Strict as Map
import           System.Process (proc, env, readCreateProcessWithExitCode, callCommand)
import           Text.ICalendar
import           Prelude (read)
import           Relude
import           Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import           Data.Time.LocalTime (TimeZone, ZonedTime(..), zonedTimeToUTC, getCurrentTimeZone, utcToZonedTime)
import           Xmobar (Exec, run, rate)

type FileName = String
type Rate     = Int

data Calendar = Calendar FileName Rate
    deriving (Show)

instance Exec Calendar where
    run (Calendar filename rate') = calendar filename rate'
    rate (Calendar _ rate')   = rate'

calendar :: FileName -> Rate -> IO String
calendar filename rate' =
    parseICalendarFile (def::DecodingFunctions) filename >>= \case
        Left err       -> pure err
        Right (iCal,_) -> latestEvent iCal rate'

latestEvent :: [VCalendar] -> Rate -> IO String
latestEvent iCal rate' = do
    eventTimes <- sort <$> extractEvents iCal
    currentTime <- getCurrentTime
    currentZone <- getCurrentTimeZone
    case getNextEvent currentTime eventTimes of
        Just nextEvent -> do
            beepIfRipe nextEvent currentTime
            return $ show $ utcToZonedTime currentZone nextEvent
        Nothing        -> return "No events found"
  where
    beepIfRipe :: UTCTime -> UTCTime -> IO ()
    beepIfRipe nextTime currentTime =
        if (realToFrac (nextTime `diffUTCTime` currentTime) ::Double) <= fromIntegral rate'
        then callCommand "beep"
        else return ()

getNextEvent :: Ord a => a -> [a] -> Maybe a
getNextEvent _ [] = Nothing
getNextEvent x (y:ys)
    | x > y = getNextEvent x ys
    | otherwise = Just y

extractEvents :: [VCalendar] -> IO [UTCTime]
extractEvents iCal = do
    let startTimes = veDTStart <$> Map.elems (Map.unions (vcEvents <$> iCal))
        dateTimes = map dtStartDateTimeValue (catMaybes startTimes)
    maybeZonedTimes <- sequence $ dateTimeToZonedTime <$> dateTimes
    return $ catMaybes $ zonedTimeToUTC <<$>> maybeZonedTimes
  where
    dateTimeToZonedTime :: DateTime -> IO (Maybe ZonedTime)
    dateTimeToZonedTime (ZonedDateTime localTime timeZone) =
        toTimeZone (toString timeZone) >>= \case
            Just timeZone' -> pure $ Just (ZonedTime localTime timeZone')
            Nothing        -> pure Nothing
    dateTimeToZonedTime _ = pure Nothing

toTimeZone :: String -> IO (Maybe TimeZone)
toTimeZone location = do
    let createProcess = (proc "date" ["+\"%z\""]) { env = Just [("TZ",location)] }
        runProcess = Right <$> readCreateProcessWithExitCode createProcess ""
    X.catch runProcess (pure . Left @X.SomeException) >>= \case
        Left _              -> pure Nothing
        Right (_,std_out,_) -> pure $ Just (read @TimeZone (take 5 $ drop 1 std_out))
