{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Postgresql.Orphans () where

import           Control.Monad (void)
import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Char8 (ByteString)
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.Time (NominalDiffTime)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Monoid
import           Data.List

import Database.Persist
import Database.Persist.Sql

instance PersistFieldSql NominalDiffTime where
  sqlType = const $ SqlOther "interval"

instance PersistField NominalDiffTime where
  toPersistValue = PersistDbSpecific . fromNominalDiffTime
  fromPersistValue (PersistDbSpecific bs)= first T.pack (toNominalDiffTime bs)


fromNominalDiffTime :: NominalDiffTime -> ByteString
fromNominalDiffTime ndt =
  let start = truncate ndt
      millis = B8.pack . init . show $ 1000 * (ndt - fromIntegral start)
      vals  = map fst $ tail $ scanl (quotRem . snd) (0,start) [truncate $ toSecond u 1 | u <- init units]
   in B8.unwords $ zipWith (\v u-> B8.pack (show v) <> " " <> u) vals units
          <> [millis, "milliseconds"]

units :: [ByteString]
units = ["years", "months", "weeks", "days", "hours", "minutes", "seconds", "milliseconds"]

toNominalDiffTime :: ByteString -> Either String NominalDiffTime
toNominalDiffTime = parseOnly ndt
  where ndt :: Parser NominalDiffTime
        ndt = sum <$> between '[' ']' (choice (map parser units) `sepBy` (skipMany space *> comma *> skipMany space))
        comma :: Parser Char
        comma = char ','
        parser :: ByteString -> Parser NominalDiffTime
        parser s = do void $ between '"' '"' $ string s
                      skipMany space
                      void $ char ':'
                      skipMany space
                      toSecond s <$> signed rational

        between :: Char -> Char -> Parser a -> Parser a
        between l r p = char l *> skipMany space *> p <* skipMany space <* char r

toSecond :: Fractional a => ByteString -> a -> a
toSecond "milliseconds" x = x / 1000
toSecond "seconds"      x = x
toSecond "minutes"      x = x*60
toSecond "hours"        x = x*60*60
toSecond "days"         x = x*60*60*24
toSecond "weeks"        x = x*60*60*24*7
toSecond "months"       x = x*60*60*24*30
toSecond "years"        x = x*60*60*24*365
