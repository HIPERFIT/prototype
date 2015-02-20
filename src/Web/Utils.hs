module Utils where

import Contract.Date

import System.Locale (defaultTimeLocale)
import Data.Time (Day)
import Data.Time.Format (readTime, formatTime)
import Data.List


-- remove spaces from both ends of string
trim = unwords . words

-- This looks ugly. We should get rid of Date and use Day instead.
-- Converting date defined in "contract" project to regular Haskell's Day.
contrDate2Day :: Date -> Day
contrDate2Day  = parseDate . ppDate
day2ContrDate :: Day -> Date
day2ContrDate = read . formatDate

parseDate = readTime defaultTimeLocale dateFormat
formatDate = formatTime defaultTimeLocale dateFormat

dateFormat = "%Y-%m-%d"
