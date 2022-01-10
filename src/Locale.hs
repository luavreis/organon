{-# LANGUAGE DeriveGeneric #-}
module Locale where

import Data.Time

data Locale
  = Portuguese
  | English
  deriving (Show, Enum, Bounded, Eq, Ord, Generic)

localeAbbrev :: Locale -> String
localeAbbrev Portuguese = "pt"
localeAbbrev English = "en"

defLocale :: Locale
defLocale = Portuguese

toLocale :: String -> Maybe Locale
toLocale "pt" = Just Portuguese
toLocale "en" = Just English
toLocale _ = Nothing

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

type Localized a = Map Locale a

ptTimeLocale :: TimeLocale
ptTimeLocale =
    TimeLocale
        { wDays =
            [ ("domingo", "dom")
            , ("segunda", "seg")
            , ("terça", "ter")
            , ("quarta", "qua")
            , ("quinta", "qui")
            , ("sexta", "sex")
            , ("sábado", "sab")
            ]
        , months =
            [ ("janeiro", "jan")
            , ("fevereiro", "fev")
            , ("março", "mar")
            , ("abril", "abr")
            , ("maio", "mai")
            , ("junho", "jun")
            , ("julho", "jul")
            , ("agosto", "ago")
            , ("setembro", "set")
            , ("outubro", "out")
            , ("novembro", "nov")
            , ("dezembro", "dez")
            ]
        , amPm = ("AM", "PM")
        , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
        , dateFmt = "%d/%m/%y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones =
            [ TimeZone 0 False "UT"
            , TimeZone 0 False "GMT"
            , TimeZone (-3 * 60) False "BRA"
            ]
        }
