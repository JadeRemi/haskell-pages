{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

type Event = (Day, T.Text)

defaultDate :: Day
defaultDate = fromGregorian 2023 4 1 -- Starting date set to April 1st, 2023

defaultEvents :: [Event]
defaultEvents = [(fromGregorian 2023 4 3, "Birthday party")]

formatDate :: Day -> T.Text
formatDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

parseDate :: T.Text -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

main :: IO ()
main = mainWidgetWithCss css $ do
    el "h1" $ text "Event Scheduler and Calendar"
    dateDyn <- holdDyn defaultDate =<< fmap (fromMaybe defaultDate . parseDate) <$> valueChange <$> inputElement def
    eventsDyn <- foldDyn (:) defaultEvents =<< fmap (fmap (flip (,) "") . fromMaybe defaultDate . parseDate) <$> valueChange <$> inputElement def
    el "h2" $ text "Event Scheduler"
    renderEventScheduler dateDyn eventsDyn
    el "h2" $ text "Calendar"
    renderDatePicker dateDyn
    renderCalendar dateDyn eventsDyn

css :: T.Text
css = "table { border-collapse: collapse; } td { border: 1px solid black; padding: 5px; }"
      <> "li { margin: 5px; } input { margin: 5px; }"