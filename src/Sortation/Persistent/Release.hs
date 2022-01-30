module Sortation.Persistent.Release where

import Data.Maybe
import Data.Text (Text)
import Database.Persist
import Database.Persist.TH
import Optics
import Sortation.Persistent.Quasi
import Sortation.Persistent.Game (GameId)

persist "Release" [persistLowerCase|
  Release
    parent GameId
    region (Maybe Text)
    language (Maybe Text)
    version (Maybe Text)
    date (Maybe Text)
|]

releaseString :: Release -> Text
releaseString release =
  mconcat $ catMaybes
    [ release ^. #region
    , release ^. #language
    , release ^. #version
    , release ^. #date
    ]
