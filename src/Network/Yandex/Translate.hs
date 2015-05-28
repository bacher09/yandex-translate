{-# LANGUAGE OverloadedStrings #-}
module Network.Yandex.Translate (
    APIKey,
    Language,
    Direction,
    directions
) where
import Prelude hiding (drop)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key, _JSON)
import Data.Text
import Data.Monoid
import Network.Wreq
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Data.HashMap.Strict
import Control.Applicative


type APIKey = Text
type Language = Text
type LanguagesDescr = HashMap Text Text
newtype Direction = Direction (Language, Language)
    deriving(Eq)


instance Show Direction where
    show (Direction (f, t)) = unpack f ++ "-" ++ unpack t


instance ToJSON Direction where
    toJSON = toJSON . show


instance FromJSON Direction where
    parseJSON = withText "Direction" $ \s ->
        case breakOn "-" s of
            (f, t)
                | "-" `isPrefixOf` t -> return $ Direction (f, drop 1 t)
                | otherwise -> fail "Can't parse language direction"


baseUrl :: String
baseUrl = "https://translate.yandex.net/api/v1.5/tr.json/"


getLangsUrl, detectUrl, translateUrl :: String
getLangsUrl = baseUrl ++ "getLangs"
detectUrl = baseUrl ++ "detect"
translateUrl = baseUrl ++ "translate"


optsWithKey :: APIKey -> Options
optsWithKey key = defaults & param "key" .~ [key]


directions :: APIKey -> Maybe Language -> IO (Maybe [Direction], Maybe LanguagesDescr)
directions ykey lang = do
    r <- asValue =<< getWith opts getLangsUrl
    return $ (^? key "dirs" ._JSON) &&& (^? key "langs" ._JSON) $ r ^. responseBody
  where
    sopts = optsWithKey ykey
    opts = fromMaybe sopts $ (\l -> sopts & param "ui" .~ [l]) <$> lang
