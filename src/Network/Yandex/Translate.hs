{-# LANGUAGE OverloadedStrings #-}
module Network.Yandex.Translate (
    APIKey,
    Language,
    Direction,
    directions,
    fromJSONMaybe
) where
import Prelude hiding (drop)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Text
import Data.Monoid
import Network.Wreq


type APIKey = Text
type Language = Text
newtype Direction = Direction (Language, Language)
    deriving(Eq)


instance Show Direction where
    show (Direction (f, t)) = show $ f <> "-" <> t


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


fromJSONMaybe :: FromJSON a => Value -> Maybe a
fromJSONMaybe v = case fromJSON v of
    Error   _ -> Nothing
    Success a -> Just a


directions :: APIKey -> IO (Maybe [Direction])
directions ykey = do
    r <- getWith opts getLangsUrl
    return $ r ^? responseBody .key "dirs" .to fromJSONMaybe ._Just
  where
    opts = optsWithKey ykey
