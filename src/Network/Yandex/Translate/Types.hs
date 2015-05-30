{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Network.Yandex.Translate.Types (
    -- types
    APIKey,
    Language,
    LanguagesDescr,
    Direction(..),
    YandexApiT,
    YandexApiConfig(..),
    TranslateParams(..),
    -- lens
    apikey,
    format,
    options,
    -- funcs
    formatDirection,
    runYandexApiT,
    runYandexApi
) where
import Prelude hiding (drop)
import Data.Default.Class
import Control.Monad.Trans.Reader
import Control.Lens.TH
import Data.Aeson
import Data.Monoid
import Data.HashMap.Strict
import Data.Text
import Control.Monad.IO.Class


type APIKey = Text
type Language = Text
type LanguagesDescr = HashMap Text Text
newtype Direction = Direction (Language, Language)
    deriving(Eq)

type YandexApiT m a = ReaderT YandexApiConfig m a


data YandexApiConfig = YandexApiConfig {
    _apikey :: APIKey
} deriving(Show, Eq)

data TranslateOptions = DetectLanguage
    deriving(Eq, Ord, Bounded, Enum)

data Format = Plain
            | HTML
    deriving(Eq, Ord, Bounded, Enum)


data TranslateParams = TranslateParams {
    _format  :: Format,
    _options :: [TranslateOptions]
} deriving(Show)


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


instance Show Format where
    show Plain = "plain"
    show HTML =  "html"


instance Show TranslateOptions where
    show DetectLanguage = "1"


makeLenses ''TranslateParams
makeLenses ''YandexApiConfig


defaultParams :: TranslateParams
defaultParams = TranslateParams Plain []


instance Default TranslateParams where
    def = defaultParams


formatDirection :: Maybe Language -> Language -> Text
formatDirection (Just f) l =  f <> "-" <> l
formatDirection Nothing t = t


runYandexApiT :: (MonadIO m) => YandexApiConfig -> YandexApiT m a -> m a
runYandexApiT conf act = runReaderT act conf

runYandexApi :: (MonadIO m) => YandexApiConfig -> YandexApiT IO a -> m a
runYandexApi conf act = liftIO $ runYandexApiT conf act
