{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Network.Yandex.Translate (
    APIKey,
    Language,
    Direction(..),
    LanguagesDescr,
    TranslateOptions(..),
    Format(..),
    TranslateParams(..),
    defaultParams,
    directions,
    detect,
    translate,
    runYandexApiT,
    runYandexApi,
    YandexApiConfig(..),
    YandexApiT,
    apikey
) where
import Prelude hiding (drop)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Text
import Data.Monoid
import Network.Wreq hiding (options)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Data.HashMap.Strict
import Control.Applicative
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Default.Class
import Control.Monad.Trans.Reader
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


baseUrl :: String
baseUrl = "https://translate.yandex.net/api/v1.5/tr.json/"


getLangsUrl, detectUrl, translateUrl :: String
getLangsUrl = baseUrl ++ "getLangs"
detectUrl = baseUrl ++ "detect"
translateUrl = baseUrl ++ "translate"


optsWithKey :: APIKey -> Options
optsWithKey key = defaults & param "key" .~ [key]


formatDirection :: Maybe Language -> Language -> Text
formatDirection (Just f) l =  f <> "-" <> l
formatDirection Nothing t = t


directions :: (MonadIO m, MonadThrow m) => Maybe Language -> YandexApiT m ([Direction], Maybe LanguagesDescr)
directions lang = do
    opts <- getOpts
    r <- liftIO $ asValue =<< getWith opts getLangsUrl
    let (dm, l) = (^? key "dirs" ._JSON) &&& (^? key "langs" ._JSON) $ r ^. responseBody
    d <- maybe (throwM $ JSONError "no dirs key in json") return dm
    return (d, l)
  where
    getOpts = do
        ykey <- view apikey
        let sopts = optsWithKey ykey
            opts = fromMaybe sopts $ (\l -> sopts & param "ui" .~ [l]) <$> lang
        return opts


detect :: (MonadIO m, MonadThrow m) => Text -> YandexApiT m Language
detect text = do
    opts <- getOpts
    r <- liftIO $ asValue =<< postWith opts detectUrl ["text" := text]
    let mlang = r ^? responseBody .key "lang" ._String
    maybe (throwM $ JSONError "Error no lang key in json") return mlang
  where
    getOpts = do
        ykey <- view apikey
        return $ optsWithKey ykey


translate :: (MonadIO m, MonadThrow m) => Maybe Language -> Language -> TranslateParams -> [Text] -> YandexApiT m ([Text], Direction, Maybe Text)
translate f t params texts = do
    opts <- getOpts
    r <- liftIO $ asValue =<< postWith opts translateUrl postParams
    let mres_text = r ^? responseBody .key "text" ._JSON
        mres_lang = r ^? responseBody .key "lang" ._JSON
        mdetected = r ^? responseBody .key "detected" .key "lang" ._String

    res_text <- maybe (throwM $ JSONError "no text key in json") return mres_text
    res_lang <- maybe (throwM $ JSONError "no lang key in json") return mres_lang
    return (res_text, res_lang, mdetected)
  where
    tdir = formatDirection f t
    postParams = ("text" :=) <$> texts
    topts = params ^. options
    getOpts = do
        ykey <- view apikey
        let bopts = optsWithKey ykey & param "lang" .~ [tdir] &
                    param "format" .~ [params ^.format .to (pack . show)]
        return $ if topts & isn't _Empty
            then bopts & param "options" .~ (topts <&> pack . show)
            else bopts


runYandexApiT :: (MonadIO m) => YandexApiConfig -> YandexApiT m a -> m a
runYandexApiT conf act = runReaderT act conf

runYandexApi :: (MonadIO m) => YandexApiConfig -> YandexApiT IO a -> m a
runYandexApi conf act = liftIO $ runYandexApiT conf act
