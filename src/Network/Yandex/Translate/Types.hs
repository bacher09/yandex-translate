{-# LANGUAGE TemplateHaskell #-}
module Network.Yandex.Translate.Types (
    -- types
    APIKey,
    Language,
    LanguagesDescr,
    Direction(..),
    YandexApiT,
    ApiInfo,
    YandexApiConfig(..),
    TranslateParams(..),
    -- lens
    apikey,
    httpOptions,
    format,
    options,
    _config,
    _session,
    -- funcs
    formatDirection,
    directionAsText,
    configureApi,
    runYandexApiT,
    runYandexApi,
    runYandexApiSession
) where
import Prelude hiding (drop)
import Data.Default.Class
import Control.Monad.Trans.Reader
import Control.Lens
import Data.Aeson hiding (Options)
import Data.Monoid
import Data.HashMap.Strict
import Data.Text
import Control.Monad.IO.Class
import Network.Wreq hiding (options)
import Network.Wreq.Session (Session, withAPISession)


type APIKey = Text
type Language = Text
type LanguagesDescr = HashMap Text Text
newtype Direction = Direction (Language, Language)
    deriving(Eq)

type ApiInfo = (YandexApiConfig, Session)
type YandexApiT m a = ReaderT ApiInfo m a


data YandexApiConfig = YandexApiConfig {
    _apikey      :: APIKey,
    _httpOptions :: Options
} deriving(Show)


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

_config :: Lens' ApiInfo YandexApiConfig
_config  = _1

_session :: Lens' ApiInfo Session
_session = _2


defaultParams :: TranslateParams
defaultParams = TranslateParams Plain []


instance Default TranslateParams where
    def = defaultParams


formatDirection :: Maybe Language -> Language -> Text
formatDirection (Just f) l =  f <> "-" <> l
formatDirection Nothing t = t


directionAsText :: Direction -> Text
directionAsText (Direction (f, l)) = f <> "-" <> l


configureApi :: APIKey -> YandexApiConfig
configureApi key = YandexApiConfig {_apikey=key, _httpOptions=opts}
  where
    opts = defaults & redirects .~ 0


runYandexApiT :: (MonadIO m) => Session -> YandexApiConfig -> YandexApiT m a -> m a
runYandexApiT sess conf act = runReaderT act (conf, sess)


runYandexApi :: (MonadIO m) => Session -> YandexApiConfig -> YandexApiT IO a -> m a
runYandexApi sess conf act = liftIO $ runYandexApiT sess conf act


runYandexApiSession :: YandexApiConfig -> YandexApiT IO a -> IO a
runYandexApiSession conf act = withAPISession (\sess -> runYandexApi sess conf act)
