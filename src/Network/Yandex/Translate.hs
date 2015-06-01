module Network.Yandex.Translate (
    -- types
    APIKey,
    Language,
    LanguagesDescr,
    Direction(..),
    YandexApiT,
    YandexApiConfig(..),
    TranslateParams(..),
    -- api funcs
    directions,
    detect,
    translate,
    -- lens
    apikey,
    httpOptions,
    format,
    options,
    _config,
    _session,
    -- other functions
    configureApi,
    runYandexApiT,
    runYandexApi,
    runYandexApiSession
) where
import Control.Lens
import Data.Aeson.Lens
import Data.Text
import Network.Wreq hiding (options)
import qualified Network.Wreq.Session as S
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class
import Network.Yandex.Translate.Types
import Network.Yandex.Translate.Internal


baseUrl :: String
baseUrl = "https://translate.yandex.net/api/v1.5/tr.json/"


getLangsUrl, detectUrl, translateUrl :: String
getLangsUrl = baseUrl ++ "getLangs"
detectUrl = baseUrl ++ "detect"
translateUrl = baseUrl ++ "translate"


directions :: (MonadIO m, MonadThrow m) => Maybe Language -> YandexApiT m ([Direction], Maybe LanguagesDescr)
directions lang = do
    opts <- getOpts
    sess <- view _session
    r <- liftIO $ asValue =<< S.getWith opts sess getLangsUrl
    let (dm, l) = (^? key "dirs" ._JSON) &&& (^? key "langs" ._JSON) $ r ^. responseBody
    d <- maybe (throwM $ JSONError "no dirs key in json") return dm
    return (d, l)
  where
    getOpts = do
        opts <- baseOptions
        return $ fromMaybe opts $ (\l -> opts & param "ui" .~ [l]) <$> lang


detect :: (MonadIO m, MonadThrow m) => Text -> YandexApiT m Language
detect text = do
    opts <- getOpts
    sess <- view _session
    r <- liftIO $ asValue =<< S.postWith opts sess detectUrl ["text" := text]
    let mlang = r ^? responseBody .key "lang" ._String
    maybe (throwM $ JSONError "Error no lang key in json") return mlang
  where
    getOpts = baseOptions


translate :: (MonadIO m, MonadThrow m) => Maybe Language -> Language -> TranslateParams -> [Text] -> YandexApiT m ([Text], Direction, Maybe Text)
translate f t params texts = do
    opts <- getOpts
    sess <- view _session
    r <- liftIO $ asValue =<< S.postWith opts sess translateUrl postParams
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
        dopts <- baseOptions
        let bopts = dopts & param "lang" .~ [tdir] &
                    param "format" .~ [params ^.format .to (pack . show)]
        return $ if topts & isn't _Empty
            then bopts & param "options" .~ (topts <&> pack . show)
            else bopts
