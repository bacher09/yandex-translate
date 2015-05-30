{-# LANGUAGE OverloadedStrings #-}
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
    -- other functions
    runYandexApiT,
    runYandexApi
) where
import Control.Lens
import Data.Aeson.Lens
import Data.Text
import Network.Wreq hiding (options)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class
import Network.Yandex.Translate.Types


baseUrl :: String
baseUrl = "https://translate.yandex.net/api/v1.5/tr.json/"


getLangsUrl, detectUrl, translateUrl :: String
getLangsUrl = baseUrl ++ "getLangs"
detectUrl = baseUrl ++ "detect"
translateUrl = baseUrl ++ "translate"


optsWithKey :: APIKey -> Options
optsWithKey key = defaults & param "key" .~ [key]


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
