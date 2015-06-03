module Network.Yandex.Dictionary (
    -- types
    DictFlags(..),
    DictionaryParams(..),
    DictAttrs(..),
    DictResult,
    -- api functions
    getLangs,
    dictLookup,
    -- lens
    uiLang,
    flags,
    text,
    number,
    partOfSpeach,
    generation,
    transcription,
    translates,
    synonyms,
    mean,
    examples
) where
import Control.Lens
import Data.Aeson.Lens
import Network.Wreq hiding (options)
import qualified Network.Wreq.Session as S
import Network.Yandex.Translate.Types
import Network.Yandex.Translate.Internal
import Network.Yandex.Dictionary.Types
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Maybe (fromMaybe)
import Data.Text (Text)


baseUrl, getLangsUrl, lookupUrl :: String
baseUrl = "https://dictionary.yandex.net/api/v1/dicservice.json/"
getLangsUrl = baseUrl ++ "getLangs"
lookupUrl = baseUrl ++ "lookup"


getLangs :: (MonadIO m, MonadThrow m) => YandexApiT m [Direction]
getLangs = do
    opts <- baseOptions
    sess <- view _session
    r <- liftIO $ asJSON =<< S.getWith opts sess getLangsUrl
    return $ r ^. responseBody


dictLookup :: (MonadIO m, MonadThrow m) => Direction -> DictionaryParams -> Text -> YandexApiT m DictResult
dictLookup dir params text = do
    opts <- getOpts
    sess <- view _session
    r <- liftIO $ asValue =<< S.getWith opts sess lookupUrl
    let mres = r ^? responseBody .key "def" ._JSON
    maybe (throwM $ JSONError "no def key in json") return mres
  where
    getOpts = do
        bopts <- baseOptions
        let opts = bopts & param "lang" .~ [directionAsText dir]
        let opts' = fromMaybe opts $ params ^. uiLang <&> \v -> opts & param "ui" .~ [v]
        let opts'' = if params ^. flags & isn't _Empty
            then opts' & param "flags" .~ [params ^. flags .to flagsText]
            else opts'

        return $ opts'' & param "text" .~ [text]
