module Network.Yandex.Dictionary (
    getLangs
) where
import Control.Lens
import Network.Wreq hiding (options)
import qualified Network.Wreq.Session as S
import Network.Yandex.Translate.Types
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow(throwM))


baseUrl, getLangsUrl, lookupUrl :: String
baseUrl = "https://dictionary.yandex.net/api/v1/dicservice.json/"
getLangsUrl = baseUrl ++ "getLangs"
lookupUrl = baseUrl ++ "lookup"


baseOptions :: (MonadIO m, MonadThrow m) => YandexApiT m Options
baseOptions = do
    ykey <- view $ _config.apikey
    opt <- view $ _config.httpOptions
    return $ opt & param "key" .~ [ykey]


getLangs :: (MonadIO m, MonadThrow m) => YandexApiT m [Direction]
getLangs = do
    opts <- baseOptions
    sess <- view _session
    r <- liftIO $ asJSON =<< S.getWith opts sess getLangsUrl
    return $ r ^. responseBody
