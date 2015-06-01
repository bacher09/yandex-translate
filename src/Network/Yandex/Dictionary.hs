module Network.Yandex.Dictionary (
    getLangs
) where
import Control.Lens
import Network.Wreq hiding (options)
import qualified Network.Wreq.Session as S
import Network.Yandex.Translate.Types
import Network.Yandex.Translate.Internal
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow(throwM))


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
