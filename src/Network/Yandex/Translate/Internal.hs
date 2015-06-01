module Network.Yandex.Translate.Internal (
    baseOptions
) where
import Control.Lens
import Network.Wreq hiding (options)
import Control.Monad.IO.Class
import Network.Yandex.Translate.Types


baseOptions :: (MonadIO m) => YandexApiT m Options
baseOptions = do
    ykey <- view $ _config.apikey
    opt <- view $ _config.httpOptions
    return $ opt & param "key" .~ [ykey]
