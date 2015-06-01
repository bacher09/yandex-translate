module Network.Yandex.DictionarySpec (
    spec
) where
import Test.Hspec
import System.Environment
import Network.Yandex.Translate
import Network.Yandex.Dictionary
import Data.Text


spec :: Spec
spec = do
    -- raise exception if var not set
    key <- runIO $ pack `fmap` getEnv "YANDEX_APIKEY_DICT"
    let conf = configureApi key

    describe "getLangs" $ do
        it "should contains en-ru and ru-en" $ do
            dirs <- runApi conf getLangs
            dirs `shouldContain` [Direction ("en", "ru")]
            dirs `shouldContain` [Direction ("ru", "en")]
  where
    runApi = runYandexApiSession
