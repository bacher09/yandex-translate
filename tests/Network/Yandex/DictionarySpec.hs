module Network.Yandex.DictionarySpec (
    spec
) where
import Test.Hspec
import System.Environment
import Network.Yandex.Translate
import Network.Yandex.Dictionary
import Data.Default.Class
import Data.Text
import Control.Lens


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

    describe "dictLookup" $ do
        it "should translate right" $ do
            dres <- runApi conf $ dictLookup (Direction ("en", "ru")) def "hello"
            let trans = dres ^.. traverse.text
            trans `shouldContain` ["hello"]
  where
    runApi = runYandexApiSession
