module Network.Yandex.TranslateSpec (
    spec
) where
import Test.Hspec
import System.Environment
import Data.Text
import Control.Applicative
import Network.Yandex.Translate
import Data.Default.Class
import Data.Maybe
import Control.Monad.IO.Class


spec :: Spec
spec = do
    -- raise exception if var not set
    key <- runIO $ pack <$> getEnv "YANDEX_APIKEY_TRANSLATE"
    let conf = configureApi key

    describe "directions" $ do
        it "should contains en-ru, ru-en directions" $ do
            (dirs, ui ) <- runApi conf $ directions Nothing
            ui `shouldBe` Nothing
            dirs `shouldContain` [Direction ("en", "ru")]
            dirs `shouldContain` [Direction ("ru", "en")]


        it "check directions for english ui" $ do
            (_, ui) <- runApi conf $ directions (Just "en")
            isJust ui `shouldBe` True

    describe "detect" $ do
        it "should detect english for word Hello" $ do
            lang <- runApi conf $ detect "Hello"
            lang `shouldBe` "en"

    describe "translate" $ do
        it "Hello should translate to russian" $ runApi conf $ do
            (msgs, dir, detect)  <- translate (Just "en") "ru" def ["hello"]
            liftIO $ msgs `shouldBe` ["привет"]
            liftIO $ detect `shouldBe` Nothing
            liftIO $ dir `shouldBe` Direction ("en", "ru")
  where
    runApi = runYandexApiSession
