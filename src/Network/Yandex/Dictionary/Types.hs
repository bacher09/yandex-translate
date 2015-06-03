{-# LANGUAGE TemplateHaskell #-}
module Network.Yandex.Dictionary.Types (
    -- types
    DictFlags(..),
    DictionaryParams(..),
    DictAttrs(..),
    DictResult,
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
    examples,
    -- other
    flagsInt,
    flagsText
) where
import Control.Lens hiding ((.=))
import Data.Bits (setBit)
import Data.Default.Class
import Data.Text (pack, Text)
import Data.Aeson
import Network.Yandex.Translate.Types


data DictFlags = Family
               | ShortPos
               | Morpho
               | PosFilter
    deriving(Eq, Ord, Bounded, Enum, Show)


data DictionaryParams = DictionaryParams {
    _uiLang :: Maybe Language,
    _flags  :: [DictFlags]
} deriving (Eq, Show)


data DictAttrs = DictAttrs {
    _text          :: Text,
    _number        :: Maybe Text,
    _partOfSpeach  :: Maybe Text,
    _generation    :: Maybe Text,
    _transcription :: Maybe Text,
    _translates    :: [DictAttrs],
    _synonyms      :: [DictAttrs],
    _mean          :: [DictAttrs],
    _examples      :: [DictAttrs]
} deriving(Eq, Show, Read);


type DictResult = [DictAttrs]


$(makeLenses ''DictionaryParams)
$(makeLenses ''DictAttrs)


defaultParams :: DictionaryParams
defaultParams = DictionaryParams {_uiLang=Nothing, _flags=[]}


instance Default DictionaryParams where
    def = defaultParams


instance FromJSON DictAttrs where
    parseJSON = withObject "DictAttrs" $ \o -> do
        text <- o .: "text"
        num <- o .:? "num"
        pos <- o .:? "pos"
        gen <- o .:? "gen"
        ts <- o .:? "ts"
        tr <- parseMList =<< o .:? "tr"
        syn <- parseMList =<< o .:? "syn"
        mean <- parseMList =<< o .:? "mean"
        examples <- parseMList =<< o .:? "ex"
        return $ DictAttrs {
            _text=text,
            _number=num,
            _partOfSpeach=pos,
            _generation=gen,
            _transcription=ts,
            _translates=tr,
            _synonyms=syn,
            _mean=mean,
            _examples=examples}
      where
        parseMList = maybe (return []) parseJSON


instance ToJSON DictAttrs where
    toJSON attrs = object params
      where
        maybeParam name = maybe [] (\v -> [name .= v])
        maybeList key sel = if isn't _Empty (attrs ^. sel)
            then [key .= (toJSON $ attrs ^. sel)]
            else []
        params = ["text" .= view text attrs] ++ 
                 maybeParam "num" (attrs ^. number) ++
                 maybeParam "pos" (attrs ^. partOfSpeach) ++
                 maybeParam "gen" (attrs ^. generation) ++
                 maybeParam "ts" (attrs ^. transcription) ++
                 maybeList "tr" translates ++
                 maybeList "sym" synonyms ++
                 maybeList "mean" mean ++
                 maybeList "ex" examples


flagsInt :: [DictFlags] -> Int
flagsInt fs = foldl setBit 0 ds
  where
    ds = map fromEnum fs


flagsText :: [DictFlags] -> Text
flagsText = pack . show . flagsInt
