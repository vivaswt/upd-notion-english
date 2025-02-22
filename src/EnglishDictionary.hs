{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module EnglishDictionary
  ( WordInfo (..),
    WordSense (..),
    RecommendedWordInfo (..),
    scrapeWord,
    getRecommendedWordInfo,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Text.HTML.Scalpel
  ( Scraper,
    attr,
    chroots,
    hasClass,
    scrapeURL,
    text,
    (//),
    (@:),
    (@=),
  )

-- | The word info mostly recommended
data RecommendedWordInfo = RecommendedWordInfo
  { recommendedWordText :: Maybe T.Text,
    recommendedWordPartOfSpeech :: Maybe T.Text,
    recommendedWordPhonetic :: Maybe T.Text,
    recommendedWordSense :: Maybe T.Text,
    recommendedWordExample :: Maybe T.Text
  }
  deriving (Show)

-- | The information about a word.
data WordInfo = WordInfo
  { -- | The word itself.
    wordText :: T.Text,
    -- | The part of speech of the word.
    wordPartOfSpeech :: T.Text,
    -- | The phonetic transcription of the word.
    wordPhonetic :: T.Text,
    -- | The senses of the word.
    wordSenses :: [WordSense],
    -- | Other results of the word.
    wordOtherResults :: [OtherResult]
  }
  deriving (Show)

-- | The sense of a word.
data WordSense = WordSense
  { -- | The sense of the word.
    wordSense :: T.Text,
    -- | The examples of the sense.
    wordSenseExamples :: [T.Text]
  }
  deriving (Show)

data OtherResult = OtherResult
  { otherResultWord :: T.Text,
    otherResultPartOfSpeech :: T.Text,
    otherResultLink :: T.Text
  }
  deriving (Show)

-- | Scrape the word information from the Oxford Learner's Dictionaries website.
scrapeWord :: T.Text -> IO (Maybe [WordInfo])
scrapeWord word = do
  let url = "https://www.oxfordlearnersdictionaries.com/search/english/?q=" <> word
  result <- scrapeURL (T.unpack url) scrapeWordInfo
  case result of
    Just bi -> do
      otherInfo <- scrapeOtherPartOfSpeech bi
      return $ Just (bi : fromMaybe [] otherInfo)
    Nothing -> return Nothing

scrapeWordInfo :: Scraper T.Text WordInfo
scrapeWordInfo = do
  word <- text ("h1" @: [hasClass "headword"])
  partOfSpeech <- text ("span" @: [hasClass "pos"])
  phonetic <-
    fmap trimSlash $
      text ("div" @: [hasClass "phons_n_am"] // "span" @: [hasClass "phon"])
  senses <- scrapeWordSenses
  otherResults <- scrapeOtherResults
  return $ WordInfo word partOfSpeech phonetic senses otherResults
  where
    trimSlash = T.dropAround (== '/')

scrapeWordSenses :: Scraper T.Text [WordSense]
scrapeWordSenses = chroots ("li" @: [hasClass "sense"]) scrapeWordSense

scrapeWordSense :: Scraper T.Text WordSense
scrapeWordSense = do
  sense' <-
    text ("span" @: [hasClass "def"]) -- multiple senses
      <|> text ("span" @: [hasClass "xrefs"]) -- single sense
  examples' <-
    chroots
      ("ul" @: [hasClass "examples"] // "li")
      (text ("span" @: [hasClass "x"]))
  return $ WordSense sense' examples'

scrapeOtherResults :: Scraper T.Text [OtherResult]
scrapeOtherResults =
  chroots
    ("div" @: ["id" @= "relatedentries"] // "li")
    scrapeOtherResult

scrapeOtherResult :: Scraper T.Text OtherResult
scrapeOtherResult = do
  otherResultPartOfSpeech' <- text "pos"
  wordText' <- text "span"
  let otherResultWordLength =
        T.length wordText' - T.length otherResultPartOfSpeech'
  let otherResultWord' =
        T.strip . T.take otherResultWordLength $ wordText'
  otherResultLink' <- attr "href" "a"
  return $ OtherResult otherResultWord' otherResultPartOfSpeech' otherResultLink'

scrapeOtherPartOfSpeech :: WordInfo -> IO (Maybe [WordInfo])
scrapeOtherPartOfSpeech wordInfo = do
  let otherResults = wordOtherResults wordInfo
      others =
        filter sameWordWithOtherPartOfSpeech otherResults

  fmap sequence . traverse scrapeWordInfoFromLink $ others
  where
    sameWordWithOtherPartOfSpeech :: OtherResult -> Bool
    sameWordWithOtherPartOfSpeech other =
      otherResultWord other == wordText wordInfo
        && otherResultPartOfSpeech other /= wordPartOfSpeech wordInfo

    scrapeWordInfoFromLink :: OtherResult -> IO (Maybe WordInfo)
    scrapeWordInfoFromLink ot = scrapeURL (T.unpack . otherResultLink $ ot) scrapeWordInfo

getRecommendedWordInfo :: T.Text -> MaybeT IO RecommendedWordInfo
getRecommendedWordInfo word = do
  infos <- MaybeT . scrapeWord $ word
  let info = getPriorWordInfo infos
      wordText' = Just . wordText $ info
      wordPartOfSpeech' = Just . wordPartOfSpeech $ info
      wordPhonetic' = Just . wordPhonetic $ info
      headSense = listToMaybe . wordSenses $ info
      wordSense' = fmap wordSense headSense
      wordExample' = headSense >>= listToMaybe . wordSenseExamples

  return $
    RecommendedWordInfo
      wordText'
      wordPartOfSpeech'
      wordPhonetic'
      wordSense'
      wordExample'

-- | Get the prior word information.
-- | If the word can be a verb, return the information of the verb.
-- | Otherwise, return the first information, which the Oxfords Dictionaries provides.
-- | Assumption : infos is not empty.
getPriorWordInfo :: [WordInfo] -> WordInfo
getPriorWordInfo =
  fromMaybe <$> head <*> find isVerb
  where
    isVerb = (== "verb") . wordPartOfSpeech
