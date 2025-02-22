{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Maybe (fromMaybe)
import qualified Data.Text as TXT
import qualified Data.Text.IO as TXTIO
import qualified EnglishDictionary as DIC
import qualified NotionFlashCard as NFC
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8 -- Set stdout to UTF-8 encoding
  r <- runExceptT updateNortionFlashCards
  case r of
    Left err -> TXTIO.putStrLn err
    Right updates -> mapM_ showResult updates

showResult :: ExceptT TXT.Text IO NFC.FlashCard -> IO ()
showResult flashCard = do
  r <- runExceptT flashCard
  TXTIO.putStrLn $ resultMessage r

-- | Get the result message
resultMessage :: Either TXT.Text NFC.FlashCard -> TXT.Text
resultMessage (Left err) = "[Error]" <> err
resultMessage (Right card) = "[Success]" <> wordText
  where
    wordText = fromMaybe "-" (NFC.flashCardAnswer card)

-- | Update flash cards in Notion
updateNortionFlashCards :: ExceptT TXT.Text IO [ExceptT TXT.Text IO NFC.FlashCard]
updateNortionFlashCards = do
  flashCards <- NFC.getFlashCardsFromNotion
  let edited = editFlashCards flashCards
  return $ fmap (>>= NFC.updateFlashCardToNotion) edited

-- | Edit a list of flash cards
editFlashCards :: [NFC.FlashCard] -> [ExceptT TXT.Text IO NFC.FlashCard]
editFlashCards = fmap editFlashCard

-- | Edit a flash card
editFlashCard :: NFC.FlashCard -> ExceptT TXT.Text IO NFC.FlashCard
editFlashCard card = do
  answer <-
    maybeToExceptT
      "This entry has no answer."
      . NFC.flashCardAnswer
      $ card

  info <-
    noteT "No entry from the dictionary."
      . DIC.getRecommendedWordInfo
      $ answer

  let question' = NFC.flashCardQuestion card <|> DIC.recommendedWordExample info
      answer' = editAnswer card info
      ipa' = NFC.flashCardIPA card <|> DIC.recommendedWordPhonetic info
      sense' = NFC.flashCardSense card <|> DIC.recommendedWordSense info

  return
    card
      { NFC.flashCardQuestion =
          maskAnswerInQuestion question' (DIC.recommendedWordText info),
        NFC.flashCardAnswer = answer',
        NFC.flashCardIPA = ipa',
        NFC.flashCardSense = sense'
      }
  where
    editAnswer card' info =
      let cardAnswer = NFC.flashCardAnswer card'
          infoText = DIC.recommendedWordText info
       in if cardAnswer == infoText
            then cardAnswer
            else mconcat [infoText, pure ":", cardAnswer]

maskAnswerInQuestion :: Maybe TXT.Text -> Maybe TXT.Text -> Maybe TXT.Text
maskAnswerInQuestion question answer = do
  q <- question
  a <- answer
  pure $ TXT.replace a "(...)" q

maybeToExceptT :: (Monad m) => e -> Maybe a -> ExceptT e m a
maybeToExceptT e Nothing = throwE e
maybeToExceptT _ (Just a) = return a

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Tag the 'Nothing' value of a 'MaybeT'
noteT :: (Monad m) => a -> MaybeT m b -> ExceptT a m b
noteT a = ExceptT . fmap (note a) . runMaybeT