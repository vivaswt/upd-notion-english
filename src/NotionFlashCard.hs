{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NotionFlashCard
  ( getFlashCardsFromNotion,
    updateFlashCardToNotion,
    FlashCard (..),
  )
where

import Control.Exception.Safe (SomeException (SomeException), catch)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import qualified Data.Text as TXT
import Data.Vector as V ((!?))
import Network.HTTP.Simple
  ( Request,
    addRequestHeader,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )
import System.Environment (lookupEnv)

data FlashCard = FlashCard
  { flashCardID :: TXT.Text,
    flashCardQuestion :: Maybe TXT.Text,
    flashCardAnswer :: Maybe TXT.Text,
    flashCardIPA :: Maybe TXT.Text,
    flashCardSense :: Maybe TXT.Text
  }
  deriving (Show)

instance FromJSON FlashCard where
  parseJSON = parseFlashCard

getFlashCardsFromNotion :: ExceptT TXT.Text IO [FlashCard]
getFlashCardsFromNotion = do
  token <- getBearerToken
  let request =
        addRequestHeader "Authorization" token
          . addRequestHeader "Notion-Version" "2022-06-28"
          . setRequestHeader "Content-Type" ["application/json"]
          . setRequestBodyJSON filterCondition
          . setRequestMethod "POST"
          $ parseRequest_ "https://api.notion.com/v1/databases/3f62546fdc9e47d1920756a6dbc55cfa/query"
  response <- catch (httpLBS request) $
    \(SomeException e) -> throwE $ "Failed to get flash cards from Notion " <> TXT.pack (show e)

  case getResponseStatusCode response of
    200 -> do
      let body = getResponseBody response
          cards = parseEither parseFlashCards =<< eitherDecode body

      ExceptT . return . first TXT.pack $ cards
    code ->
      throwE $
        "Failed to get flash cards from Notion : " <> (TXT.pack . show $ code)
  where
    filterCondition =
      object
        [ "filter"
            .= object
              [ "and"
                  .= [ object
                         [ "property" .= ("基準日時" :: String),
                           "date" .= object ["is_empty" .= True]
                         ],
                       object
                         [ "property" .= ("自動更新済" :: String),
                           "checkbox" .= object ["equals" .= False]
                         ]
                     ]
              ]
        ]

updateFlashCardToNotion :: FlashCard -> ExceptT TXT.Text IO FlashCard
updateFlashCardToNotion flashCard = do
  token <- getBearerToken
  let request = editUpdateRequest token flashCard
  response <- catch (httpLBS request) $
    \(SomeException e) -> throwE $ "Failed to update flash card to Notion " <> TXT.pack (show e)
  case getResponseStatusCode response of
    200 -> return flashCard
    code ->
      throwE $
        "Failed to update flash card to Notion :"
          <> (fromMaybe "?" . flashCardAnswer $ flashCard)
          <> " : "
          <> (TXT.pack . show $ code)

editUpdateRequest :: B.ByteString -> FlashCard -> Request
editUpdateRequest tk fc =
  addRequestHeader "Authorization" tk
    . addRequestHeader "Notion-Version" "2022-06-28"
    . setRequestHeader "Content-Type" ["application/json"]
    . setRequestBodyJSON (editUpdateRequestBody fc)
    . setRequestMethod "PATCH"
    $ parseRequest_
    $ "https://api.notion.com/v1/pages/" ++ (TXT.unpack . flashCardID $ fc)

editUpdateRequestBody :: FlashCard -> Value
editUpdateRequestBody fc =
  [aesonQQ|{
    "properties": {
      "問題": {
        "type" : "title",
        "title" : #{textValue $ fromMaybe "" (flashCardQuestion fc)}
      },
      "答え": {
        "rich_text" : #{textValue $ fromMaybe "" (flashCardAnswer fc)}
      },
      "発音": {
        "rich_text" : #{textValue $ fromMaybe "" (flashCardIPA fc)}
      },
      "意味": {
        "rich_text" : #{textValue $ fromMaybe "" (flashCardSense fc)}
      },
      "自動更新済": {
        "type" : "checkbox",
        "checkbox" : true
      }
    }
  }|]

textValue :: TXT.Text -> Value
textValue t =
  [aesonQQ|[
    {
      "type" : "text",
      "text" : {
        "content" : #{t},
        "link" : null
      },
      "annotations": {
        "bold": false,
        "italic": false,
        "strikethrough": false,
        "underline": false,
        "code": false,
        "color": "default"
      },
      "href": null
    }
  ]|]

parseFlashCards :: Value -> Parser [FlashCard]
parseFlashCards = withObject "FlashCards" $ parseJSONList <=< (.: "results")

parseFlashCard :: Value -> Parser FlashCard
parseFlashCard = withObject "FlashCards" $ \o -> do
  cardID <- o .: "id"
  properties <- o .: "properties"
  question <- getTitleContent properties "問題"
  answer <- getRichTextContent properties "答え"
  ipa <- getRichTextContent properties "発音"
  sense <- getRichTextContent properties "意味"
  return $ FlashCard cardID question answer ipa sense
  where
    safeHeadArray :: Array -> Parser (Maybe Value)
    safeHeadArray a = return $ a V.!? 0

    getTitleContent :: Object -> Key -> Parser (Maybe TXT.Text)
    getTitleContent properties key = do
      property <- properties .: key
      richTexts <- property .: "title"
      firstRichText <- withArray "title" safeHeadArray richTexts
      traverse getContent firstRichText

    getRichTextContent :: Object -> Key -> Parser (Maybe TXT.Text)
    getRichTextContent properties key = do
      property <- properties .: key
      richTexts <- property .: "rich_text"
      firstRichText <- withArray "rich_text" safeHeadArray richTexts
      traverse getContent firstRichText

    getContent :: Value -> Parser TXT.Text
    getContent = withObject "title" $ \o -> do
      txt <- o .: "text"
      txt .: "content"

getBearerToken :: ExceptT TXT.Text IO B.ByteString
getBearerToken = do
  token <- liftIO $ lookupEnv "PRIVATE_NOTION_TOKEN"
  case token of
    Just t -> return . BC.pack $ "Bearer " ++ t
    Nothing -> throwE "PRIVATE_NOTION_TOKEN is not set"