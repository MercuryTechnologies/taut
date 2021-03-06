{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Taut
  ( -- * Types
    SlackText
  , SlackImage(..)
  , SlackContext
  , SlackMessage

    -- * Produce
  , Slack(..)
  , Image(..)
  , Markdown(..)

  , (<+>)
  , parens
  , brackets
  , ticks
  , codeBlock
  , bold
  , newline
  , mention

  , context

    -- * Consume
  , render
  , toPostMessageRequest
  , chatPostMessage

    -- * Re-exports
  , Slack.HasToken(..)
  , Slack.HasManager(..)
  , userLookupByEmail
  ) where

import Control.Applicative (optional)
import Control.Monad (guard)
import Data.Foldable (asum)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson (FromJSON(..), ToJSON(..), Object, (.=), object, withObject, withText, (.:))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Word (Word8, Word16, Word32, Word64)
import Web.Slack.Chat (PostMsgReq(..), PostMsgRsp, mkPostMsgReq)
import Web.Slack.Common (SlackClientError)
import Web.Slack.User (Email(..), UserRsp)
import Web.Slack.Types (UserId(..))
import qualified Data.Aeson.Text as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Web.Slack as Slack

data SlackText = SlackText Int64 Builder
  deriving stock (Eq, Ord, Show)

instance IsString SlackText where
  fromString s = SlackText (fromIntegral (length s)) (fromString s)

instance Semigroup SlackText where
  SlackText len0 b0 <> SlackText len1 b1 = SlackText (len0 + len1) (b0 <> b1)

instance Monoid SlackText where
  mempty = SlackText 0 mempty

instance ToJSON SlackText where
  toJSON (SlackText len b)
    | len >= maxMessageLength = toJSON (TB.toLazyText b)
    | otherwise = toJSON (TL.take maxMessageLength (TB.toLazyText b) <> truncateMessage)
    where
      truncateMessage = "\n...Rest of block truncated for slack\n"
      truncateMessageLength = fromIntegral (TL.length truncateMessage)
      maxMessageLength = 3000 - truncateMessageLength

instance FromJSON SlackText where
  parseJSON = withText "SlackText" (pure . message)

newtype SlackMessage = SlackMessage [SlackBlock]
  deriving stock (Show)
  deriving newtype (Eq, ToJSON, FromJSON)

data SlackBlock
  = SlackBlockSection SlackText
  | SlackBlockImage SlackImage
  | SlackBlockContext SlackContext
  | SlackBlockDivider
  deriving stock (Eq, Show)
  -- TODO: SlackActions

instance ToJSON SlackBlock where
  toJSON = \case
    SlackBlockSection slackText -> object
      [ "type" .= ("section" :: Text)
      , "text" .= SlackContentText slackText
      ]
    SlackBlockImage i -> toJSON (SlackContentImage i)
    SlackBlockContext contents -> object
      [ "type" .= ("context" :: Text)
      , "elements" .= contents
      ]
    SlackBlockDivider -> object
      [ "type" .= ("divider" :: Text)
      ]

instance FromJSON SlackBlock where
  parseJSON v = asum $ map ($ v) [blockSection, blockImage, blockContext, blockDivider]
    where
      blockSection = withObject "SlackBlockSection" $ \o -> do
        typ <- o .: "type"
        guard (typ == ("section" :: Text))
        txt <- o .: "text"
        pure $ SlackBlockSection txt
      blockImage = const $ do
        SlackContentImage i <- parseJSON v
        pure $ SlackBlockImage i
      blockContext = withObject "SlackBlockContext" $ \o -> do
        typ <- o .: "type"
        guard (typ == ("context" :: Text))
        contents <- o .: "elements"
        pure $ SlackBlockContext contents
      blockDivider = withObject "SlackBlockDivider" $ \o -> do
        typ <- o .: "type"
        guard (typ == ("divider" :: Text))
        pure SlackBlockDivider

data SlackContent
  = SlackContentText  SlackText
  | SlackContentImage SlackImage
  deriving stock (Eq, Show)

instance ToJSON SlackContent where
  toJSON = \case
    SlackContentText t -> object
      [ "type" .= ("mrkdwn" :: Text)
      , "text" .= t
      ]
    SlackContentImage (SlackImage mtitle altText url) -> object $
      [ "type" .= ("image" :: Text)
      , "image_url" .= url
      , "alt_text" .= altText
      ] <> maybe [] mkTitle mtitle
    where
      mkTitle title =
        [ "title" .= object
          [ "type" .= ("plain_text" :: Text)
          , "text" .= title
          ]
        ]

instance FromJSON SlackContent where
  parseJSON v = asum $ map ($ v) [contentText, contentImage]
    where
      contentText = withObject "SlackContentText" $ \o -> do
        typ <- o .: "type"
        guard (typ == ("type" :: Text))
        txt <- o .: "text"
        pure $ SlackContentText txt
      contentImage = withObject "SlackContentImage" $ \o -> do
        typ <- o .: "type"
        guard (typ == ("image" :: Text))
        url <- o .: "image_url"
        altText <- o .: "alt_text"
        mtitle <- optional $ do
          title :: Object <- o .: ("title" :: Text)
          title .: "text"
        pure $ SlackContentImage $ SlackImage mtitle altText url

data SlackImage = SlackImage
  { slackImageTitle   :: Maybe Text
    -- ^ Optional title
  , slackImageAltText :: Text
  , slackImageUrl     :: Text
  }
  deriving stock (Eq, Show)

newtype SlackContext = SlackContext [SlackContent]
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid, ToJSON, FromJSON)

-- | Render a 'SlackMessage' to Strict 'Text'.
render :: SlackMessage -> Text
render = id
  . TL.toStrict
  . Aeson.encodeToLazyText
  . toJSON

infixr 6 <+>
(<+>) :: SlackText -> SlackText -> SlackText
(<+>) x y = x <> " " <> y

parens :: SlackText -> SlackText
parens x = "(" <> x <> ")"

brackets :: SlackText -> SlackText
brackets x = "[" <> x <> "]"

ticks :: SlackText -> SlackText
ticks x = "`" <> x <> "`"

codeBlock :: SlackText -> SlackText
codeBlock x = "```\n" <> x <> "\n```"

bold :: SlackText -> SlackText
bold x = "*" <> x <> "*"

newline :: SlackText -> SlackText
newline x = x <> "\n"

-- | https://api.slack.com/reference/surfaces/formatting#mentioning-users
mention :: UserId -> SlackText
mention slackUserId = "<@" <> message slackUserId <> ">"

class Slack a where
  message :: a -> SlackText

instance (a ~ Char) => Slack [a] where
  message = fromString

instance (a ~ Char) => Slack (NonEmpty a) where
  message (a :| as) = message (a : as)

instance Slack Text where
  message t = SlackText (fromIntegral (T.length t)) (TB.fromText t)

instance Slack TL.Text where
  message t = SlackText (fromIntegral (TL.length t)) (TB.fromLazyText t)

instance Slack a => Slack (Maybe a) where
  message = maybe mempty message

newtype IntegralSlack a = IntegralSlack a

instance (Integral a, Show a) => Slack (IntegralSlack a) where
  message (IntegralSlack a) = SlackText (fromIntegral (length (show a))) (TB.decimal a)

deriving via (IntegralSlack Word) instance Slack Word
deriving via (IntegralSlack Word8) instance Slack Word8
deriving via (IntegralSlack Word16) instance Slack Word16
deriving via (IntegralSlack Word32) instance Slack Word32
deriving via (IntegralSlack Word64) instance Slack Word64

deriving via (IntegralSlack Int) instance Slack Int
deriving via (IntegralSlack Int8) instance Slack Int8
deriving via (IntegralSlack Int16) instance Slack Int16
deriving via (IntegralSlack Int32) instance Slack Int32
deriving via (IntegralSlack Int64) instance Slack Int64

instance Slack UserId where
  message = message . unUserId

class Image a where
  image :: SlackImage -> a

instance Image SlackMessage where
  image i = SlackMessage [SlackBlockImage i]

instance Image SlackContext where
  image i = SlackContext [SlackContentImage i]

class Markdown a where
  markdown :: SlackText -> a

instance Markdown SlackMessage where
  markdown t = SlackMessage [SlackBlockSection t]

instance Markdown SlackContext where
  markdown t = SlackContext [SlackContentText t]

context :: SlackContext -> SlackMessage
context c = SlackMessage [SlackBlockContext c]

toPostMessageRequest :: ()
  => Text
  -> SlackMessage
  -> PostMsgReq
toPostMessageRequest channel msg
  = (mkPostMsgReq channel "") { postMsgReqBlocks = Just (render msg) }

chatPostMessage :: (MonadReader env m, Slack.HasManager env, Slack.HasToken env, MonadIO m)
  => PostMsgReq
  -> m (Either SlackClientError PostMsgRsp)
chatPostMessage = Slack.chatPostMessage

userLookupByEmail :: (MonadReader env m, Slack.HasManager env, Slack.HasToken env, MonadIO m)
  => Text
  -> m (Either SlackClientError UserRsp)
userLookupByEmail = Slack.userLookupByEmail . Email
