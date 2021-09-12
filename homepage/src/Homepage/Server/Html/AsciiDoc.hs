module Homepage.Server.Html.AsciiDoc where

import Data.String
import qualified Data.Text as T
import GHC.Generics
import Text.Blaze

newtype AsciiDocHtml = AsciiDocHtml { unRawHtml :: T.Text }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (IsString, Monoid, Semigroup)

instance ToMarkup AsciiDocHtml where
  toMarkup = preEscapedText . unRawHtml
