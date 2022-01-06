module Homepage.BaseUrl where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import Data.Word
import GHC.Generics

displayBaseUrl :: BaseUrl -> T.Text
displayBaseUrl BaseUrl { baseUrlScheme, baseUrlAuthority, baseUrlPath } =
  baseUrlScheme <> ":" <> maybe "" displayBaseUrlAuthority baseUrlAuthority <> "/" <> T.intercalate "/" baseUrlPath

data BaseUrl = BaseUrl
    { baseUrlScheme :: T.Text
    , baseUrlAuthority :: Maybe BaseUrlAuthority
    , baseUrlPath :: [T.Text]
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "baseUrl", A.CamelToKebab], A.RejectUnknownFields] BaseUrl

displayBaseUrlAuthority :: BaseUrlAuthority -> T.Text
displayBaseUrlAuthority BaseUrlAuthority { baseUrlAuthorityHost, baseUrlAuthorityPort } =
  "//" <> baseUrlAuthorityHost <> maybe "" ((":" <>) . T.pack . show) baseUrlAuthorityPort

data BaseUrlAuthority = BaseUrlAuthority
    { baseUrlAuthorityHost :: T.Text
    , baseUrlAuthorityPort :: Maybe Word16
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "baseUrlAuthority", A.CamelToKebab], A.RejectUnknownFields] BaseUrlAuthority
