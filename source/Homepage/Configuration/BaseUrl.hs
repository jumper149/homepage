module Homepage.Configuration.BaseUrl where

import Data.Aeson qualified as A
import Data.Text qualified as T
import Data.Word
import Deriving.Aeson qualified as A
import GHC.Generics

displayBaseUrl :: BaseUrl -> T.Text
displayBaseUrl BaseUrl { baseUrlScheme, baseUrlAuthority, baseUrlPath } =
  let absolutePath = case baseUrlPath of
                       [] -> "/"
                       segments -> "/" <> T.intercalate "/" segments <> "/"
   in baseUrlScheme <> ":" <> maybe "" displayBaseUrlAuthority baseUrlAuthority <> absolutePath

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
