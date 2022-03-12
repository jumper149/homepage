module Homepage.Configuration.Contact where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import GHC.Generics

data ContactInformation = ContactInformation
    { contactHeaderIcons :: HeaderIcons
    , contactName :: T.Text
    , contactSourceUrl :: Maybe T.Text
    , contactHomepageLabel :: Maybe T.Text
    , contactDonateInformation :: Maybe DonateInformation
    , contactEmailAddress :: Maybe T.Text
    , contactMatrix :: Maybe T.Text
    , contactLiberaChat :: Maybe T.Text
    , contactGithubUsername :: Maybe T.Text
    , contactGitlabUsername :: Maybe T.Text
    , contactHackageUsername :: Maybe T.Text
    , contactAurUsername :: Maybe T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "contact", A.CamelToKebab], A.RejectUnknownFields] ContactInformation

data HeaderIcons = HeaderIcons
    { headerIconFeed :: Bool
    , headerIconGithub :: Bool
    , headerIconGitlab :: Bool
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "headerIcon", A.CamelToKebab], A.RejectUnknownFields] HeaderIcons

data DonateInformation = DonateInformation
    { donatePaypalUrl :: T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "donate", A.CamelToKebab], A.RejectUnknownFields] DonateInformation
