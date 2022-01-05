module Homepage.Contact where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Deriving.Aeson as A
import GHC.Generics

data ContactInformation = ContactInformation
    { contactEmailAddress :: T.Text
    , contactMatrix :: T.Text
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving (A.FromJSON, A.ToJSON) via
    A.CustomJSON '[A.FieldLabelModifier '[A.StripPrefix "contact", A.CamelToKebab], A.RejectUnknownFields] ContactInformation
