{-# LANGUAGE  LambdaCase
            , DeriveDataTypeable
            , DeriveGeneric
            , GeneralizedNewtypeDeriving
            , OverloadedStrings
            , TemplateHaskell
  #-}
module Matter.Language.Syntax.Name where

import Control.Lens
import Data.Aeson
import Data.Binary (Binary)
import Data.Data
import Data.Semigroup
import Data.Monoid ((<>))
import Data.String
import Data.Text (Text, unpack)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Matter.Language.Syntax.Location


------------------------------------------------------------------------------
-- Name Hint
data NameHint = N Text | Nameless
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Binary NameHint
instance Plated NameHint
instance FromJSON NameHint
instance ToJSON NameHint

instance Semigroup NameHint where
  (<>) h1 h2 = case (h1, h2) of
    (N n, _)      -> N n
    (_, N n)      -> N n
    _             -> Nameless

instance Monoid NameHint where
  mempty = Nameless




fromText :: IsString a => Text -> a
fromText = fromString . unpack


withNameHint :: Pretty p => NameHint -> (Text -> p) -> p
withNameHint (N n) f = f n
withNameHint Nameless _ = undefined


data QName = QName ModuleName Text
  deriving (Eq, Generic, Ord, Show)

data QConstr = QConstr QName Text
  deriving (Eq, Generic, Ord, Show)

newtype ModuleName = ModuleName [Text]
  deriving (Eq, Generic, Ord, Show, Semigroup, Monoid)


------------------------------------------------------------------------------
-- Name Data Type
data Name t b
  = Name b
  | NLoc Loc (Name t b)
  | NTerm t (Name t b)
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


instance (Binary t, Binary b) => Binary (Name t b)
instance (Data t, Plated t, Data b, Plated b) => Plated (Name t b)
instance (FromJSON t, FromJSON b) => FromJSON (Name t b)
instance (ToJSON t, ToJSON b) => ToJSON (Name t b)


--------------------------------------------------------------------------------
-- Helpers

mkLocName :: (b, Loc) -> Name t b
mkLocName (n, l) = NLoc l $ Name n


locName :: Name t b -> Maybe Loc
locName = \case
  Name _ -> Nothing
  NLoc l _ -> Just l
  NTerm _ n -> locName n

locName' :: Name t b -> Loc
locName' = \case
  Name _ -> error "Name has no embedded location."
  NLoc l _ -> l
  NTerm _ n -> locName' n

  
typeName :: Name t b -> Maybe t
typeName = \case
  Name _ -> Nothing
  NLoc _ n -> typeName n
  NTerm t _ -> Just t
  

typeName' :: Name t b -> t
typeName' = \case
  Name _ -> error "Name has no embedded term."
  NLoc _ n -> typeName' n
  NTerm t _ -> t


readName :: Name t b -> b
readName = \case
  Name n -> n
  NLoc _ n -> readName n
  NTerm _ n -> readName n


-- -----------------------------------------------------------------------------
-- | Pretty Printing

instance (Pretty t, Pretty b) => Pretty (Name t b) where
  pretty = \case
    Name n    -> pretty n
    NLoc _ n  -> pretty n
    NTerm _ n -> pretty n