{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.GraphQL.Draft.Introspection where

import           Control.Monad                 (fail, when)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text                     (Text)
import           Protolude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.Text                     as T

import           Language.GraphQL.Draft.Syntax
import           Language.GraphQL.Draft.Utils


-- Introspection spec: https://facebook.github.io/graphql/June2018/#sec-Introspection

data IntrospectionResult
  = IntrospectionResult
  { _irSchema :: !GISchema
  , _irType   :: !(Maybe GIType)
  } deriving (Show, Eq)

-- GraphQL Introspection __Schema
data GISchema
  = GISchema
  { _gisTypes            :: ![GIType]
  , _gisQueryType        :: !(Maybe GIType)
  , _gisMutationType     :: !(Maybe GIType)
  , _gisSubscriptionType :: !(Maybe GIType)
  , _gisDirectives       :: ![GIDirective]
  } deriving (Show, Eq)

-- GraphQL Introspection __Type
data GIType
  = GIType
  { _gitKind          :: !GITypeKind
  , _gitName          :: !(Maybe Text)
  , _gitDescription   :: !(Maybe Text)
  , _gitFields        :: !(Maybe [GIField])
  , _gitInterfaces    :: !(Maybe [GIType])
  , _gitPossibleTypes :: !(Maybe [GIType])
  , _gitEnumValues    :: !(Maybe [GIEnumValue])
  , _gitInputFields   :: !(Maybe [GIInputValue])
  , _gitOfType        :: !(Maybe GIType)
  } deriving (Show, Eq)

data GIField
  = GIField
  { _gifName              :: !Text
  , _gifDescription       :: !(Maybe Text)
  , _gifArgs              :: ![GIInputValue]
  , _gifType              :: !GIType
  , _gifIsDeprecated      :: !Bool
  , _gifDeprecationReason :: !(Maybe Text)
  } deriving (Show, Eq)

data GIInputValue
  = GIInputValue
  { _giivName         :: !Text
  , _giivDescription  :: !(Maybe Text)
  , _giivType         :: !GIType
  , _giivDefaultValue :: !(Maybe Text)
  } deriving (Show, Eq)

data GIEnumValue
  = GIEnumValue
  { _gievName              :: !Text
  , _gievDescription       :: !(Maybe Text)
  , _gievIsDeprecated      :: !Bool
  , _gievDeprecationReason :: !(Maybe Text)
  } deriving (Show, Eq)

data GITypeKind
  = TKScalar
  | TKObject
  | TKInterface
  | TKUnion
  | TKEnum
  | TKInputObject
  | TKList
  | TKNonNull
  deriving (Show, Eq)

-- GraphQL Introspection Directive
data GIDirective
  = GIDirective
  { _gidName        :: !Text
  , _gidDescription :: !(Maybe Text)
  , _gidLocations   :: ![GIDirectiveLocation]
  , _gidArgs        :: ![GIInputValue]
  } deriving (Show, Eq)

data GIDirectiveLocation
  = DLQuery
  | DLMutation
  | DLSubscription
  | DLField
  | DLFragmentDefinition
  | DLFragmentSpread
  | DLInlineFragment
  | DLSchema
  | DLScalar
  | DLObject
  | DLFieldDefinition
  | DLArgumentDefinition
  | DLInterface
  | DLUnion
  | DLEnum
  | DLEnumValue
  | DLInputObject
  | DLInputFieldDefinition
  deriving (Show, Eq)


instance FromJSON IntrospectionResult where
  parseJSON = withObject "IntrospectionResult" $ \o -> do
    dat    <- o .: "data"
    schema <- dat .: "__schema"
    ty     <- dat .:? "__type"
    return $ IntrospectionResult schema ty

instance FromJSON GIDirectiveLocation where
  parseJSON = \case
    "QUERY"                  -> return DLQuery
    "MUTATION"               -> return DLMutation
    "SUBSCRIPTION"           -> return DLSubscription
    "FIELD"                  -> return DLField
    "FRAGMENT_DEFINITION"    -> return DLFragmentDefinition
    "FRAGMENT_SPREAD"        -> return DLFragmentSpread
    "INLINE_FRAGMENT"        -> return DLInlineFragment
    "SCHEMA"                 -> return DLSchema
    "SCALAR"                 -> return DLScalar
    "OBJECT"                 -> return DLObject
    "FIELD_DEFINITION"       -> return DLFieldDefinition
    "ARGUMENT_DEFINITION"    -> return DLArgumentDefinition
    "INTERFACE"              -> return DLInterface
    "UNION"                  -> return DLUnion
    "ENUM"                   -> return DLEnum
    "ENUM_VALUE"             -> return DLEnumValue
    "INPUT_OBJECT"           -> return DLInputObject
    "INPUT_FIELD_DEFINITION" -> return DLInputFieldDefinition

    _ -> fail "not a valid DirectiveLocation"

instance ToJSON GIDirectiveLocation where
  toJSON = \case
    DLQuery                -> "QUERY"
    DLMutation             -> "MUTATION"
    DLSubscription         -> "SUBSCRIPTION"
    DLField                -> "FIELD"
    DLFragmentDefinition   -> "FRAGMENT_DEFINITION"
    DLFragmentSpread       -> "FRAGMENT_SPREAD"
    DLInlineFragment       -> "INLINE_FRAGMENT"
    DLSchema               -> "SCHEMA"
    DLScalar               -> "SCALAR"
    DLObject               -> "OBJECT"
    DLFieldDefinition      -> "FIELD_DEFINITION"
    DLArgumentDefinition   -> "ARGUMENT_DEFINITION"
    DLInterface            -> "INTERFACE"
    DLUnion                -> "UNION"
    DLEnum                 -> "ENUM"
    DLEnumValue            -> "ENUM_VALUE"
    DLInputObject          -> "INPUT_OBJECT"
    DLInputFieldDefinition -> "INPUT_FIELD_DEFINITION"

instance FromJSON GITypeKind where
  parseJSON = \case
    "SCALAR"       -> return TKScalar
    "OBJECT"       -> return TKObject
    "INTERFACE"    -> return TKInterface
    "UNION"        -> return TKUnion
    "ENUM"         -> return TKEnum
    "INPUT_OBJECT" -> return TKInputObject
    "LIST"         -> return TKList
    "NON_NULL"     -> return TKNonNull

instance ToJSON GITypeKind where
  toJSON = \case
    TKScalar      -> "SCALAR"
    TKObject      -> "OBJECT"
    TKInterface   -> "INTERFACE"
    TKUnion       -> "UNION"
    TKEnum        -> "ENUM"
    TKInputObject -> "INPUT_OBJECT"
    TKList        -> "LIST"
    TKNonNull     -> "NON_NULL"


$(deriveJSON (aesonDrop 5 camelCase) ''GIInputValue)
$(deriveJSON (aesonDrop 4 camelCase) ''GIField)
$(deriveJSON (aesonDrop 5 camelCase) ''GIEnumValue)
$(deriveJSON (aesonDrop 4 camelCase) ''GIDirective)
$(deriveJSON (aesonDrop 4 camelCase) ''GIType)
$(deriveJSON (aesonDrop 4 camelCase) ''GISchema)


-- err :: (Monad m) => Text -> m a
-- err = fail . T.unpack

-- kindErr gKind eKind = err $ "Invalid `kind: " <> gKind <> "` in " <> eKind

-- instance FromJSON ScalarTypeDefinition where
--   parseJSON = withObject "ScalarTypeDefinition" $ \o -> do
--     kind <- o .: "kind"
--     name <- o .:  "name"
--     desc <- o .:? "description"
--     when (kind /= "SCALAR") $ kindErr kind "scalar"
--     return $ ScalarTypeDefinition desc name []

-- instance FromJSON ObjectTypeDefinition where
--   parseJSON = withObject "ObjectTypeDefinition" $ \o -> do
--     kind       <- o .: "kind"
--     name       <- o .:  "name"
--     desc       <- o .:? "description"
--     fields     <- o .: "fields"
--     interfaces <- o .: "interfaces"
--     when (kind /= "OBJECT") $ kindErr kind "object"
--     return $ ObjectTypeDefinition desc name interfaces [] fields

-- instance FromJSON FieldDefinition where
--   parseJSON = withObject "FieldDefinition" $ \o -> do
--     name  <- o .:  "name"
--     desc  <- o .:? "description"
--     args  <- o .: "args"
--     _type <- o .: "type"
--     return $ FieldDefinition desc name args _type []


-- instance FromJSON GType where
--   parseJSON = withObject "GType" $ \o -> do
--     kind  <- o .: "kind"
--     mName <- o .:? "name"
--     mType <- o .:? "ofType"
--     case (kind, mName, mType) of
--       ("NON_NULL", _, Just typ) -> return $ mkNotNull typ
--       ("NON_NULL", _, Nothing)  -> err "NON_NULL should have `ofType`"
--       ("LIST", _, Just typ)     -> return $ TypeList (Nullability True) (ListType typ)
--       ("LIST", _, Nothing)      -> err "LIST should have `ofType`"
--       (_, Just name, _)         -> return $ TypeNamed (Nullability True) name
--       _                         -> err $ "kind: " <> kind <> " should have name"

--     where
--       mkNotNull typ = case typ of
--         TypeList _ typ -> TypeList (Nullability False) typ
--         TypeNamed _ n  -> TypeNamed (Nullability False) n


-- instance FromJSON ListType

-- $(J.deriveFromJSON (J.aesonDrop 4 J.camelCase) ''InputValueDefinition)
-- $(J.deriveFromJSON (J.aesonDrop 3 J.camelCase) ''ObjectFieldG)

-- instance (J.FromJSON a) => J.FromJSON (ObjectValueG a)

-- $(J.deriveFromJSON J.defaultOptions{J.sumEncoding=J.UntaggedValue} ''ValueConst)
-- $(J.deriveFromJSON J.defaultOptions{J.sumEncoding=J.UntaggedValue} ''Value)


-- instance FromJSON InterfaceTypeDefinition where
--   parseJSON = withObject "InterfaceTypeDefinition" $ \o -> do
--     kind  <- o .: "kind"
--     name  <- o .:  "name"
--     desc  <- o .:? "description"
--     fields <- o .: "fields"
--     when (kind /= "INTERFACE") $ kindErr kind "interface"
--     return $ InterfaceTypeDefinition desc name [] fields

-- instance FromJSON UnionTypeDefinition where
--   parseJSON = withObject "UnionTypeDefinition" $ \o -> do
--     kind  <- o .: "kind"
--     name  <- o .:  "name"
--     desc  <- o .:? "description"
--     possibleTypes <- o .: "possibleTypes"
--     when (kind /= "UNION") $ kindErr kind "union"
--     return $ UnionTypeDefinition desc name [] possibleTypes

-- instance FromJSON EnumTypeDefinition where
--   parseJSON = withObject "EnumTypeDefinition" $ \o -> do
--     kind  <- o .: "kind"
--     name  <- o .:  "name"
--     desc  <- o .:? "description"
--     vals  <- o .: "enumValues"
--     when (kind /= "ENUM") $ kindErr kind "enum"
--     return $ EnumTypeDefinition desc name [] vals

-- instance FromJSON EnumValueDefinition where
--   parseJSON = withObject "EnumValueDefinition" $ \o -> do
--     name  <- o .:  "name"
--     desc  <- o .:? "description"
--     return $ EnumValueDefinition desc name []

-- instance FromJSON InputObjectTypeDefinition where
--   parseJSON = withObject "InputObjectTypeDefinition" $ \o -> do
--     kind  <- o .: "kind"
--     name  <- o .:  "name"
--     desc  <- o .:? "description"
--     mInputFields <- o .:? "inputFields"
--     let inputFields = fromMaybe [] mInputFields
--     when (kind /= "INPUT_OBJECT") $ kindErr kind "input_object"
--     return $ InputObjectTypeDefinition desc name [] inputFields


-- instance FromJSON TypeDefinition where
--   parseJSON = withObject "TypeDefinition" $ \o -> do
--     kind :: Text <- o .: "kind"
--     case kind of
--       "SCALAR"       -> TypeDefinitionScalar      <$> parseJSON (J.Object o)
--       "OBJECT"       -> TypeDefinitionObject      <$> parseJSON (J.Object o)
--       "INTERFACE"    -> TypeDefinitionInterface   <$> parseJSON (J.Object o)
--       "UNION"        -> TypeDefinitionUnion       <$> parseJSON (J.Object o)
--       "ENUM"         -> TypeDefinitionEnum        <$> parseJSON (J.Object o)
--       "INPUT_OBJECT" -> TypeDefinitionInputObject <$> parseJSON (J.Object o)


-- instance FromJSON SchemaDocument where
--   parseJSON = withObject "SchemaDocument" $ \o -> do
--     _data <- o .: "data"
--     schema <- _data .: "__schema"
--     -- the list of types
--     types <- schema .: "types"
--     -- query root
--     queryType <- schema .: "queryType"
--     queryRoot <- queryType .: "name"
--     -- mutation root
--     mMutationType <- schema .:? "mutationType"
--     mutationRoot <- case mMutationType of
--       Nothing      -> return Nothing
--       Just mutType -> do
--         mutRoot <- mutType .: "name"
--         return $ Just mutRoot
--     -- subscription root
--     mSubsType <- schema .:? "subscriptionType"
--     subsRoot <- case mSubsType of
--       Nothing      -> return Nothing
--       Just subsType -> do
--         subRoot <- subsType .: "name"
--         return $ Just subRoot
--     return $ SchemaDocument types queryRoot mutationRoot subsRoot


-- getNamedTyp :: TypeDefinition -> Name
-- getNamedTyp ty = case ty of
--   TypeDefinitionScalar t      -> _stdName t
--   TypeDefinitionObject t      -> _otdName t
--   TypeDefinitionInterface t   -> _itdName t
--   TypeDefinitionUnion t       -> _utdName t
--   TypeDefinitionEnum t        -> _etdName t
--   TypeDefinitionInputObject t -> _iotdName t
