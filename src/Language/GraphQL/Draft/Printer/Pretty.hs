{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a printer for the @GraphQL@ language.

module Language.GraphQL.Draft.Printer.Pretty where

import           Data.Text                             (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Protolude

import           Language.GraphQL.Draft.Printer        hiding (node,
                                                        selectionSet)
import           Language.GraphQL.Draft.Syntax


renderPretty :: Doc Text -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions

renderCompact :: Doc Text -> Text
renderCompact = renderStrict . layoutCompact

renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = renderPretty . executableDocument

instance Pretty Name where
  pretty = pretty. unName

instance Printer (Doc Text) where
  stringP = pretty
  textP   = pretty
  charP   = pretty
  intP    = pretty
  doubleP = pretty
  nameP   = pretty
  nodeP = node
  selectionSetP = selectionSet


node :: TypedOperationDefinition -> Doc Text
node (TypedOperationDefinition _ name vars dirs sels) =
  pretty (fromMaybe "" name)
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <+> selectionSet sels

selectionSet :: SelectionSet -> Doc Text
selectionSet []     = ""
selectionSet selSet = nest 2 $ vbraces $ vsep (map selection selSet)
  where vbraces matter = vsep ["{", matter, "}"]
