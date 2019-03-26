{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import           Protolude
import           System.Environment                        (getArgs)

import qualified Data.ByteString.Lazy                      as BL
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding.Error                  as TE
import qualified Data.Text.Lazy                            as TL
import qualified Data.Text.Lazy.Encoding                   as TL


import           Generator.Language.GraphQL.Document
import           Generator.Language.GraphQL.Primitives
import           Generator.Language.GraphQL.Selection
import           Language.GraphQL.Draft.Parser             (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer.ByteString as PP.BB
import qualified Language.GraphQL.Draft.Printer.Pretty     as PP
import qualified Language.GraphQL.Draft.Printer.Text       as PP.TB


data TestMode = TMDev | TMQuick | TMRelease
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    TMQuick   -> runTest 20
    TMDev     -> runTest 100
    TMRelease -> runTest 1000
  where
    parseArgs = foldr parseArg TMDev
    parseArg str _ = case str of
      "quick"   -> TMQuick
      "release" -> TMRelease
      _         -> TMDev

runTest :: TestLimit -> IO ()
runTest = void . tests

tests :: TestLimit -> IO Bool
tests nTests =
  checkParallel $ Group "Test.printer.parser"
    [ ("property [ parse (prettyPrint ast) == ast ]", propParserPrinter nTests)
    , ("property [ parse (textbuilderPrint ast) == ast ]", propParserTBPrinter nTests)
    , ("property [ parse (bytestringBuilderPrint ast) == ast ]", propParserBBPrinter nTests)
    ]

propParserPrinter :: TestLimit -> Property
propParserPrinter space =
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = PP.renderPretty $ PP.executableDocument xs
    either (\e -> print e >> fail e) (xs ===) $ parseExecutableDoc rendered
  where
    fail e = footnote (T.unpack e) >> failure

propParserTBPrinter :: TestLimit -> Property
propParserTBPrinter space =
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = PP.TB.renderExecutableDoc xs
    either (\e -> print e >> fail e) (xs ===) $ parseExecutableDoc rendered
  where
    fail e = footnote (T.unpack e) >> failure

propParserBBPrinter :: TestLimit -> Property
propParserBBPrinter space =
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = PP.BB.renderExecutableDoc xs
    either (\e -> print e >> fail e) (xs ===) $ parseExecutableDoc $
      bsToTxt rendered
  where
    fail e = footnote (T.unpack e) >> failure

bsToTxt :: BL.ByteString -> T.Text
bsToTxt = TL.toStrict . TL.decodeUtf8With TE.lenientDecode
