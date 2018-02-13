{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Data.SearchEngine -- from full-text-search package

import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad (unless)

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Ix
import Data.List (intersect, union)
import qualified Data.List.NonEmpty as NE
import Data.Time

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (hFlush, stdout)

type RecipeSearchEngine = SearchEngine
      RecipeDescription
      Index
      RecipeDocField
      NoFeatures

data RecipeDocField = RecipeIndex
                    | RecipeDocument
                    deriving (Eq, Ord, Enum, Bounded, Ix, Show)

type Index = Int
type Document = [T.Text]

data RecipeDescription =
  RecipeDescription Index Document
  deriving (Eq, Show)

recipeSearchConfig :: SearchConfig RecipeDescription Index RecipeDocField NoFeatures
recipeSearchConfig = SearchConfig
  { documentKey          = documentIndex
  , extractDocumentTerms = extractRecipeTokens
  , transformQueryTerm   = normalizeQueryToken
  , documentFeatureValue = const noFeatures
  }

documentIndex :: RecipeDescription -> Index
documentIndex (RecipeDescription docIndex _ ) = docIndex

extractRecipeTokens :: RecipeDescription -> RecipeDocField -> Document
extractRecipeTokens (RecipeDescription docIndex _) RecipeIndex = [(T.pack $ show docIndex)]
extractRecipeTokens (RecipeDescription _ doc) RecipeDocument = doc

normalizeQueryToken :: Term -> RecipeDocField -> Term
normalizeQueryToken term RecipeIndex = term
normalizeQueryToken term RecipeDocument = T.toLower term

initialRecipeSearchEngine :: RecipeSearchEngine
initialRecipeSearchEngine = initSearchEngine recipeSearchConfig defaultSearchRankParameters


defaultSearchRankParameters :: SearchRankParameters RecipeDocField NoFeatures
defaultSearchRankParameters =
  SearchRankParameters { paramK1
                       , paramB
                       , paramFieldWeights
                       , paramFeatureWeights = noFeatures
                       , paramFeatureFunctions = noFeatures
                       , paramResultsetSoftLimit = 200
                       , paramResultsetHardLimit = 400
                       , paramAutosuggestPrefilterLimit = 500
                       , paramAutosuggestPostfilterLimit = 500
                       }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: RecipeDocField -> Float
    paramB RecipeIndex    = 1.0
    paramB RecipeDocument = 0.5

    paramFieldWeights :: RecipeDocField -> Float
    paramFieldWeights RecipeIndex    = 20
    paramFieldWeights RecipeDocument = 5

testMain :: IO ()
testMain = do
  putStrLn "Spinning up."
  let demoRecords = [ (RecipeDescription 1 ["See"])
                    , (RecipeDescription 2 ["See", "Jane"])
                    , (RecipeDescription 3 ["See", "Jane", "Run"])
                    ]
  let searchEngine = insertDocs demoRecords initialRecipeSearchEngine

  putStrLn "Constructing demo index..."
  printTiming "Done!" $
    evaluate searchEngine >> return ()
  putStrLn $ "search engine invariant: " ++ show (invariant searchEngine)
  let rankedResultsSee  = query searchEngine $ [T.pack "See"]
      rankedResultsJane = query searchEngine $ [T.pack "Jane"]
      rankedResultsRun  = query searchEngine $ [T.pack "Run"]
      rankedResultsCase = query searchEngine $ [T.pack "SEE"]

  putStrLn "Results:"
  putStrLn (show rankedResultsSee)
  putStrLn (show rankedResultsJane)
  putStrLn (show rankedResultsRun)
  putStrLn (show rankedResultsCase)
  putStrLn "fin!"

readDocID :: [T.Text] -> Int
readDocID line = read $ T.unpack $ head $ drop 1 line

formatError :: ParseError t e -> T.Text -> T.Text
formatError (TrivialError neList _ _) parseLine = do
  let command = T.toLower $ head $ T.words parseLine
  case (NE.head neList) of
    SourcePos _ _ sourceColumn -> do
      case command of
        "index" -> "index err: column " `T.append` (T.pack $ show sourceColumn)
        "query" -> "query err: column " `T.append` (T.pack $ show sourceColumn)
        _       -> "invalid command."
formatError (FancyError neSourcePos _) parseLine = do
  let command = T.toLower $ head $ T.words parseLine
  case (NE.head neSourcePos) of
    SourcePos _ _ sourceColumn ->
      case command of
        "index" -> "index err: column " `T.append` (T.pack $ show sourceColumn)
        "query" -> "query err: column " `T.append` (T.pack $ show sourceColumn)
        _       -> "invalid command"


dbRepl :: TVar (IO RecipeSearchEngine) -> [T.Text] -> IO ()
dbRepl tvarDB lineList =
  let line = T.intercalate " " lineList in
  case (runParser whileParser "" line) of
  Left parseError -> putStrLn $ T.unpack $ formatError parseError line
  Right (IndexCmd (RecipeDescription docIndex docWords)) -> do
    let doc = RecipeDescription docIndex docWords
    removeDoc tvarDB docIndex
    addDoc tvarDB doc
    -- TODO: Reformat this to meet requirements.
    putStrLn "Added a doc."
  Right (QueryCmd qExp) -> do
    keyList <- queryDB tvarDB qExp
    -- TODO: Reformat this to meet requirements.
    putStrLn "Results:"
    putStrLn (show keyList)
    return ()

insertDocIO :: RecipeDescription -> IO RecipeSearchEngine -> IO RecipeSearchEngine
insertDocIO recipe rseIO = do
  rse <- rseIO
  let rse' = insertDoc recipe rse
  return rse'

deleteDocIO :: Index -> IO RecipeSearchEngine -> IO RecipeSearchEngine
deleteDocIO docIndex rseIO = do
  rse <- rseIO
  let rse' = deleteDoc docIndex rse
  return rse'

removeDoc :: TVar (IO RecipeSearchEngine) -> Index -> IO ()
removeDoc tvarDB docIndex = do
  atomically $ modifyTVar' tvarDB $ deleteDocIO docIndex

queryIO :: QExp -> IO RecipeSearchEngine -> IO [Index]
queryIO queryExp rseIO = do
  rse <- rseIO
  case queryExp of
    SingleTerm term -> do
      let result = query rse [term]
      return result
    NestedExp op nExp1 nExp2 -> do
      index1 <- queryIO nExp1 rseIO
      index2 <- queryIO nExp2 rseIO
      case op of
        And -> return $ intersect index1 index2
        Or  -> return $ union index1 index2

dbCommand :: TVar (IO RecipeSearchEngine) -> SearchReplCommand -> IO [Index]
dbCommand tvarDB command =
  case command of
    IndexCmd recipe -> do
      addDoc tvarDB recipe
      return ([] :: [Index])
    QueryCmd qexp   -> queryDB tvarDB qexp

queryDB :: TVar (IO RecipeSearchEngine) -> QExp -> IO [Index]
queryDB tvarDB termList = do
  rseIO <- readTVarIO tvarDB
  queryIO termList rseIO

addDoc :: TVar (IO RecipeSearchEngine) -> RecipeDescription -> IO ()
addDoc tvarDB recipe = do
  atomically $ modifyTVar' tvarDB $ insertDocIO recipe

type Parser = Parsec Void T.Text
type WordList = [Term]
type QueryExpr = [Term]

data SearchReplCommand = IndexCmd RecipeDescription
                       | QueryCmd QExp
                       deriving (Eq, Show)

data SearchCommands = SIndex
                    | SQuery
                    deriving (Eq, Show)

whileParser :: Parser SearchReplCommand
whileParser = between sc eof replCmdP

replCmdP :: Parser SearchReplCommand
replCmdP = do
  cmd <- commandP
  case cmd of
    SIndex -> do
      docIndex <- singleNumberP
      wordList <- wordListP
      return $ IndexCmd (RecipeDescription docIndex wordList)
    SQuery -> do
      queryList <- whileQueryP
      return $ QueryCmd queryList

rword :: T.Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

commandP :: Parser SearchCommands
commandP =   SIndex <$ rword "index"
         <|> SQuery <$ rword "query"

wordListP :: Parser WordList
wordListP = (fmap . fmap) T.pack $ some $ lexeme (try $ some letterChar)

data Op = Or
        | And
        deriving (Eq, Show)

data QExp = SingleTerm T.Text
          | NestedExp Op QExp QExp
          deriving (Eq, Show)

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(" ) (symbol ")")

opP :: Parser Op
opP = And <$ symbol "&"
  <|> Or  <$ symbol "|"

singleTermP :: Parser QExp
singleTermP = do
  notFollowedBy eof
  term <- termP
  return $ SingleTerm term

nestedExpP :: Parser QExp
nestedExpP = do
  notFollowedBy eof
  field1 <- queryP
  op <- opP
  field2 <- queryP
  return $ NestedExp op field1 field2

whileQueryP :: Parser QExp
whileQueryP = between sc eof queryP

queryP :: Parser QExp
queryP =  try    singleTermP
      <|> parens nestedExpP
      <|>        nestedExpP

singleNumberP :: Parser Int
singleNumberP = lexeme L.decimal

termP :: Parser T.Text
termP = fmap T.pack $ lexeme $ some letterChar

main :: IO ()
main = do
  tVarSearchEngine <- newTVarIO $ evaluate initialRecipeSearchEngine
  let loop = do
        putStr ">"
        hFlush stdout
        t <- T.getLine
        unless (T.null t) $ do
          let lineWords = T.words t
          dbRepl tVarSearchEngine lineWords
          loop
  return ()
  loop

printTiming :: String -> IO () -> IO ()
printTiming msg action = do
  t <- getCurrentTime
  action
  t' <- getCurrentTime
  putStrLn (msg ++ ". time: " ++ show (diffUTCTime t' t))
