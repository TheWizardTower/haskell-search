{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Data.SearchEngine -- from full-text-search package

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad (unless, void)

import Data.Ix
import Data.Time

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO (hFlush, stdout)


-- SearchConfig
--    documentKey :: doc -> key
--    extractDocumentTerms :: doc -> field -> [Term]
--    transformQueryTerm   :: Term -> field -> Term
--    documentFeatureValue :: doc -> feature -> Float

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

recipeSearchConfig :: SearchConfig RecipeDescription Index RecipeDocField NoFeatures
recipeSearchConfig = SearchConfig
  { documentKey          = documentIndex
  , extractDocumentTerms = extractRecipeTokens
  , transformQueryTerm   = normalizeQueryToken
  , documentFeatureValue = const noFeatures
  }

documentIndex :: RecipeDescription -> Index
documentIndex (RecipeDescription index _ ) = index

extractRecipeTokens :: RecipeDescription -> RecipeDocField -> Document
extractRecipeTokens (RecipeDescription index _) RecipeIndex = [(T.pack $ show index)]
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


dbRepl :: TVar (IO RecipeSearchEngine) -> [T.Text] -> IO ()
dbRepl tvarDB line = case (head line) of
  "index" -> do
    let docIndex = readDocID line
        docWords = drop 2 line
        doc      = RecipeDescription docIndex docWords
    removeDoc tvarDB docIndex
    addDoc tvarDB doc
    putStrLn "Added a doc."
  "query" -> do
    let queryWords = drop 1 line
    keyList <- queryDB tvarDB queryWords
    putStrLn "Results:"
    putStrLn (show keyList)
    return ()
  _ -> do
    -- Put an error message here, and print the help page.
    putStrLn "You did a weird thing."
    return ()

insertDocIO :: RecipeDescription -> IO RecipeSearchEngine -> IO RecipeSearchEngine
insertDocIO recipe rseIO = do
  rse <- rseIO
  let rse' = insertDoc recipe rse
  return rse'

deleteDocIO docIndex rseIO = do
  rse <- rseIO
  let rse' = deleteDoc docIndex rse
  return rse'

removeDoc :: TVar (IO RecipeSearchEngine) -> Index -> IO ()
removeDoc tvarDB docIndex = do
  atomically $ modifyTVar' tvarDB $ deleteDocIO docIndex

queryIO :: [Term] -> IO RecipeSearchEngine -> IO [Index]
queryIO queryTerm rseIO = do
  rse <- rseIO
  let result = query rse queryTerm
  return result

queryDB :: TVar (IO RecipeSearchEngine) -> [Term] -> IO [Index]
queryDB tvarDB termList = do
  rseIO <- readTVarIO tvarDB
  queryIO termList rseIO

addDoc :: TVar (IO RecipeSearchEngine) -> RecipeDescription -> IO ()
addDoc tvarDB recipe = do
  atomically $ modifyTVar' tvarDB $ insertDocIO recipe

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
