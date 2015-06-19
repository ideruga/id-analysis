module Handler.AnalysisTest where

import Import hiding (length, withManager)
import System.Random
import Data.Text hiding (pack, map, length)
import Text.Read (reads, read)
import Data.List hiding (map, (++))
import Data.Aeson (Value, encode, object, (.=))
import Control.Monad.IO.Class  (liftIO)
import Data.Aeson              (Value (Object, String))
import Data.Aeson              (encode, object, (.=))
import Database.Persist
import Database.Persist.Sql
import Data.Aeson.Parser       (json)
import Data.Conduit            (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl,
                                          requestBody, withManager)


getAnalysisTestR :: Int -> Int -> Text -> Handler RepPlain
getAnalysisTestR chapter currentSection rndText = do 
    sections <- runDB $ selectList [SectionChapterId ==. toSqlKey (fromIntegral chapter), SectionDone ==. True, SectionNumber !=. fromIntegral currentSection] []
    let rndDouble = read (Data.Text.unpack rndText) :: Double
    let rndInt = floor $ rndDouble*(realToFrac $ length $ map entityVal sections)
    let randomSectionEntity = sections!!rndInt
    let (Entity randomSectionId randomSection) = randomSectionEntity
    let randomSectionName = maybe "-" (\ x -> x) $ sectionName randomSection
    let randomSectionDescription = maybe "-" (\ x -> x) $ sectionDescription randomSection
    return $ RepPlain . toContent $ show $ encode $ Test {newSectionName = randomSectionName, newSectionDescription = randomSectionDescription, newSectionNumber = fromIntegral $ sectionNumber randomSection, chapterNumber = chapter}
        --Nothing -> return RepPlain . toContent $ show $ encode $ Test {newSectionName = "woops", newSectionNumber = 0, chapterNumber = chapter} 
        
    --res <- runDB $ rawQuery "SELECT count(*) cnt, sum(CASE WHEN done THEN 1 ELSE 0 END) sm FROM SECTION" [] $$ CL.map (convertFromPersistent) =$ CL.consume
    --allSections <- runDB $ selectList [SectionChapterId ==. toSqlKey chapterId] []
    --let allSectionsTotal = length $ map entityVal allSections
    --cnt <- runDB $ count [SectionChapterId ==. toSqlKey (fromIntegral chapter), SectionDone ==. True]
    --res <- runDB $ rawQuery "SELECT id, name FROM section WHERE chapterId = ? AND done = true OFFSET random()*? LIMIT 1" [chapter, cnt] $$ CL.consume
    --let (newSection, newSectionName) = parseValues res
    --let newSection = 4
    --let newSectionName = "test section"
    --randomSection <- selectSections chapter cnt
    --return $ RepPlain . toContent $ show $ encode $ Test {sectionName = (SectionName randomSection), sectionNumber = fromSqlKey (id randomSection), chapterNumber = chapter}
    --where
        --selectSections :: Int -> Int -> Handler [Entity Section]
        --selectSections chapter cnt = runDB $ rawSql s [toPersistValue chapter, toPersistValue cnt]
            --where s = "SELECT ?? FROM section WHERE chapterId = ? AND done = true OFFSET random()*? LIMIT 1"

data Test = Test
    { newSectionName :: Text, newSectionDescription :: Text, newSectionNumber :: Int, chapterNumber :: Int} deriving Show
instance ToJSON Test where
    toJSON (Test newSectionName newSectionDescription newSectionNumber chapterNumber) = object ["sectionName" .= newSectionName, "sectionDescription" .= newSectionDescription, "sectionNumber" .= newSectionNumber, "chapterNumber" .= chapterNumber]

