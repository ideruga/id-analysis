module Handler.AnalysisTest where

import Data.Maybe
import Import hiding (length, withManager)
import System.Random
import Data.Text hiding (pack, map, length, filter)
import Text.Read (reads, read)
import Data.List hiding (map, (++), filter, elem)
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


getProbabilisticTest emailAddr chapter currentSection rndText = do
    doneSectionsDB <- runDB $ selectList [DoneSectionsUserIdent ==. emailAddr] []
    let doneSections = map (doneSectionsSectionId . entityVal) doneSectionsDB
    
    sections <- runDB $ selectList [SectionChapterId ==. toSqlKey (fromIntegral chapter), SectionNumber !=. fromIntegral currentSection] [Asc SectionId]
    let selectedSections = filter (\ x -> (entityKey x) `elem` doneSections) $ sections

    let rndDouble = read (Data.Text.unpack rndText) :: Double
    let rndInt = floor $ rndDouble*(realToFrac $ length selectedSections)
    let randomSectionEntity = selectedSections!!rndInt
    let (Entity randomSectionId randomSection) = randomSectionEntity
    let randomSectionName = maybe "-" (\ x -> x) $ sectionName randomSection
    let randomSectionDescription = maybe "-" (\ x -> x) $ sectionDescription randomSection
    return Test {newSectionName = randomSectionName, newSectionDescription = randomSectionDescription, newSectionNumber = fromIntegral $ sectionNumber randomSection, chapterNumber = chapter}
 


getSectionTest emailAddr chapter currentSection rndText = do
    maybeSectionEntity <- runDB $ selectFirst [SectionChapterId ==. toSqlKey (fromIntegral chapter), SectionNumber >=. fromIntegral (currentSection + 1)] [Asc SectionId]

    let sectionEntity = fromJust maybeSectionEntity
    let section = entityVal sectionEntity
    let selectedSectionName = maybe "-" (\ x -> x) $ sectionName section
    let selectedSectionDescription = maybe "-" (\ x -> x) $ sectionDescription section
    return Test {newSectionName = selectedSectionName, newSectionDescription = selectedSectionDescription, newSectionNumber = fromIntegral $ sectionNumber section, chapterNumber = chapter}
 


getAnalysisTestR :: Text -> Int -> Int -> Text -> Handler RepPlain
getAnalysisTestR testType chapter currentSection rndText = do 
    mauth <- maybeAuth
    let emailAddr = case mauth of Nothing -> "nothing"
                                  Just (Entity _ user) -> userIdent user 
    let res = if testType == "prob" then getProbabilisticTest emailAddr chapter currentSection rndText
                                    else getSectionTest emailAddr chapter currentSection rndText
    test <- res
    return $ RepPlain . toContent $ show $ encode $ test

 
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

