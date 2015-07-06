module Handler.AnalysisStats where

import Import hiding (length,filter)
import Data.Map (Map)
import Data.Text (empty)
import Data.Maybe
import Database.Persist
import Database.Persist.Sql
import Data.List (length,filter, genericLength)
import Text.Julius (rawJS)
import qualified Data.Conduit.List as CL
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.BrowserId

data UISection = UISection { uiSectionId :: Int
    , uiSectionName :: Text
    , uiSectionDescription :: Text
    , uiSectionDone :: Bool
}

data UIChapter = UIChapter { uiChapterId :: Int    
    , uiChapterName :: Text
    , uiChapterDescription :: Text
    , uiChapterSections :: [UISection]
    , uiChapterProgress :: Int
}
sectionChecked :: UISection -> Bool
sectionChecked section = uiSectionDone section

getAnalysisStatsR :: Int64 -> Handler Html
getAnalysisStatsR chapterId = do
    mauth <- maybeAuth
    let emailAddr = case mauth of Nothing -> "nothing"
                                  Just (Entity _ user) -> userIdent user 
    $(logInfo) $ "authentication: " ++ (pack (show emailAddr))
    maid <- maybeAuthId

    let abc = True
    sections <- runDB $ selectList ([] :: [Filter Section]) []

    doneSectionsDB <- runDB $ selectList [DoneSectionsUserIdent ==. emailAddr] []
    let doneSections = map (doneSectionsSectionId . entityVal) doneSectionsDB


    chapters <- runDB $ selectList ([] :: [Filter Chapter]) []
    let uiChapters = [UIChapter { uiChapterId = chapterKeyToInt chId
                   , uiChapterName = chapterName ch
                   , uiChapterDescription = case chapterDescription ch of Nothing -> empty
                                                                          Just txt -> txt
                   , uiChapterSections = [UISection { uiSectionId = sectionKeyToInt seId
                                                    , uiSectionName = case sectionName se of Nothing -> empty
                                                                                             Just name -> name
                                                    , uiSectionDescription = case sectionDescription se of Nothing -> empty
                                                                                                           Just desc -> desc
                                                    , uiSectionDone = seId `elem` doneSections} 
                                                    | Entity seId se <- filter (\ s -> chId == sectionChapterId (entityVal s)) sections]
                   , uiChapterProgress = calcChapterProgress chId sections doneSections } 
                   | Entity chId ch <- chapters]


    let totalSectionsCount = toRational $ sum $ map (\ c -> genericLength $ uiChapterSections c) uiChapters
    let doneSectionsCount = toRational $ sum $ map (\ c -> genericLength $ filter (\ s -> uiSectionDone s) (uiChapterSections c)) uiChapters
    let allSectionsPercent = toInteger $ floor $ 100 * doneSectionsCount / totalSectionsCount

    defaultLayout $(widgetFile "analysis")
    where
        chapterKeyToInt :: Key Chapter -> Int
        chapterKeyToInt key = fromIntegral $ fromSqlKey key

        sectionKeyToInt :: Key Section -> Int
        sectionKeyToInt key = fromIntegral $ fromSqlKey key

        calcChapterProgress :: Key Chapter -> [Entity Section] -> [Key Section] -> Int
        calcChapterProgress chKey sects doneSections = do
            let allSections = filter (\ s -> chKey == sectionChapterId (entityVal s)) sects
            let allCount = toRational $ length allSections
            let doneCount = toRational $ length $ filter (\ s -> (entityKey s) `elem` doneSections) allSections 
            fromInteger $ toInteger $ floor (100 * doneCount/allCount) 

--    where 
-- convertFromPersistent [] = Nothing
-- convertFromPersistent [PersistInt64 sum,PersistInt64 category] = do
-- putStrLn (pack (show (sum)))
-- return (Just (sum,category))
-- convertFromPersistent _ = Nothing

