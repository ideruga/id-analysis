module Handler.AnalysisStats where

import Import hiding (length,filter)
import Data.Map (Map)
import Data.Maybe
import Database.Persist
import Database.Persist.Sql
import Data.List (length,filter)
import Text.Julius (rawJS)
import qualified Data.Conduit.List as CL

getAnalysisStatsR :: Int64 -> Handler Html
getAnalysisStatsR chapterId = do
    --res <- runDB $ rawQuery "SELECT count(*) cnt, sum(CASE WHEN done THEN 1 ELSE 0 END) sm FROM SECTION" [] $$ CL.map (convertFromPersistent) =$ CL.consume
    allSections <- runDB $ selectList ([] :: [Filter Section]) []
    let allSectionsTotal = length $ map entityVal allSections
    $(logInfo) $ "allSectionsTotal value: " ++ (pack (show allSectionsTotal))
    let allSectionsDone = length . filter (\ x -> sectionDone x == True) $ map entityVal allSections
    $(logInfo) $ "allSectionsDone: " ++ (pack (show allSectionsDone))
    let tmp0 = (toRational 100)*(toRational allSectionsDone)/(toRational allSectionsTotal)
    let allSectionsPercent = show $ toInteger $ floor tmp0


    chapters <- runDB $ selectList ([] :: [Filter Chapter]) []
    sections <- runDB $ selectList [SectionChapterId ==. toSqlKey chapterId] []
    let sectionsTotal = length $ map entityVal sections
    $(logInfo) $ "Greeting from getHomeR: " ++ (pack (show sectionsTotal))
    let sectionsDone = length . filter (\ x -> sectionDone x == True) $ map entityVal sections
    $(logInfo) $ "Greeting from getHomeR: " ++ (pack (show sectionsDone))
    let tmp = (toRational 100)*(toRational sectionsDone)/(toRational sectionsTotal)
    let sectionsPercent = show $ toInteger $ floor tmp
    defaultLayout $(widgetFile "analysis")
--    where 
--        convertFromPersistent [] = Nothing
--        convertFromPersistent [PersistInt64 sum,PersistInt64 category] = do
--            putStrLn (pack (show (sum)))
--            return (Just (sum,category))
--        convertFromPersistent _ = Nothing

