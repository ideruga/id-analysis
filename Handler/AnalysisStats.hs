module Handler.AnalysisStats where

import Import hiding (length,filter)
import Data.Map (Map)
import Data.Maybe
import Database.Persist
import Database.Persist.Sql
import Data.List (length,filter)
import Text.Julius (rawJS)
import qualified Data.Conduit.List as CL
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.BrowserId


getAnalysisStatsR :: Int64 -> Handler Html
getAnalysisStatsR chapterId = do
    mauth <- maybeAuth
    let emailAddr = case mauth of Nothing -> "nothing"
                                  Just (Entity _ user) -> userIdent user 
    $(logInfo) $ "authentication: " ++ (pack (show emailAddr))
    maid <- maybeAuthId

    doneSectionsDB <- runDB $ selectList [DoneSectionsUserIdent ==. emailAddr] []
    let doneSections = map (doneSectionsSectionId . entityVal) doneSectionsDB
    $(logInfo) $ "allSectionsDone: " ++ (pack . show . length) doneSections

    --res <- runDB $ rawQuery "SELECT count(*) cnt, sum(CASE WHEN done THEN 1 ELSE 0 END) sm FROM SECTION" [] $$ CL.map (convertFromPersistent) =$ CL.consume
    allSections <- runDB $ selectList ([] :: [Filter Section]) []
    let allSectionsTotal = length allSections
    $(logInfo) $ "allSectionsTotal value: " ++ (pack (show allSectionsTotal))
    let tmp0 = (toRational 100)*((toRational . length) doneSections)/(toRational allSectionsTotal)
    let allSectionsPercent = show $ toInteger $ floor tmp0


    chapters <- runDB $ selectList ([] :: [Filter Chapter]) []
    sections <- runDB $ selectList [SectionChapterId ==. toSqlKey chapterId] []

    let sectionsTotal = length sections
    $(logInfo) $ "Sectioins in current chapter: " ++ (pack (show sectionsTotal))
    let sectionsDone = length . filter (\ x -> x `elem` doneSections) $ map entityKey sections
    $(logInfo) $ "Greeting from getHomeR: " ++ (pack (show sectionsDone))
    let tmp = (toRational 100)*(toRational sectionsDone)/(toRational sectionsTotal)
    let sectionsPercent = show $ toInteger $ floor tmp
    defaultLayout $(widgetFile "analysis")
--    where 
-- convertFromPersistent [] = Nothing
-- convertFromPersistent [PersistInt64 sum,PersistInt64 category] = do
-- putStrLn (pack (show (sum)))
-- return (Just (sum,category))
-- convertFromPersistent _ = Nothing

