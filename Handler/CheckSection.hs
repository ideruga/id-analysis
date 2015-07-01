module Handler.CheckSection where

import Import
import Data.Aeson (Value, encode, object, (.=))
import Database.Persist
import Database.Persist.Sql
import Data.Aeson.Parser       (json)
import Data.Conduit            (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl,
                                          requestBody, withManager)


getCheckSectionR :: Int -> Handler RepPlain
getCheckSectionR sectionIdD = do
    mauth <- maybeAuth
    let emailAddr = case mauth of Nothing -> error "can't serve unauthenticated user"
                                  Just (Entity _ user) -> userIdent user 
    runDB $ deleteWhere [DoneSectionsSectionId ==. (toSqlKey $ fromIntegral sectionIdD), DoneSectionsUserIdent ==. emailAddr]
    runDB $ insert $ DoneSections (toSqlKey $ fromIntegral sectionIdD) emailAddr
    return $ RepPlain . toContent $ show $ encode $ Reply {newRecordId = 1}

data Reply = Reply
    { newRecordId :: Int} deriving Show
instance ToJSON Reply where
    toJSON (Reply newRecordId) = object ["recordId" .= newRecordId] 
