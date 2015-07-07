module Handler.UpdateSection where

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


getUpdateSectionR :: Int -> Text -> Text -> Handler RepPlain
getUpdateSectionR pSectionId pSectionName pSectionDescription = do
    mauth <- maybeAuth
    let emailAddr = case mauth of Nothing -> "nothing"
                                  Just (Entity _ user) -> userIdent user 

    runDB $ updateWhere [SectionId ==. (toSqlKey (fromIntegral pSectionId))] [SectionName =. Just pSectionName, SectionDescription =. Just pSectionDescription]
    return $ RepPlain "ok"
