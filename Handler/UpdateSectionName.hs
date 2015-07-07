module Handler.UpdateSectionName where

import Import
import Handler.UpdateSection

getUpdateSectionNameR :: Int -> Text -> Handler RepPlain
getUpdateSectionNameR pSectionId pSectionName = getUpdateSectionR pSectionId pSectionName ""
