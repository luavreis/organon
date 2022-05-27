-- |

module Site.Roam.OrgAttach where
import Org.Types
import Org.Walk

getAttachment :: OrgInline -> Set FilePath
getAttachment (Link (URILink "attachment" att) _) = one (toString att)
getAttachment _ = mempty

queryAttachments :: Walkable OrgInline a => a -> Set FilePath
queryAttachments = query getAttachment
