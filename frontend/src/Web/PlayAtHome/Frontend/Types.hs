module Web.PlayAtHome.Frontend.Types where
import Web.PlayAtHome.Types

data FrontendState
  = Unauthorized
  | Authorized UserId
  | InRoom UserId RoomId RoomInfo
  deriving (Read, Show, Eq, Ord)
