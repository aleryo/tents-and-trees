{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Client
import           Servant.Server
import           Test.Hspec

main :: IO ()
main = hspec spec

-- * Types for Game

-- | Content of single Cell
data Cell = Tree | Empty
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Constraints definitions for tents placement
data Board = Board { columns :: [ Int ]
                   , cells   :: [ (Int, [ Cell ]) ]
                   }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Current state of the Game
data Game = Game { board :: Board }
          deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Play = Play { what     :: Cell
                 , position :: (Int, Int)
                 }
          deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Api type
type TentsAndTreesAPI = "play" :> Capture "playername" Text :> Get '[JSON] Game
                   :<|> "play" :> Capture "playername" Text :> ReqBody '[JSON] Play :> Post '[JSON] Game

tentsAndTreesAPI :: Proxy TentsAndTreesAPI
tentsAndTreesAPI = Proxy

-- ** Client Definition
playGame :: Text -> ClientM Game
sendPlay :: Text -> Play -> ClientM Game
playGame :<|> sendPlay = client tentsAndTreesAPI


-- ** Server

startServer p = async $ run p (serve tentsAndTreesAPI handlers)
  where
    handlers = newGame :<|> play
    newGame _ = pure $ Game { board = Board { columns =       [ 0    , 1     ]
                                            , cells   = [ (1, [ Tree , Empty ])
                                                        , (0, [ Empty, Tree  ])
                                                        ]
                                            }
                            }
    play _ _ = undefined

stopServer = cancel

gameServer = bracket (startServer 8888) stopServer . const

-- * calcul des contraintes
-- * endpoint pour jouer
-- * rendering HTML
--   * capture souris
spec :: Spec
spec = describe "Tents and Trees" $ do

  around_ gameServer $ describe "Server" $ do

   it "on GET /play provides a new game" $ do
     env <- ClientEnv <$> newManager defaultManagerSettings <*> pure (BaseUrl Http "localhost" 8888 "") <*> pure Nothing

     Right game <- runClientM (playGame "Alice") env

     game `shouldBe` Game { board = Board { columns =       [ 0    , 1     ]
                                          , cells   = [ (1, [ Tree , Empty ])
                                                      , (0, [ Empty, Tree  ])
                                                      ]
                                          }
                          }
