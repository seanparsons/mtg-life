{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Map as M
import GHC.Generics
import Miso
import Miso.String (MisoString)
import qualified Miso.String as S

default (MisoString)

data PlayerModel = PlayerModel
  { _life    :: Int
  } deriving (Eq, Show, Generic)

makeLenses ''PlayerModel

data Model = Model
  { _player1 :: PlayerModel
  , _player2 :: PlayerModel
  } deriving (Eq, Show, Generic)

makeLenses ''Model

startingModel :: Model
startingModel = Model
  { _player1 = PlayerModel 20
  , _player2 = PlayerModel 20
  }

data PlayerUpdate = LifeChange Int deriving (Eq, Show, Generic)

data Message = NoOp
             | Player1Update PlayerUpdate
             | Player2Update PlayerUpdate
             deriving (Eq, Show, Generic)

main :: IO ()
main =
  let initialAction = NoOp
      model         = startingModel
      update        = updateModel
      view          = viewModel
      events        = defaultEvents
      mountPoint    = Nothing
      subs          = []
  in  startApp App {..}

updateModel :: Message -> Model -> Effect Message Model
updateModel NoOp m = noEff m
updateModel (Player1Update (LifeChange lifeChange)) m = noEff $ over (player1 . life) (+ lifeChange) m
updateModel (Player2Update (LifeChange lifeChange)) m = noEff $ over (player2 . life) (+ lifeChange) m

scoreChangeElement :: MisoString -> Int -> (PlayerUpdate -> Message) -> View Message
scoreChangeElement label scoreChange playerUpdateMessage =
  div_
    [ style_ $ M.fromList [("width", "20%"), ("margin", "auto")]
    , onClick $ playerUpdateMessage (LifeChange scoreChange)
    ]
    [ text label ]

viewPlayer :: PlayerModel -> (PlayerUpdate -> Message) -> View Message
viewPlayer m@PlayerModel{..} playerUpdateMessage =
  div_
    [ style_ $ M.fromList [("width", "50%"), ("height", "100%"), ("display", "flex")] ]
    [ scoreChangeElement "-" (-1) playerUpdateMessage 
    , div_
      [ style_ $ M.fromList [("width", "20%"), ("margin", "auto")] ]
      [ text $ S.pack $ show _life ]
    , scoreChangeElement "+" (1) playerUpdateMessage 
    ]

viewModel :: Model -> View Message
viewModel m@Model{..} =
  div_
    [ style_ $ M.fromList [("display", "flex"), ("flex-direction", "row"), ("height", "100vh;") ]
    ]
    [ viewPlayer _player1 Player1Update
    , viewPlayer _player2 Player2Update
    ]
