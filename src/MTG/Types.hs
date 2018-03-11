{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MTG.Types where

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
             | Reset
             deriving (Eq, Show, Generic)
