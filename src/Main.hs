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
import qualified Miso.Svg as SVG
import qualified Miso.Svg.Attribute as SVGA
import Miso.String (MisoString)
import qualified Miso.String as S
import MTG.Types

default (MisoString)

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
updateModel Reset _ = noEff startingModel

centerContent :: [(MisoString, MisoString)]
centerContent = [("display", "flex"), ("align-items", "center"), ("justify-content", "center")]

resetIconPath :: MisoString
resetIconPath = "M 25 5 C 14.351563 5 5.632813 13.378906 5.054688 23.890625 C 5.007813 24.609375 5.347656 25.296875 5.949219 25.695313 C 6.550781 26.089844 7.320313 26.132813 7.960938 25.804688 C 8.601563 25.476563 9.019531 24.828125 9.046875 24.109375 C 9.511719 15.675781 16.441406 9 25 9 C 29.585938 9 33.699219 10.925781 36.609375 14 L 34 14 C 33.277344 13.988281 32.609375 14.367188 32.246094 14.992188 C 31.878906 15.613281 31.878906 16.386719 32.246094 17.007813 C 32.609375 17.632813 33.277344 18.011719 34 18 L 40.261719 18 C 40.488281 18.039063 40.71875 18.039063 40.949219 18 L 44 18 L 44 8 C 44.007813 7.460938 43.796875 6.941406 43.414063 6.558594 C 43.03125 6.175781 42.511719 5.964844 41.96875 5.972656 C 40.867188 5.988281 39.984375 6.894531 40 8 L 40 11.777344 C 36.332031 7.621094 30.964844 5 25 5 Z M 43.03125 23.972656 C 41.925781 23.925781 40.996094 24.785156 40.953125 25.890625 C 40.488281 34.324219 33.558594 41 25 41 C 20.414063 41 16.304688 39.074219 13.390625 36 L 16 36 C 16.722656 36.011719 17.390625 35.632813 17.753906 35.007813 C 18.121094 34.386719 18.121094 33.613281 17.753906 32.992188 C 17.390625 32.367188 16.722656 31.988281 16 32 L 9.71875 32 C 9.507813 31.96875 9.296875 31.96875 9.085938 32 L 6 32 L 6 42 C 5.988281 42.722656 6.367188 43.390625 6.992188 43.753906 C 7.613281 44.121094 8.386719 44.121094 9.007813 43.753906 C 9.632813 43.390625 10.011719 42.722656 10 42 L 10 38.222656 C 13.667969 42.378906 19.035156 45 25 45 C 35.648438 45 44.367188 36.621094 44.945313 26.109375 C 44.984375 25.570313 44.800781 25.039063 44.441406 24.636719 C 44.078125 24.234375 43.570313 23.996094 43.03125 23.972656 Z"

scoreChangeElement :: Int -> (PlayerUpdate -> Message) -> View Message
scoreChangeElement scoreChange playerUpdateMessage =
  let minusElements = [ SVG.rect_ [ SVG.x_ "16", SVG.y_ "36", SVG.width_ "48", SVG.height_ "8", SVG.fill_ "black"] [] ]
      plusElements = minusElements ++ [ SVG.rect_ [ SVG.x_ "36", SVG.y_ "16", SVG.width_ "8", SVG.height_ "48", SVG.fill_ "black"] [] ]
      changeElements = if scoreChange >= 0 then plusElements else minusElements
  in  div_
    [ style_ $ M.fromList ([("width", "20%"), ("height", "40%")] ++ centerContent)
    ]
    [ SVG.svg_
      [ SVG.width_ "80", SVG.height_ "80"
      , SVG.onClick $ playerUpdateMessage (LifeChange scoreChange)
      ]
      ([ SVG.circle_ [ SVG.cx_ "40", SVG.cy_ "40", SVG.r_ "35", SVG.stroke_ "black", SVG.strokeWidth_ "8", SVG.fill_ "white"] []
      ] ++ changeElements)
    ]

lifeElement :: Int -> View Message
lifeElement lifeValue =
  div_
    [ style_ $ M.fromList ([("width", "20%"), ("height", "20%"), ("font-size", "80px")] ++ centerContent)
    ]
    [ text $ S.pack $ show lifeValue
    ]

viewPlayer :: PlayerModel -> (PlayerUpdate -> Message) -> View Message
viewPlayer m@PlayerModel{..} playerUpdateMessage =
  div_
    [ style_ $ M.fromList [("width", "50%"), ("height", "100%"), ("display", "flex"), ("flex-direction", "column"), ("align-items", "center")]
    ]
    [ scoreChangeElement (-1) playerUpdateMessage
    , lifeElement _life
    , scoreChangeElement (1) playerUpdateMessage
    ]

resetIcon :: View Message
resetIcon =
  div_
    [ style_ $ M.fromList ([("pointer-events", "none"), ("width", "100%"), ("height", "100%"), ("position", "absolute")] ++ centerContent)
    ]
    [ SVG.svg_
      [ style_ $ M.fromList ([("pointer-events", "auto"), ("width", "100px"), ("height", "100px"), ("position", "absolute")])
      , SVG.onClick Reset
      ]
      [ SVG.path_ [ SVG.d_ resetIconPath, SVG.transform_ "scale(2 2)" ] []
      ]
    ]

viewModel :: Model -> View Message
viewModel m@Model{..} =
  div_
    [ style_ $ M.fromList
      [ ("display", "flex")
      , ("flex-direction", "row")
      , ("height", "100vh")
      , ("width", "100vw")
      , ("user-select", "none")
      ]
    ]
    [ viewPlayer _player1 Player1Update
    , viewPlayer _player2 Player2Update
    , resetIcon
    ]

