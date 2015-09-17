{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Main where

import Brick
import Brick.Widgets.List
import Data.Functor
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

header :: Widget
header = vBox $ map txt
  [ "Press 'q' to quit."
  , "Try scrolling beyond what's currently visible."
  , "The list doesn't scroll until you've gone a bit off-screen."
  , "The problem seems to be caused by this header."
  ]

type State = List Int

initState :: State
initState = list "list" [0 .. 1000] 1

attrs :: AttrMap
attrs = attrMap defAttr [(listSelectedAttr, defAttr `withStyle` reverseVideo)]

draw :: State -> [Widget]
draw st = [header <=> renderList st (const $ str . show)]

handler :: State -> Event -> EventM (Next State)
handler st (EvKey (KChar 'q') []) = halt st
handler st e = handleEvent e st >>= continue

app :: App State Event
app =
  App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handler
  , appStartEvent = return
  , appAttrMap = const attrs
  , appLiftVtyEvent = id
  }

main :: IO ()
main = void $ defaultMain app initState
