{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import XMonad
import XMonad.Layout.Circle
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.Script
import XMonad.Layout.OneBig
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import qualified XMonad.StackSet as W
import Data.Map (fromList)
import Data.Monoid (mappend)
import Control.Monad (ap)
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout
import XMonad.Layout.BoringWindows
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Util.EZConfig


myLayout = windowNavigation
    $ smartBorders
    $ onWorkspace "1:browsing" (OneBig (15/16) (15/16))
    $ Dishes 2 3 (1/7) ||| Full
  where
    -- one master window, 45% of the screen, with resize disallowed
    tiled = Tall 1 (5/100) (7/16)

main = xmonad $ defaultConfig {
  workspaces = ["1:browsing", "2", "3", "4", "5", "6", "7", "8", "9"],
  logHook = fadeInactiveLogHook 0.8,
  borderWidth = 2,
  normalBorderColor = "#000000",
  focusedBorderColor = "#00FF00",
  focusFollowsMouse = False,
  modMask = mod4Mask,
  layoutHook = myLayout
} `removeKeys` removedKeys `additionalKeysP` specialKeys `additionalKeys` normalKeys
  where
    removedKeys =
      [ (mod4Mask .|. shiftMask, xK_m)
      , (mod4Mask, xK_m)
      , (mod4Mask, xK_Tab)
      , (mod4Mask .|. shiftMask, xK_Tab)
      , (mod4Mask, xK_c)
      , (mod4Mask, xK_g)
      ]
    specialKeys =
      [ ("<XF86MonBrightnessUp>",   spawn "sudo brightness up")
      , ("<XF86MonBrightnessDown>", spawn "sudo brightness down")
      , ("<XF86AudioRaiseVolume>",  spawn "volume up")
      , ("<XF86AudioLowerVolume>",  spawn "volume down")
      , ("<XF86AudioMute>",         spawn "volume 0") ]
    normalKeys =
      [ ((controlMask, xK_space), spawn "battery")
      , ((mod4Mask .|. mod1Mask, xK_Delete), spawn "xscreensaver-command -lock")

      -- (For Dvorak) change focus up/down with the right hand middle fingers
      -- move a window up or down in prority with shift + middle fingers
      , ((mod4Mask, xK_t), windows W.focusUp)
      , ((mod4Mask .|. shiftMask, xK_t), windows W.swapUp)
      , ((mod4Mask, xK_n), windows W.focusDown)
      , ((mod4Mask .|. shiftMask, xK_n), windows W.swapDown)

      -- (For Dvorak) change workspaces left/right with the right hand outside fingers
      -- move a window to a left or right workspace with shiftMask (and follow it)
      , ((mod4Mask, xK_h), prevWS)
      , ((mod4Mask .|. shiftMask, xK_h), shiftToPrev *> prevWS)
      , ((mod4Mask, xK_s), nextWS)
      , ((mod4Mask .|. shiftMask, xK_s), shiftToNext *> nextWS)

      -- Jump to the master, or set the current focus to the master
      , ((mod4Mask .|. shiftMask, xK_m), windows W.swapMaster)
      , ((mod4Mask, xK_m), windows W.focusMaster)
      ]


-- Altered version of Dishes that allows more than one dish per stack,
-- and splits stacks horizontally.  It also accepts a second
-- parameter for how many horizontal dishes can be in a stack
data Dishes a = Dishes Int Int Rational deriving (Show, Read)
instance LayoutClass Dishes a where
    pureLayout (Dishes nmaster dishesPerStack h) r =
        ap zip (dishes h r nmaster dishesPerStack . length) . W.integrate
    pureMessage (Dishes nmaster dishesPerStack h) m = fmap incmastern (fromMessage m)
        where incmastern (IncMasterN d) = Dishes (max 0 (nmaster+d)) dishesPerStack h

dishes :: Rational -> Rectangle -> Int -> Int -> Int -> [Rectangle]
dishes h s nmaster dishesPerStack n = if n <= nmaster
                        then splitHorizontally n s
                        else ws
 where
    (filledDishStackCount, remainder) =
      (n - nmaster) `quotRem` dishesPerStack

    (firstDepth, dishStackCount) =
      if remainder == 0 then
        (dishesPerStack, filledDishStackCount)
      else
        (remainder, filledDishStackCount + 1)

    (masterRect, dishesRect) =
      splitVerticallyBy (1 - (fromIntegral dishStackCount) * h) s

    dishStackRects =
      splitVertically dishStackCount dishesRect

    allDishRects = case dishStackRects of
      (firstStack:bottomDishStacks) ->
        splitHorizontally firstDepth firstStack ++ (bottomDishStacks >>= splitHorizontally dishesPerStack)
      [] -> []

    ws =
      splitHorizontally nmaster masterRect ++ allDishRects
