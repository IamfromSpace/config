{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad (ap)
import qualified Data.List as List
import Data.Map (fromList)
import Data.Monoid (mappend)
import System.IO
import XMonad
import XMonad.Actions.CycleWS
       (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Script
import XMonad.Layout
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BoringWindows
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import qualified XMonad.StackSet as Set
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

myLayout =
    avoidStruts $
    windowNavigation $
    smartBorders $
    onWorkspace "5:browsing" (OneBig (15 / 16) (15 / 16)) $
    Dishes 2 3 (1 / 7) ||| Full
    -- one master window, 45% of the screen, with resize disallowed
  where
    tiled = Tall 1 (5 / 100) (7 / 16)

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $
      docks $
        withNavigation2DConfig def $
        def
        { workspaces = ["1:browsing", "2", "3", "4", "5", "6", "7", "8", "9"]
        , logHook =
              fadeInactiveLogHook 0.8 <+>
              dynamicLogWithPP
                  xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
        , borderWidth = 2
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#00FF00"
        , focusFollowsMouse = False
        , modMask = mod4Mask
        , layoutHook = myLayout
        , terminal = "alacritty"
        } `removeKeys`
        removedKeys `additionalKeysP`
        specialKeys `additionalKeys`
        normalKeys
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
        [ ("<XF86MonBrightnessUp>", spawn "sudo brightness up")
        , ("<XF86MonBrightnessDown>", spawn "sudo brightness down")
        , ("<XF86AudioRaiseVolume>", spawn "volume up")
        , ("<XF86AudioLowerVolume>", spawn "volume down")
        , ("<XF86AudioMute>", spawn "volume 0")
        ]
    normalKeys =
        [ ((controlMask, xK_space), spawn "battery")
        , ( (mod4Mask .|. mod1Mask, xK_Delete)
          , spawn "xscreensaver-command -lock")
        , ( (mod4Mask, xK_p), spawn "rofi -modi drun,ssh,window -show drun -show-icons")
      -- (For Dvorak) change window focus 2D
        , ((mod4Mask, xK_h), windowGo L False)
        , ((mod4Mask, xK_t), windowGo U False)
        , ((mod4Mask, xK_n), windowGo D False)
        , ((mod4Mask, xK_s), windowGo R False)
      -- (For Dvorak) swap windows 2D
        , ((mod4Mask .|. controlMask, xK_h), windowSwap L False)
        , ((mod4Mask .|. controlMask, xK_t), windowSwap U False)
        , ((mod4Mask .|. controlMask, xK_n), windowSwap D False)
        , ((mod4Mask .|. controlMask, xK_s), windowSwap R False)
      -- (For Dvorak) change workspace focus 2D
        , ((mod4Mask .|. shiftMask, xK_h), doWsNav2D L)
        , ((mod4Mask .|. shiftMask, xK_t), doWsNav2D U)
        , ((mod4Mask .|. shiftMask, xK_n), doWsNav2D D)
        , ((mod4Mask .|. shiftMask, xK_s), doWsNav2D R)
      -- (For Dvorak) move focused window to workspace in 2D dir, and follow it
        , ( (mod4Mask .|. shiftMask .|. controlMask, xK_h)
          , doWsShift2D L *> doWsNav2D L)
        , ( (mod4Mask .|. shiftMask .|. controlMask, xK_t)
          , doWsShift2D U *> doWsNav2D U)
        , ( (mod4Mask .|. shiftMask .|. controlMask, xK_n)
          , doWsShift2D D *> doWsNav2D D)
        , ( (mod4Mask .|. shiftMask .|. controlMask, xK_s)
          , doWsShift2D R *> doWsNav2D R)
      -- Jump to the master, or set the current focus to the master
        , ((mod4Mask .|. shiftMask, xK_m), windows W.swapMaster)
        , ((mod4Mask, xK_m), windows W.focusMaster)
        ]

-- Experimental WIP layout that is based on preserving a target aspect ratio
-- where possible for two or more windows.  An example might be for YouTube
-- broadcasting.  You want to have two screens that are both 16:9, one for
-- viewing the feed (which is 16:9) and one to broadcast to the feed,
-- (which is also 16:9).  You may want one to be larger than the other, but
-- the aspect ratio should always be preserved.  As of right now, you get
-- the excess rects (assuming that there even is any remainder), but that's it,
-- the other 5th+ windows are hidden.
data Aspect a =
    Aspect
    deriving (Show, Read)

instance LayoutClass Aspect a where
    pureLayout _ r = ap zip (aspect r . length) . W.integrate
    pureMessage x m = Nothing

aspect :: Rectangle -> Int -> [Rectangle]
aspect r n =
    let aspect = fromIntegral (rect_width r) / fromIntegral (rect_height r)
        topCut = 2 / 5
        target = 16 / 9
        cutPoint = (1 / aspect) * target
    in if n <= 0
           then [r]
           else let (top, bottom) = splitVerticallyBy topCut r
                    (tr, tl) = splitHorizontallyBy (1 - cutPoint * topCut) top
                    (br, bl) =
                        splitHorizontallyBy (cutPoint * (1 - topCut)) bottom
                in [br, tl, bl, tr]

-- Altered version of Dishes that allows more than one dish per stack,
-- and splits stacks horizontally.  It also accepts a second
-- parameter for how many horizontal dishes can be in a stack
data Dishes a =
    Dishes Int
           Int
           Rational
    deriving (Show, Read)

instance LayoutClass Dishes a where
    pureLayout (Dishes nmaster dishesPerStack h) r =
        ap zip (dishes h r nmaster dishesPerStack . length) . W.integrate
    pureMessage (Dishes nmaster dishesPerStack h) m =
        fmap incmastern (fromMessage m)
      where
        incmastern (IncMasterN d) =
            Dishes (max 0 (nmaster + d)) dishesPerStack h

dishes :: Rational -> Rectangle -> Int -> Int -> Int -> [Rectangle]
dishes h s nmaster dishesPerStack n =
    if n <= nmaster
        then splitHorizontally n s
        else ws
  where
    (filledDishStackCount, remainder) = (n - nmaster) `quotRem` dishesPerStack
    (firstDepth, dishStackCount) =
        if remainder == 0
            then (dishesPerStack, filledDishStackCount)
            else (remainder, filledDishStackCount + 1)
    (masterRect, dishesRect) =
        splitVerticallyBy (1 - fromIntegral dishStackCount * h) s
    dishStackRects = splitVertically dishStackCount dishesRect
    allDishRects =
        case dishStackRects of
            (firstStack:bottomDishStacks) ->
                splitHorizontally firstDepth firstStack ++
                (bottomDishStacks >>= splitHorizontally dishesPerStack)
            [] -> []
    ws = splitHorizontally nmaster masterRect ++ allDishRects

doWsNav2D :: Direction2D -> X ()
doWsNav2D = doWs2D Set.view

doWsShift2D :: Direction2D -> X ()
doWsShift2D = doWs2D Set.shift

{-
doWs2D :: this type def is really ugly, so I'm letting the inference handle this one... -}
doWs2D op dir = fmap (workspaces . config) ask >>= (windows . ws2D op dir)

ws2D ::
       (Ord a, Eq i, Eq s)
    => (i -> Set.StackSet i l a s sd -> Set.StackSet i l a s sd)
    -> Direction2D
    -> [i]
    -> Set.StackSet i l a s sd
    -> Set.StackSet i l a s sd
ws2D op dir orderedWsTags set =
    let clamp a b = max a . min b
        maybeIndex = List.elemIndex (Set.currentTag set) orderedWsTags
        getNewIndex i =
            let (q, r) = (i `quotRem` 3)
                (q2, r2) =
                    case dir of
                        U -> (q + 1, r)
                        D -> (q - 1, r)
                        R -> (q, r + 1)
                        L -> (q, r - 1)
            in clamp 0 2 q2 * 3 + clamp 0 2 r2
    -- Note that (!!) isn't total, but it _should_ be impossible
    -- to try to retrieve an index that doesn't exist, that would
    -- indicate a bug.
    in case fmap ((!!) orderedWsTags . getNewIndex) maybeIndex of
           Just i -> op i set
           Nothing -> set
