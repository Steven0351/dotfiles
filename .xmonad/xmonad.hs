{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Endo)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D (Next, Prev), WSType (WSIs), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.WithAll (killAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh) -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Layout.Renamed (renamed, pattern Replace)
import XMonad.Layout.ResizableTile (MirrorResize (MirrorExpand, MirrorShrink), pattern ResizableTall)
import XMonad.Layout.ShowWName (SWNConfig, showWName', swn_bgcolor, swn_color, swn_fade, swn_font)
import XMonad.Layout.Simplest (pattern Simplest)
import XMonad.Layout.Spacing (Spacing (..), decScreenSpacing, decWindowSpacing, incScreenSpacing, incWindowSpacing, spacingRaw, pattern Border)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.ThreeColumns (pattern ThreeColMid)
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg, windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows/cmd key

myTerminal :: String
myTerminal = "kitty -e /usr/bin/fish"

myBrowser :: String
myBrowser = "qutebrowser"

myBorderWidth :: Dimension
myBorderWidth = 2 -- Border width for windows

myNormColor :: String
myNormColor = "#4C566A" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#A3BE8C" -- Border color of focused windows

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom &"
  spawnOnce "~/.fehbg &" -- set last saved feh wallpaper
  --setWMName "LG3D" -- This is apparently required for Java GUI apps. God help us all. Leaving here for future me just in case

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Layouts I like to use
tall =
  renamed [Replace "tall"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          limitWindows 12 $
            mySpacing 8 $
              ResizableTall 1 (3 / 100) (1 / 2) []

threeCol =
  renamed [Replace "three col mid"] $
    smartBorders $
      windowNavigation $
        subLayout [] (smartBorders Simplest) $
          mySpacing 8 $
            limitWindows 7 $
              ThreeColMid 1 (3 / 100) (1 / 2)

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = "xft:Fira:bold:size=60",
      swn_fade = 1.0,
      swn_bgcolor = "#4c566a",
      swn_color = "#eceff4"
    }

-- The layout hook
myLayoutHook =
  avoidStruts $
    windowArrange $
      T.toggleLayouts tall $
        mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
      withBorder myBorderWidth tall
        ||| withBorder myBorderWidth threeCol

myWorkspaces = [" dev ", " web ", " doc ", " chat "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
    -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
    -- I'm doing it this way because otherwise I would have to write out the full
    -- name of my workspaces and the names would be very long if using clickable workspaces.
    [ className =? "confirm" --> doFloat,
      className =? "file_progress" --> doFloat,
      className =? "dialog" --> doFloat,
      className =? "download" --> doFloat,
      className =? "error" --> doFloat,
      className =? "notification" --> doFloat,
      className =? "pinentry-gtk-2" --> doFloat,
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat,
      title =? "Visual Studio Code" --> doShift (myWorkspaces !! 0),
      title =? "Mozilla Firefox" --> doShift (myWorkspaces !! 1),
      className =? "qutebrowser" --> doShift (myWorkspaces !! 1),
      title =? "Signal" --> doShift (myWorkspaces !! 3),
      (className =? "firefox" <&&> resource =? "Dialog") --> doFloat, -- Float Firefox Dialog
      isFullscreen --> doFullFloat
    ]

-- Key Binding Helpers
type KeyBinding = ((KeyMask, KeySym), X ())

keyBinding :: KeyMask -> Maybe KeyMask -> KeySym -> X () -> KeyBinding
keyBinding modifier Nothing keySym command = ((modifier, keySym), command)
keyBinding modifier (Just other) keySym command = ((modifier .|. other, keySym), command)

mod' :: KeySym -> X () -> KeyBinding
mod' = keyBinding myModMask Nothing

mod'' :: KeyMask -> KeySym -> X () -> KeyBinding
mod'' = keyBinding myModMask . Just

modShift :: KeySym -> X () -> KeyBinding
modShift = mod'' shiftMask

modControl :: KeySym -> X () -> KeyBinding
modControl = mod'' controlMask

control :: KeySym -> X () -> KeyBinding
control = keyBinding controlMask Nothing

control' :: KeyMask -> KeySym -> X () -> KeyBinding
control' = keyBinding controlMask . Just

myWorkspaceKeySyms :: [(WorkspaceId, KeySym)]
myWorkspaceKeySyms = zip myWorkspaces [xK_1 ..]

-- When applied with a KeySym and WorkspaceId this shifts the
-- currently focused window to the provided workspace.
shiftWindowKeyBinding :: KeySym -> WorkspaceId -> KeyBinding
shiftWindowKeyBinding keySym id = modControl keySym $ windows $ W.shift id

-- Since myWorkspaceKeySyms has to be in the order of (WorkspaceId, KeySym)
-- and not (KeySym, WorkspaceId), we flip the args of shiftWindowKeyBinding,
-- then we uncurry it so we can just apply the tuple directly. Finally we apply
-- that to map so it can operate on [(WorkspaceId, KeySym)].
shiftWindowKeyBindings :: [(WorkspaceId, KeySym)] -> [KeyBinding]
shiftWindowKeyBindings = map $ uncurry $ flip shiftWindowKeyBinding

shiftWindowKeyBindings' :: [KeyBinding]
shiftWindowKeyBindings' =
  shiftWindowKeyBindings myWorkspaceKeySyms

myKeys :: [KeyBinding]
myKeys =
  -- Xmonad
  [ modControl xK_r $ spawn "xmonad --recompile",
    modShift xK_r $ spawn "xmonad --restart",
    control' mod1Mask xK_q $ io exitSuccess, -- quits Xmonad
    -- Run Prompt
    modShift xK_Return $ spawn "rofi -show run",
    -- Commonly used programs
    mod' xK_Return $ spawn myTerminal,
    mod' xK_b $ spawn myBrowser,
    -- Kill Windows
    modShift xK_c kill1, -- Kills focused window
    modShift xK_a killAll, -- Kills all windows in workspace
    -- Workspaces
    mod' xK_period nextScreen, -- Switch focus to next monitor
    mod' xK_comma prevScreen, -- Switch focus to previous monitor
    control' mod1Mask xK_j $ decWindowSpacing 4, -- Decrease window gaps by 4
    control' mod1Mask xK_k $ incWindowSpacing 4, -- Increase window gaps by 4
    control' mod1Mask xK_h $ decScreenSpacing 4,
    control' mod1Mask xK_l $ incScreenSpacing 4,
    mod' xK_m $ windows W.focusMaster, -- Shift focus to master
    mod' xK_j $ windows W.focusDown, -- Toggle focus down (also clockwise in tall)
    mod' xK_k $ windows W.focusUp, -- Toggle focus up (also counter-clockwise in tall)
    modShift xK_m $ windows W.swapMaster,
    modShift xK_j $ windows W.swapDown,
    modShift xK_k $ windows W.swapUp,
    mod' xK_BackSpace promote, -- Makes focused window master
    modShift xK_Tab rotSlavesDown, -- Rotates all windows except master
    modControl xK_Tab rotAllDown,
    control xK_Tab $ sendMessage NextLayout, -- Toggles next layout
    control xK_h $ sendMessage Shrink,
    mod' xK_l $ sendMessage Expand,
    mod'' mod1Mask xK_j $ sendMessage MirrorShrink,
    mod'' mod1Mask xK_k $ sendMessage MirrorExpand
  ]
    ++ shiftWindowKeyBindings'

main :: IO ()
main = do
  -- Launching three instances of xmobar on their monitors.
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0"
  -- the xmonad, ya know...what the WM is named after!
  xmonad $
    ewmh
      def
        { manageHook = myManageHook <+> manageDocks,
          handleEventHook = docksEventHook,
          modMask = myModMask,
          terminal = myTerminal,
          startupHook = myStartupHook,
          layoutHook = showWName' myShowWNameTheme $ myLayoutHook,
          workspaces = myWorkspaces,
          borderWidth = myBorderWidth,
          normalBorderColor = myNormColor,
          focusedBorderColor = myFocusColor,
          logHook =
            dynamicLogWithPP $
              xmobarPP
                { ppOutput = \x -> hPutStrLn xmproc0 x,
                  ppCurrent = xmobarColor "#A3BE8C" "", -- Current workspace
                  ppVisible = xmobarColor "#A3BE8C" "" . clickable, -- Visible but not current workspace
                  ppHidden = xmobarColor "#5E81AC" "" . clickable, -- Hidden workspaces
                  ppHiddenNoWindows = xmobarColor "#D08770" "" . clickable, -- Hidden workspaces (no windows)
                  ppTitle = xmobarColor "#D8DEE9" "" . shorten 60, -- Title of active window
                  ppSep = "<fc=#4C566A> <fn=1>|</fn> </fc>", -- Separator character
                  ppUrgent = xmobarColor "#BF616A" "" . wrap "!" "!", -- Urgent workspace
                  ppOrder = \(ws : l : t : _) -> [ws, l, t] -- order of things in xmobar
                }
        }
      `additionalKeys` myKeys
