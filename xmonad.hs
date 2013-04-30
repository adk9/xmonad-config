import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS
import XMonad.Actions.Plane
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig (additionalKeys,additionalKeysP)

import qualified XMonad.StackSet          as W
import qualified Data.Map                 as M
import qualified DBus                     as D
import qualified DBus.Client              as D
import qualified Codec.Binary.UTF8.String as UTF8

myTerminal = "/usr/bin/gnome-terminal"
myWorkspaces = ["1", "2", "3", "4", "5", "6"]

myStartupHook = composeAll [
    setWMName "LG3D",
    startupHook gnomeConfig
    ]

myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "3"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "Do"             --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "Google-chrome"  --> doShift "3"
    , resource  =? "emacs"          --> doShift "2"
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , resource  =? "skype"          --> doFloat
    , className =? "Xchat"          --> doShift "4"
    , className =? "Empathy"        --> doShift "4"
    , className =? "Pidgin"         --> doShift "4"
    , className =? "Thunderbird"    --> doShift "4"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    , manageDocks
    , manageHook gnomeConfig ]

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}
      
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

keysToAdd x = [ -- Gnome close window
                -- ,  ((modMask x, xK_F4), kill)
                -- Shift to previous workspace
                  (((modMask x .|. controlMask), xK_Left), prevWS)
                -- Shift to next workspace
                ,  (((modMask x .|. controlMask), xK_Right), nextWS)
                -- Shift window to previous workspace
                ,  (((modMask x .|. shiftMask), xK_Left), shiftToPrev)
                -- Shift window to next workspace
                ,  (((modMask x .|. shiftMask), xK_Right), shiftToNext)
                -- Gnome Do
                ,  ((modMask x, xK_p), spawn "gnome-do")
                -- Restart xmonad
                ,  ((modMask x, xK_z), restart "xmonad" True)
              ]

-- remove some of the default key bindings
keysToRemove x =
    [ (modMask x, xK_p)
    , (modMask x, xK_q)
    ]

defKeys    = keys gnomeConfig
delKeys x  = foldr M.delete           (defKeys x) (keysToRemove x)
myKeys x   = foldr (uncurry M.insert) (delKeys x) (keysToAdd    x)
 
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ gnomeConfig {
    terminal           = myTerminal,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
    mouseBindings      = myMouseBindings <+> mouseBindings gnomeConfig,
    logHook            = composeAll [
      dynamicLogWithPP (prettyPrinter dbus),
      logHook  gnomeConfig
      ],
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook
    }
    `additionalKeysP` 
        [
          ("M-S-q", spawn "gnome-session-quit")
        ]
    `additionalKeys`
        M.toList (planeKeys (mod1Mask .|. controlMask) GConf Finite)
