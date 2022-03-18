{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.X11.ExtraTypes.XF86
import XMonad hiding (Color)
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP hiding (wrap, shorten)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..), MirrorResize(..))
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed (tabbedAlways, shrinkText, Theme(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import System.Exit
import System.IO

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Printf
import Data.Text.Format (Only(..))
import qualified Data.Text.Format as F

import qualified XMonad.DBus as D
import qualified DBus.Client as DC

type Color = T.Text

wrap :: T.Text -> T.Text -> T.Text -> T.Text
wrap left right middle = left <> middle <> right

pad :: T.Text -> T.Text
pad = wrap " " " "

shorten :: Int -> T.Text -> T.Text
shorten maxlen text = case text `T.compareLength` maxlen of
   GT -> T.snoc (T.take maxlen text) ellipsis
   otherwise -> text
   where ellipsis = '…'

format fmt = TL.toStrict . F.format fmt
format1 str item = format str (Only item)

data MouseButton
  = LeftClick
  | MiddleClick
  | RightClick
  | ScrollUp
  | ScrollDown
  | DoubleLeftClick
  | DoubleMiddleClick
  | DoubleRightClick
  deriving (Eq, Ord, Show, Enum)

fromMouseButton :: MouseButton -> Int
fromMouseButton = succ . fromEnum

data LemonbarFormatting
  = Foreground Color
  | Background Color
  | Reverse
  | Underline Color
  | Overline Color
  | Font Int
  | Offset Int
  | Action MouseButton T.Text
  deriving (Eq, Show)

lemonbarFormatOne :: LemonbarFormatting -> T.Text -> T.Text
lemonbarFormatOne fmt = case fmt of
  (Foreground color)      -> wrap (bracket $ format1 "F{}" color) (bracket "F-")
  (Background color)      -> wrap (bracket $ format1 "B{}" color) (bracket "B-")
  (Reverse)               -> wrap (bracket "R") (bracket "R")
  (Underline color)       -> wrap (bracket (format1 "u{}" color) <> bracket "+u") (bracket "-u")
  (Overline color)        -> wrap (bracket (format1 "o{}" color) <> bracket "+o") (bracket "-o")
  (Font index)            -> wrap (bracket (format1 "T{}" index)) (bracket "T-")
  (Offset size)           -> (bracket (format1 "O{}" size) <>)
  (Action button cmd)     -> wrap (bracket (format "A{}:{}:" (fromMouseButton button, (escape ':' cmd))))
                                  (bracket "A")
  where
    bracket = wrap "%{" "}"
    escape char =
      let charT = T.singleton char in
      T.replace charT (T.cons '\\' charT)

lemonbarFormat :: [LemonbarFormatting] -> T.Text -> T.Text
lemonbarFormat fmts = foldr (.) id (lemonbarFormatOne <$> fmts)

windowTitleColor = "#F0C674"
activeWorkspaceColor = "#F0C674"
inactiveWorkspaceColor = "#707880"

convertText :: (T.Text -> T.Text) -> String -> String
convertText f s1 = T.unpack (f (T.pack s1))

-- Override the PP values as you would like (see XMonad.Hooks.StatusBar.PP documentation)
myLogHook :: DC.Client -> PP
myLogHook dbus = def {
    ppOutput = D.send dbus
    , ppTitle = convertText (lemonbarFormat [ Foreground windowTitleColor ] . shorten 50)
    , ppSep = " | "
    , ppCurrent = convertText (lemonbarFormat [ Foreground activeWorkspaceColor ] . wrap "[" "]")
    , ppHidden = convertText (lemonbarFormat [ Foreground inactiveWorkspaceColor ])
}

---------------------------------------------------------------------------------------------------
-- Layouts
myLayouts = toggleLayouts (noBorders Full) (myTiled ||| myTabbed ||| noBorders Full)
    where
        myTiled = spacingWithEdge 4 $ smartBorders $ ResizableTall 1 (5/100) (3/5) []
        myTabbed = noBorders $ tabbedAlways shrinkText myTabConf

myBorderColor = "#F0C674"

myTabConf = def
    { fontName = "xft:FiraCode Nerd Font:size=7:antialias=true"
    , activeColor         = "#282A2E"
    , activeBorderColor   = myBorderColor
    , activeTextColor     = "#C5C8C6"
    , inactiveColor       = "#282A2E" -- "#496749"
    , inactiveBorderColor = "#333333"
    , inactiveTextColor   = "#C5C8C6"
    , urgentColor         = "#900000"
    , urgentBorderColor   = "#2f343a"
    , urgentTextColor     = "#ffffff"
    }

myExitPromp = def {
    font = "xft:FiraCode Nerd Font:size=10:antialias=true"
    , position = CenteredAt { xpCenterY = 0.5, xpWidth = 0.5 }
}

myManageHook = composeAll
   [ className =? "Pavucontrol"  --> doCenterFloat
   , className =? "Nm-openconnect-auth-dialog"  --> doCenterFloat
   , manageDocks
   ]

main :: IO ()
main = do
    -- Connect to DBus
    dbus <- D.connect
    -- Request access (needed when sending messages)
    D.requestAccess dbus    

    xmonad $ ewmh $ docks def
        { startupHook = setDefaultCursor xC_left_ptr <+> startupHook def
        , manageHook = myManageHook <+> manageHook def
        , layoutHook = smartBorders $ desktopLayoutModifiers $ myLayouts
        , logHook = dynamicLogWithPP (myLogHook dbus)
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "alacritty"
        , focusedBorderColor = myBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "loginctl lock-session")
        , ((0, xK_Print), spawn "gnome-screenshot --interactive")
        , ((mod4Mask, xK_f), sendMessage (Toggle "Full"))
        , ((mod4Mask, xK_d), spawn "rofi -show drun -show-icons")
        , ((mod4Mask, xK_k), spawn "toggle-keyboard-layout")
        , ((mod4Mask, xK_y), spawn "emoji")
        , ((mod4Mask, xK_x), spawn "screenlayout")
        , ((mod4Mask, xK_grave), spawn "dunstctl action")
        , ((mod4Mask .|. shiftMask, xK_grave), spawn "dunstctl close")
        , ((mod4Mask .|. controlMask, xK_grave), spawn "dunstctl history-pop")
        , ((mod4Mask, xK_m), spawn "pavucontrol")
        , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
        , ((mod4Mask, xK_n), spawn "alacritty -e ncmpc")
        , ((mod4Mask .|. shiftMask, xK_b), spawn "toggle-headphones")
        , ((mod4Mask .|. shiftMask, xK_q), kill)
        , ((mod4Mask .|. shiftMask, xK_e), confirmPrompt myExitPromp "exit" $ io (exitWith ExitSuccess))
        ] `removeKeys` 
        [ (mod4Mask .|. shiftMask, xK_c) -- old close
        ]
