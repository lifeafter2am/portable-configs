import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO (Handle, hPutStrLn, hGetContents)
import qualified XMonad.StackSet as W
import Dzen
import XMonad.Hooks.DynamicLog hiding (dzen)

-- layouts
import XMonad.Layout
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import Data.Ratio ((%))
import XMonad.Layout.Gaps

-- Font
myFont = "nu"

-- Colors
myBgBgColor          = "#000000"
myFgColor            = "#3b6fbb"
myBgColor            = "#000000"
myHighlightedFgColor = "#9479af"
myHighlightedBgColor = "#000000"

myActiveBorderColor   = "gray80"
myInactiveBorderColor = "gray20"

myCurrentWsFgColor     = "#3b6fbb"
myCurrentWsBgColor     = "#000000"
myVisibleWsFgColor     = "#9479af"
myVisibleWsBgColor     = "#000000"
myHiddenWsFgColor      = "#9479af"
myHiddenEmptyWsFgColor = "gray50"
myUrgentWsBgColor      = "brown"
myTitleFgColor         = "#9479af"

myUrgencyHintFgColor   = "#9479af"
myUrgencyHintBgColor   = "brown"

-- layout definitions
customLayout = avoidStruts $ ResizableTall 2 (3/100) (1/2) [] ||| Full

-- Workspaces
myWorkspaces = ["ichi","ni","san","shi","go","roku","sichi","hachi","ku"]

-- myWorkspaces = [ wrapBitmap "arch_10x10.xbm"
--	       , wrapBitmap "fox.xbm"
--	       , wrapBitmap "dish.xbm"
--	       , wrapBitmap "cat.xbm"
--	       , wrapBitmap "empty.xbm"
--	       , wrapBitmap "shroom.xbm"
--	       , wrapBitmap "bug_02.xbm"
--	       , wrapBitmap "eye_l.xbm"
--	       , wrapBitmap "eye_r.xbm"
--	       ]

-- wrapBitmap :: String -> String
-- wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsDir ++ bitmap ++ ")^p(5)"

-- Window rules
myManageHook = composeAll
	[ className =? "stuff" --> doFloat
        , title =? "Chromium" --> doShift "2"
	]

-- icons directory
myBitmapsDir = "/home/ishikawa/.dzen/"

-- main config
main :: IO ()
main = do
    d <- spawnDzen myLeftBar

    spawnToDzen "conky -c /home/ishikawa/.conkyrc" myRightBar
    spawn "xcompmgr"  
    xmonad $ defaultConfig
        { manageHook         = myManageHook 
        , terminal           = "urxvtc"
        , workspaces         = myWorkspaces
        , borderWidth        = 0
        , normalBorderColor  = myInactiveBorderColor
        , focusedBorderColor = myActiveBorderColor
        , layoutHook         = customLayout
        , logHook            = myLogHook d
        , modMask            = mod4Mask -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys

-- Extra Keys 
myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((controlMask, xK_Print)       	, spawn "sleep 0.5; scrot -s")
         , ((0, xK_Print)                 	, spawn "scrot")
	 , ((mod4Mask, xK_x)		  	, spawn "xscreensaver-command -lock")
	 , ((mod4Mask .|. shiftMask, xK_x)	, spawn "xscreensaver-command -prefs")
	 , ((mod4Mask, xK_p)			, spawn "dmenu_run")
	 , ((mod4Mask, xK_z)              	, sendMessage MirrorShrink)
	 , ((mod4Mask, xK_a)              	, sendMessage MirrorExpand)
	 ]
-- Dual screen change from greedyView to view         
	++  
	 [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (myWorkspaces) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	 
-- Status Bar Config
myLeftBar :: DzenConf
myLeftBar = defaultDzen
	{ width    = Just $ Percent 50
	, height   = Just 16
	, font	   = Just myFont
	, fgColor = Just myFgColor
	, bgColor = Just myBgColor
	}

myRightBar :: DzenConf
myRightBar = myLeftBar
	{ xPosition = Just $ Percent 50
        , width      = Just $ Percent 50
	, alignment  = Just RightAlign
	}


-- Dzen Config
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppOutput          = hPutStrLn h
    , ppSep             = wrapBg myBgBgColor " | "
    , ppWsSep           = " : "
    , ppCurrent         = dzenColor myCurrentWsFgColor myCurrentWsBgColor . wrap "[" "]"
    , ppVisible         = dzenColor myVisibleWsFgColor myVisibleWsBgColor . wrap "[" "]"
    , ppHidden          = wrapFg myHiddenWsFgColor
    , ppHiddenNoWindows = const "" 
    , ppUrgent          = wrapBg myUrgentWsBgColor
    , ppTitle           = (" " ++) . wrapFg myTitleFgColor 
    , ppLayout          = wrapFg "" . (\x -> 
        case x of
            "ResizableTall" -> "ResizableTall"
            "Full"          -> "Full"
            _               -> x) 
  }
  where
    wrapFg fg = dzenColor fg ""
    wrapBg bg = dzenColor "#9479af" bg

