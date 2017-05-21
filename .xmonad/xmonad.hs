import XMonad
import XMonad.Config.Xfce

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import qualified Data.Map as M
import qualified XMonad.StackSet as W

floatingClasses =
  [ "MPlayer"
  , "Gimp"
  , "Wrapper"
  , "xfce4-xkb-plugin"
  , "xfce4-appfinder"
  ]

-- IMPORTANT! When you add fields to the record they are overwritten, not merged
-- Check https://wiki.haskell.org/Xmonad/Using_xmonad_in_XFCE before touching!
main = xmonad xfceConfig
    { modMask            = mod4Mask
    , terminal           = "xfce4-terminal"
    , focusFollowsMouse  = True
    , clickJustFocuses   = False
    , normalBorderColor  = "#dddddd"
    , focusedBorderColor = "#ffffff"
    , manageHook         = manageDocks <+> (composeAll . map (\n -> className =? n --> doFloat)) floatingClasses
    , keys               = myKeys
    , startupHook = ewmhDesktopsStartup <+> setWMName "LG3D"
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Toggle status bar gap
    [ ((modm,               xK_b     ), sendMessage ToggleStruts)

    -- Log out
    , ((modm .|. shiftMask, xK_q     ), spawn "xfce4-session-logout")                  

    -- Change configuration
--    , ((modm,               xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- launch a terminal
    , ((modm .|. shiftMask, xK_Return), spawn "xfce4-terminal")

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch appfinder
    , ((modm .|. shiftMask, xK_p     ), spawn "xfce4-appfinder")

    -- switch to "es" keyboard
    , ((modm,               xK_s     ), spawn "setxkbmap es")

    -- switch to "us" keyboard
    , ((modm,               xK_d     ), spawn "setxkbmap us")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    ]
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- reordered
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
