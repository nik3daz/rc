{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Control.Monad
import Data.List (delete)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Cmd
import System.Environment
import System.Exit
import System.IO
import System.Process
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.BorderResize
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationMadness
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Dishes
import XMonad.Layout.Hidden as Hidden
import XMonad.Layout.MagicFocus
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Roledex
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation as NAV
import XMonad.Util.NamedWindows
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.WindowProperties (getProp32)
import XMonad.Util.XUtils as XUtils

import qualified XMonad.Hooks.ICCCMFocus as ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.StackSet as S


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "konsole"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#FF0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,                xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,                xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm,                 xK_h ),                   sendMessage $ Go L)
    , ((modm .|. shiftMask,   xK_h ),                   sendMessage $ NAV.Swap L)
    , ((modm .|. controlMask, xK_h ),               do {sendMessage $ ExpandTowards L; sendMessage Expand})
    , ((modm .|. shiftMask .|. controlMask, xK_h ), do {sendMessage $ ShrinkFrom L;    sendMessage Shrink})
    , ((modm,                 xK_l ),                   sendMessage $ Go R)
    , ((modm .|. shiftMask,   xK_l ),                   sendMessage $ NAV.Swap R)
    , ((modm .|. controlMask, xK_l ),               do {sendMessage $ ExpandTowards R; sendMessage Shrink})
    , ((modm .|. shiftMask .|. controlMask, xK_l ), do {sendMessage $ ShrinkFrom R;    sendMessage Expand})
    , ((modm,                 xK_k ),                   sendMessage $ Go U)
    , ((modm .|. shiftMask,   xK_k ),                   sendMessage $ NAV.Swap U)
    , ((modm .|. controlMask, xK_k ),               do {sendMessage $ ExpandTowards U; sendMessage MirrorExpand})
    , ((modm .|. shiftMask .|. controlMask, xK_k ), do {sendMessage $ ShrinkFrom U;    sendMessage MirrorShrink})
    , ((modm,                 xK_j ),                   sendMessage $ Go D)
    , ((modm .|. shiftMask,   xK_j ),                   sendMessage $ NAV.Swap D)
    , ((modm .|. controlMask, xK_j ),               do {sendMessage $ ExpandTowards D; sendMessage MirrorShrink})
    , ((modm .|. shiftMask .|. controlMask, xK_j ), do {sendMessage $ ShrinkFrom D;    sendMessage MirrorExpand})

    , ((modm,                 xK_Left ),                   sendMessage $ Go L)
    , ((modm .|. shiftMask,   xK_Left ),                   sendMessage $ NAV.Swap L)
    , ((modm .|. controlMask, xK_Left ),               do {sendMessage $ ExpandTowards L; sendMessage Expand})
    , ((modm .|. shiftMask .|. controlMask, xK_Left ), do {sendMessage $ ShrinkFrom L;    sendMessage Shrink})
    , ((modm,                 xK_Right ),                   sendMessage $ Go R)
    , ((modm .|. shiftMask,   xK_Right ),                   sendMessage $ NAV.Swap R)
    , ((modm .|. controlMask, xK_Right ),               do {sendMessage $ ExpandTowards R; sendMessage Shrink})
    , ((modm .|. shiftMask .|. controlMask, xK_Right ), do {sendMessage $ ShrinkFrom R;    sendMessage Expand})
    , ((modm,                 xK_Up ),                   sendMessage $ Go U)
    , ((modm .|. shiftMask,   xK_Up ),                   sendMessage $ NAV.Swap U)
    , ((modm .|. controlMask, xK_Up ),               do {sendMessage $ ExpandTowards U; sendMessage MirrorExpand})
    , ((modm .|. shiftMask .|. controlMask, xK_Up ), do {sendMessage $ ShrinkFrom U;    sendMessage MirrorShrink})
    , ((modm,                 xK_Down ),                   sendMessage $ Go D)
    , ((modm .|. shiftMask,   xK_Down ),                   sendMessage $ NAV.Swap D)
    , ((modm .|. controlMask, xK_Down ),               do {sendMessage $ ExpandTowards D; sendMessage MirrorShrink})
    , ((modm .|. shiftMask .|. controlMask, xK_Down ), do {sendMessage $ ShrinkFrom D;    sendMessage MirrorExpand})
    , ((modm,    xK_r     ), sendMessage BSP.Rotate )
    , ((modm,    xK_s     ), sendMessage BSP.Swap )
    --, ((mod4Mask,             xK_Return), windows W.swapMaster)
    , ((modm , xK_x     ), do {withFocused XUtils.hideWindow; withFocused Hidden.hideWindow})
    , ((modm , xK_v     ), do {popNewestHiddenWindow; withFocused XUtils.showWindow})

    , ((modm , xK_c     ), sendMessage FocusParent)
    , ((modm , xK_a     ), sendMessage Equalize)
    , ((modm .|. shiftMask, xK_a     ), sendMessage Balance)

    , ((modm .|. controlMask, xK_m     ), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_u     ), withFocused (sendMessage . UnMerge))

    -- Push window back into tiling
    , ((XMonad.mod4Mask,    xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN (-1)))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask .|. controlMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
--    , ((controlMask       , xK_Print     ), spawn "echo attach  |nc localhost 12345 -q 0")
    , ((controlMask       , xK_Print     ), spawn "DISPLAY=win8:0 konsole")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) [xK_1 ..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_u, xK_i, xK_o] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [
      ((modm, xK_F2), spawn "gmrun")
    , ((modm, xK_F4), kill1)
    , ((modm, xK_Delete), kill1)
    --, ((mod4Mask, xK_i), spawn "/usr/bin/fetchotp -x")

    --, ((controlMask .|. mod1Mask, xK_l), spawn "dm-tool lock")
    , ((modm,                     xK_F1), spawn "dm-tool lock")

    , ((controlMask, xK_F11             ), spawn "/home/ivan/opt/bin/volume down")
    , ((controlMask, xK_F12             ), spawn "/home/ivan/opt/bin/volume up")
    , ((0, xF86XK_Calculator            ), spawn "/home/ivan/.xmonad/ivan/toggle_qemu_sound.sh")
    ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
data MyAccordion a = MyAccordion Rational Rational Rational deriving ( Read, Show )

instance LayoutClass MyAccordion Window where
    pureLayout (MyAccordion fract atten delta) sc ws = zip all (splitPropotional propotions sc)
     where
       
       ups    = W.up ws
       dns    = W.down ws
       all    = (reverse ups)  ++ [W.focus ws] ++ dns

       propotions = (reverse $ one (length ups) f) ++ [fract] ++ (one (length dns) f) where
         f = (1 - fract) / 2
         one 0 sz = []
         one 1 sz = [sz]
         one n sz = sz : (one (n-1) (sz / atten))


       splitPropotional [_] rect = [rect]
       splitPropotional (sz:sizes) rect = r0 : splitPropotional sizes rother where
          (r0, rother) = splitVerticallyBy (sz / sum(sz:sizes)) rect

    pureMessage (MyAccordion fract atten delta) m =
            msum [fmap resize (fromMessage m), fmap mresize (fromMessage m)]

      where resize Shrink             = MyAccordion (max (1-((1-fract)*delta)) 0.3) 2 delta
            resize Expand             = MyAccordion (1-((1-fract)/delta)) 2 delta
            mresize MirrorShrink      = MyAccordion fract (atten*delta) delta
            mresize MirrorExpand      = MyAccordion fract (max (atten/delta) 1) delta

isLayoutToll :: X Bool
isLayoutToll = fmap (isSuffixOf "Preview") $ gets (description . S.layout . S.workspace . S.current . windowset)

myLayout = renamed [ CutWordsLeft 1] . avoidStruts . borderResize . hiddenWindows $ configurableNavigation noNavigateBorders $ layouts
-- ||| threeCol
  where
     layouts = bsp ||| noBorders (Full)

     bsp = subLayout [] (MyAccordion 0.8 2 1.5) $ emptyBSP
     tiled = named "Tiled" $ reflectHoriz $ ResizableTall nmaster delta ratio []
     accordion = named "Accord" $ MyAccordion 0.8 2 1.5
     preview = named "Preview" $ Mirror $ magicFocus $ Tall 1 (3/100) (1/2)
     -- mirrorTiled = Mirror $ ResizableTall nmaster delta ratio []
     -- threeCol = Mirror $ ThreeCol nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "top"            --> doFloat
    , className =? "Kcalc"          --> doFloat
    , className =? "Kmix"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
--myEventHook = mempty -- docksEventHook
myEventHook = followOnlyIf (fmap not isLayoutToll)


-- icon name = "^i(icons/"++name++".xpm) "
 
myAddIcon iconName s = "{" ++ iconName ++ "}" ++ s
addLayoutName text s = "[" ++ text ++ "] " ++ s

getIconData Nothing = return ""
getIconData (Just winid) = return winid

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook xmproc = do
    ICCCMFocus.takeTopFocus
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort dzenPP
    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset
    -- workspace list
    let ws = pprWindowSet sort' urgents dzenPP winset
    -- window title
    let winid = S.peek winset >>= return . show
    iconData <- getIconData winid

    dynamicLogWithPP $ dzenPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = addLayoutName ld . dzenColor "#202020" "" . myAddIcon iconData . shorten 150
                       , ppLayout = \x -> "  "
                       }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = docksStartupHook

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe "/home/ivan/.xmonad/bar/bar.sh"
  xmonad $ docks $ defaults xmproc

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults xmproc = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
    }
