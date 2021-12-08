import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.NoBorders
import XMonad.Util.Run(safeSpawn)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalMouseBindings)
import XMonad.Util.EZConfig(removeMouseBindings)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import System.Cmd
import System.Environment
import System.IO
import Data.Monoid
import Control.Monad (when)
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Data.Monoid (All(All))
import XMonad.Util.WindowProperties (getProp32)
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.StackSet as S
import System.Exit
import XMonad.Layout.FixedColumn
import XMonad.Layout.MultiColumns

import XMonad.Layout.BoringWindows
import XMonad.Layout.Grid
import XMonad.Layout.GridVariants
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.Minimize
import XMonad.ManageHook
import Data.Ratio ((%))


myLogHook xmproc = do
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
                       , ppTitle = dzenColor "#9581A1" "" . myAddIcon iconData . shorten 150
                       , ppLayout = \x -> "  "
                       , ppVisible = dzenColor "#FFFFFF" "#9581A1" . pad
                       , ppCurrent = dzenColor "#FFFFFF" "#6674DE" . pad
                       , ppHidden = dzenColor "#9581A1" "" . pad
                       }

myAddIcon iconName s = case iconName of
  "" -> s
  otherwise -> "{" ++ iconName ++ "} " ++ s

getIconData Nothing = return ""
getIconData (Just winid) = return winid

-- layout = smartBorders(avoidStruts(onWorkspace "8" im $ (grid1 ||| grid2 ||| grid3 ||| Full ||| tiled ||| Mirror tiled)))
layout = minimize(boringWindows(smartBorders(avoidStruts(grid1 ||| grid2 ||| grid3 ||| {- fixedColumn ||| -} Full ||| layoutHints(grid1) ||| tiled ||| Mirror tiled))))
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 4/5
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    gridRatio = (16 / 9)
    grid1 = SplitGrid XMonad.Layout.GridVariants.L 1 1 (80 / 100) gridRatio (1/100)
    grid2 = SplitGrid XMonad.Layout.GridVariants.L 1 2 (64 / 100) gridRatio (1/100)
    grid3 = SplitGrid XMonad.Layout.GridVariants.L 1 1 (32 / 100) gridRatio (1/100)
    {- fixedColumn = FixedColumn 1 20 180 10 -}
    {- fixedColumn = multiCol [1] 1 0.01 (-0.5) -}
    -- im = withIM (1 % 7) (And (Title "Chat") (Role "pop-up")) (GridRatio (2/6))

myWorkspaces = map (:[]) ['1'..'9']

isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)
lazyView w ws = if isVisible w ws then ws else W.view w ws

myKeys = [ ("M-A", io (exitWith ExitSuccess))
         , ("M-k", focusUp)
         , ("M-j", focusDown)
         , ("M-a", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
         , ("M-m", withFocused minimizeWindow)
         , ("M-S-m", sendMessage RestoreNextMinimizedWin)
         , ("M-b", markBoring)
         , ("M-S-r", swapNextScreen)
         , ("M-r", rotAllUp)
         , ("M-S-b", clearBoring)
         , ("M-z", viewScreen 0)
         , ("M-x", viewScreen 1)
         , ("M-<Up>", windows W.focusUp)
         , ("M-<Down>", windows W.focusDown)
         , ("M-<Left>", viewScreen 0)
         , ("M-<Right>", viewScreen 1)
        ] ++ [ (otherModMasks ++ "M-" ++ [key], action tag)
          | (tag, key)  <- zip myWorkspaces "123456789"
          , (otherModMasks, action) <- [("", windows . lazyView)] -- was W.greedyView
        ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask .|. controlMask, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

main = do
    xmproc <- spawnPipe "$HOME/.xmonad/bar/dzen.sh"
    xmonad $ ewmh defaultConfig
        { manageHook = manageDocks <+> myManageHooks
        , layoutHook = layout
        , logHook            = myLogHook xmproc
        , focusedBorderColor = "#FF0000"
        , normalBorderColor = "#000000"
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , focusFollowsMouse = False
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        , mouseBindings = myMouseBindings
        }
        `additionalKeysP` myKeys
        `additionalKeys`
        [ ((controlMask .|. mod1Mask, xK_l), safeSpawn "xscreensaver-command" ["-lock"])
        {- [ ((controlMask .|. mod1Mask, xK_l), spawn "xautolock -locknow") -}
        , ((mod4Mask, xK_f), fullFloatFocused)
        , ((mod4Mask, xK_g), floatFocused)
        , ((mod4Mask, xK_p), spawn "gmrun")
        , ((0, xK_Print), spawn "snipit")
        ]

myManageHooks = composeAll
-- Allows focusing other monitors without killing the fullscreen
 [ isFullscreen --> (doF W.focusDown <+> doFullFloat),
    className =? "top"            --> doFloat
 
-- Single monitor setups, or if the previous hook doesn't work
    {-[ isFullscreen --> doFullFloat-}
    -- skipped
    ]

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

floatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFloat f

-- Stolen from a newer version of XMonad.Hooks.EwmhDesktops:
-- |
-- An event hook to handle applications that wish to fullscreen using the
-- _NET_WM_STATE protocol. This includes users of the gtk_window_fullscreen()
-- function, such as Totem, Evince and OpenOffice.org.
--
-- Note this is not included in 'ewmh'.
myFullscreenEventHook :: Event -> X All
myFullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 state win

  let isFull = fromIntegral fullsc `elem` wstate

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      ptype = 4 -- The atom property type for changeProperty
      chWstate f = io $ changeProperty32 dpy win state ptype propModeReplace (f wstate)

  when (typ == state && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ delete (fi fullsc)
      windows $ W.sink win

  return $ All True

  -- DLM: Added to resolve a few dependencies:
  where fi :: (Integral a, Num b) => a -> b
        fi = fromIntegral

myFullscreenEventHook _ = return $ All True
