# IceWM like bar for XMonad

IcwWM like bar for XMonad with build in support for
some system graphs like CPU top, memory usage, network
and battery.  

## Installation

For configuration options change src/Main.hs before building.

- The entry point is ```bars``` which contains list of bars to create.
- Each bar has screen number and gravity option for placement and list
    of widgets to add.
- Widgets are layed out from right to left (and optionally bottom to up in frames).
- There are multiple Attributes which can be added for supported widgets,
    like ```#Width```, ```#TextColor```, ```#SetFont```

After changing configuration, to build the bar run:
```sh
sudo apt-get install libx11-dev libxinerama-dev libx11-dev libxrandr-dev libxft-dev
git clone https://github.com/IvanVolosyuk/bar.git
cd bar
cabal install
mkdir ~/.xmonad/bar
cp dist/build/bar/bar ~/.xmonad/bar/bar
# Default on-click handlers: top.sh and clock.sh (change in Main.hs)
cp extra/* ~/.xmonad/bar
```

XMonad configuration should be mostly compatible with dzen. Example config
```xmonad.hs``` is included in extras. I have:
```
import Control.Monad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

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
defaults xmproc = defaultConfig {
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

```

