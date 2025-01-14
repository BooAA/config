local wezterm = require 'wezterm'
local act = wezterm.action

local config = {}

if wezterm.config_builder then
   config = wezterm.config_builder()
end

config.automatically_reload_config = true

config.color_scheme = 'GruvboxDark'
config.font = wezterm.font 'Hack'
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }
config.font_size = 12.0
config.command_palette_font_size = 12.0
config.adjust_window_size_when_changing_font_size = false

config.window_padding = {
   left = 5,
   right = 5,
   top = 0,
   bottom = 0
}

config.mux_env_remove = {
   -- 'SSH_AUTH_SOCK',
   -- 'SSH_CLIENT',
   -- 'SSH_CONNECTION',
}

config.animation_fps = 60
config.max_fps = 120
config.cursor_blink_rate = 0
config.text_blink_rate = 0
config.status_update_interval = 20

config.window_close_confirmation = 'NeverPrompt'
config.audible_bell = 'Disabled'
config.hide_mouse_cursor_when_typing = false

config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.tab_max_width = 20
config.show_tab_index_in_tab_bar = true

config.check_for_updates = false
config.detect_password_input = false
config.scrollback_lines = 50000
config.webgpu_power_preference = 'HighPerformance'

config.disable_default_key_bindings = true
config.keys = {
   -- tab -- 
   { key = 'Tab', mods = 'CTRL', action = act.ActivateTabRelative(1) },
   { key = 'Tab', mods = 'SHIFT|CTRL', action = act.ActivateTabRelative(-1) },
   { key = 'PageUp', mods = 'CTRL', action = act.ActivateTabRelative(-1) },
   { key = 'PageDown', mods = 'CTRL', action = act.ActivateTabRelative(1) },
   { key = 'PageUp', mods = 'SHIFT|CTRL', action = act.MoveTabRelative(-1) },
   { key = 'PageDown', mods = 'SHIFT|CTRL', action = act.MoveTabRelative(1) },   
   { key = '1', mods = 'CTRL', action = act.ActivateTab(0) },
   { key = '2', mods = 'CTRL', action = act.ActivateTab(1) },
   { key = '3', mods = 'CTRL', action = act.ActivateTab(2) },
   { key = '4', mods = 'CTRL', action = act.ActivateTab(3) },
   { key = '5', mods = 'CTRL', action = act.ActivateTab(4) },
   { key = '6', mods = 'CTRL', action = act.ActivateTab(5) },
   { key = '7', mods = 'CTRL', action = act.ActivateTab(6) },
   { key = '8', mods = 'CTRL', action = act.ActivateTab(7) },
   { key = '9', mods = 'CTRL', action = act.ActivateTab(-1) },
   { key = 'T', mods = 'SHIFT|CTRL', action = act.SpawnTab 'CurrentPaneDomain' },
   { key = 'W', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },

   -- copy, paste, search --
   { key = 'C', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
   { key = 'V', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
   { key = 'F', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
   { key = ' ', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },

   -- scrollback --
   { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollByLine(-1) },
   { key = 'DownArrow', mods = 'SHIFT', action = act.ScrollByLine(1) },   
   { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-0.5) },
   { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(0.5) },
   { key = 'Home', mods = 'SHIFT', action = act.ScrollToTop },
   { key = 'End', mods = 'SHIFT', action = act.ScrollToBottom },

   -- pane --
   { key = '%', mods = 'SHIFT|CTRL', action = act.SplitHorizontal{ domain =  'CurrentPaneDomain' } },
   { key = '\"', mods = 'SHIFT|CTRL', action = act.SplitVertical{ domain =  'CurrentPaneDomain' } },
   { key = 'LeftArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left' },
   { key = 'RightArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right' },
   { key = 'UpArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up' },
   { key = 'DownArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down' },
   { key = 'H', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left' },
   { key = 'J', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down' },
   { key = 'K', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up' },
   { key = 'L', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right' },
   { key = 'A', mods = 'SHIFT|CTRL', action = act.PaneSelect },
   { key = 'S', mods = 'SHIFT|CTRL', action = act.PaneSelect { mode = 'SwapWithActive' } },
   { key = 'Z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },

   -- misc --
   { key = '+', mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
   { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
   { key = '=', mods = 'CTRL', action = act.ResetFontSize },
   { key = 'P', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },   
   -- { key = 'R', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
   { key = 'D', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
}

return config
