local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.color_scheme = 'Solarized Light (Gogh)'
config.send_composed_key_when_left_alt_is_pressed = false                     config.send_composed_key_when_right_alt_is_pressed = false


return config
