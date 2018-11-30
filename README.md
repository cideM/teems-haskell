# teems-haskell :dizzy:
[![Build Status](https://travis-ci.org/cideM/teems-haskell.svg?branch=master)](https://travis-ci.org/cideM/teems-haskell)

## Quick start

Create `config.json` in your home directory using RGBA values as `json` arrays.

```json
[
    {
        "name": "zenburn",
        "colors": {
            "cursor": [255, 255, 255, 1],
            "foreground": [220, 220, 204, 1],
            "background": [63, 63, 63, 1],
            "color0": [31, 31, 31, 1],
            "color8": [112, 144, 128, 1],
            "color1": [112, 80, 80, 1],
            "color9": [220, 163, 163, 1],
            "color2": [96, 180, 138, 1],
            "color10": [195, 191, 159, 1],
            "color3": [223, 175, 143, 1],
            "color11": [240, 223, 175, 1],
            "color4": [80, 96, 112, 1],
            "color12": [148, 191, 243, 1],
            "color5": [220, 140, 195, 1],
            "color13": [236, 147, 211, 1],
            "color6": [140, 208, 211, 1],
            "color14": [147, 224, 227, 1],
            "color7": [220, 220, 204, 1],
            "color15": [255, 255, 255, 1]
        }
    }
]
```

```sh
  $ teems-haskell --help # most important command
  $ teems-haskell activate -t zenburn -c ~/config.json # activate theme -t from config file -c
  $ teems-haskell list-themes -c ~/config.json # list all themes in config file
  $ teems-haskell list-apps # list supported apps.
```

The app mutates your config files in place and **does not create any backups**. Usage of a `dotfiles` repo or something similar is advised.

## Supported apps

- Alacritty
- XTerm
- X
- Termite
- Kitty
