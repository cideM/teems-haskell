# teems-haskell

## Quick start

Create `config.json` in your home directory.

```json
[
    {
        "name": "zenburn",
        "colors": {
            "foreground": "#dcdccc",
            "background": "#3f3f3f",
            "color0": "#1f1f1f",
            "color8": "#709080",
            "color1": "#705050",
            "color9": "#dca3a3",
            "color2": "#60b48a",
            "color10": "#c3bf9f",
            "color3": "#dfaf8f",
            "color11": "#f0dfaf",
            "color4": "#506070",
            "color12": "#94bff3",
            "color5": "#dc8cc3",
            "color13": "#ec93d3",
            "color6": "#8cd0d3",
            "color14": "#93e0e3",
            "color7": "#dcdccc",
            "color15": "#ffffff"
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
