# Installation

Build Helix with steel (this also installs the `forge` package manager):

```sh
git clone https://github.com/mattwparas/helix.git
git checkout steel-event-system
cargo xtask steel
```

Install the plugin:

```sh
forge pkg install --git https://github.com/mattwparas/steel-pty
```

Load the plugin by adding the following line to `~/.config/helix/init.scm`:

```
(require "steel-pty/term.scm")
```

# Usage

- `:open-term`: open a new terminal docked on the right edge of the editor
- `:open-floating-term`: open a new terminal as a centered, draggable floating window
- `Shift-Tab`: switch back to the editor
- `:new-term`: create a new terminal instance (docked)
- `:new-floating-term`: create a new terminal instance (floating)
- `:switch-term`: switch between terminal instances
- `:hide-terminal`: hide the terminal
- `:kill-current-terminal`: kill the current terminal instance
- `:copy-terminal-selection`: copy the highlighted text in the active terminal to the system clipboard

### Floating terminal mouse

- Drag the title bar to move the window.
- Click the `×` at the right of the title bar to kill the terminal.
- Click in the body to focus, then click+drag to select text.
- `Ctrl-Shift-C` copies the selection if your host terminal lets it through; otherwise bind `:copy-terminal-selection` to a key in your Helix config (most terminals capture `Ctrl-Shift-C` themselves before Helix sees it).

