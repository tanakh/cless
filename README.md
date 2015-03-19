# cless: Colorized LESS

# Install

* Install [Haskell Platform](https://www.haskell.org/platform/).

* Install `cless` using `cabal`.

```bash
$ cabal update
$ cabal install cless
```

# Usage

```bash
> cless --help
cless: Colorized LESS

Usage: cless [-L|--list-langs] [-S|--list-styles] [-N|--LINE-NUMBERS]
             [-l|--lang LANG] [-s|--style STYLE] [FILE]
  Print the content of FILE with syntax highlighting

Available options:
  -h,--help                Show this help text
  -L,--list-langs          Show the list of supported languages
  -S,--list-styles         Show the list of supported styles
  -N,--LINE-NUMBERS        Show line numbers
  -l,--lang LANG           Specify language name
  -s,--style STYLE         Specify style name (default 'pygments')
```

# Screenshots

![screenshot1](https://raw.githubusercontent.com/tanakh/cless/master/img/screenshot1.png)
![screenshot2](https://raw.githubusercontent.com/tanakh/cless/master/img/screenshot2.png)
