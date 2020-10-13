# Daily Emacs


### Installation

```bash
git clone --recurse-submodules https://github.com/wandersoncferreira/daily-emacs ~/.emacs.d
```

![](emacs.png)
*Produced by current setup!*


### Listings

``` bash
.
|-- apps
|   |-- circe
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- docker
|   |   |-- functions.el
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- ledger
|   |   |-- functions.el
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- README.md
|   |-- telega
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   `-- webpaste
|       |-- init.el
|       |-- pkgs
|       `-- README.md
|-- bin
|   |-- add
|   |-- remove
|   `-- update
|-- completion
|   |-- etc
|   |   `-- smex-items
|   |-- functions.el
|   |-- init.el
|   |-- pkgs
|   |   |-- company-mode
|   |   |-- ido-completing-read-plus
|   |   |-- smex
|   |   `-- swiper
|   `-- README.md
|-- core
|   |-- etc
|   |   |-- abbrev_defs
|   |   |-- cheatsheet.org
|   |   |-- custom.el
|   |   |-- recentf
|   |   `-- savehist
|   |-- functions.el
|   |-- init.el
|   |-- pkgs
|   |   |-- emacs-which-key
|   |   `-- exec-path-from-shell
|   `-- README.md
|-- cosmetics
|   |-- functions.el
|   |-- init.el
|   |-- pkgs
|   |   `-- diminish
|   |-- README.md
|   `-- themes
|       |-- alabaster-theme
|       |-- monokai-emacs
|       |-- noctilux-theme
|       |-- ritchie
|       `-- zenburn-emacs
|-- dependencies
|   |-- init.el
|   |-- pkgs
|   |   |-- a.el
|   |   |-- bui.el
|   |   |-- dash.el
|   |   |-- edit-indirect
|   |   |-- emacs-emojify
|   |   |-- emacs-memoize
|   |   |-- emacsql
|   |   |-- emacsql-sqlite3
|   |   |-- emacs-request
|   |   |-- emacs-web-server
|   |   |-- epl
|   |   |-- f.el
|   |   |-- frame-cmds
|   |   |-- frame-fns
|   |   |-- gh.el
|   |   |-- ht.el
|   |   |-- hydra
|   |   |-- json-reformat
|   |   |-- json-snatcher
|   |   |-- jump.el
|   |   |-- logito
|   |   |-- lsp-ui
|   |   |-- marshal.el
|   |   |-- parseclj
|   |   |-- parseedn
|   |   |-- pcache
|   |   |-- pfuture
|   |   |-- pkg-info
|   |   |-- posframe
|   |   |-- queue
|   |   |-- rainbow-identifiers
|   |   |-- s.el
|   |   |-- sesman
|   |   |-- spinner.el
|   |   |-- tablist
|   |   |-- transient
|   |   |-- visual-fill-column
|   |   `-- with-editor
|   `-- README.md
|-- editor
|   |-- etc
|   |-- functions.el
|   |-- init.el
|   |-- pkgs
|   |   |-- change-inner.el
|   |   |-- easy-kill
|   |   |-- expand-region.el
|   |   |-- fix-word
|   |   |-- goto-chg
|   |   |-- highlight-symbol.el
|   |   `-- multiple-cursors.el
|   `-- README.md
|-- emacs.png
|-- init.el
|-- langs
|   |-- clojure
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- common-lisp
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- haskell
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- python
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- README.md
|   |-- scala
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   `-- sql
|       |-- init.el
|       |-- pkgs
|       `-- README.md
|-- modes
|   |-- json
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- markdown
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   |-- prog
|   |   |-- functions.el
|   |   |-- init.el
|   |   |-- pkgs
|   |   `-- README.md
|   `-- README.md
|-- org
|   |-- functions.el
|   |-- init.el
|   |-- pkgs
|   |   |-- company-org-roam
|   |   |-- org-roam
|   |   |-- org-roam-server
|   |   `-- plantuml-mode
|   `-- README.md
|-- projects
|   |-- etc
|   |   |-- projectile-bookmarks.eld
|   |   `-- projectile.cache
|   |-- init.el
|   |-- pkgs
|   |   `-- projectile
|   `-- README.md
|-- README.md
|-- search
|   |-- init.el
|   |-- pkgs
|   |   |-- avy
|   |   |-- jump-char
|   |   `-- wgrep
|   `-- README.md
|-- shell
|   |-- init.el
|   `-- README.md
|-- snippets
|-- tramp
|-- url
|   |-- cookies
|   `-- cookies~
|-- version-control
|   |-- functions.el
|   |-- init.el
|   |-- pkgs
|   |   |-- gist.el
|   |   |-- git-modes
|   |   |-- git-timemachine
|   |   `-- magit
|   `-- README.md
`-- window
    |-- functions.el
    |-- init.el
    |-- pkgs
    |   |-- ace-window
    |   |-- switch-window
    |   |-- windresize
    |   `-- zoom-frm
    `-- README.md

133 directories, 80 files
```

### Update

This will run the pull command for all submodules cloned inside the
project.

```bash
~/.emacs.d/bin/update
```

