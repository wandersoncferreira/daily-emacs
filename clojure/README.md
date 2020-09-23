# Clojure

My current setup for Clojure is simple and leverages Cider
functionalities. Basically, when you start Emacs there are only three
functions available for you from `cider`: `cider-jack-in`,
`cider-connect`, and `cider-jack-in-clj&cljs`.

If you call any of these functions, the whole cider & co will be
loaded.

This is great behavior to keep the session clean when you are working
with something else.


## Dependencies

- clojure-mode
- cider
- cljr-ivy (proprietary)
- clj-refactor
- flycheck-clj-kondo


## Main bindings

| Key      | Function |
| ---      | -------- |
| C-c M-j  | `cider-jack-in` |
| C-c C-m  | `cljr-refactor-prefix` |
| C-c C-r  | `cljr-ivy` | 


You can always press `C-h` in the middle of any keychord and
`which-key` popup will show up to help you remember what you want to
do.
