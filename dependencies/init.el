;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-20 11:11:25 (wand)>

;;; Code:

;; * spinner.el (library)
;;   - http://github.com/Malabarba/spinner.el
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "spinner.el")

;; * parseedn (library)
;;   - https://github.com/clojure-emacs/parseedn
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "parseedn")

;; * queue (library)
;;   - https://github.com/emacsmirror/queue
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "queue")

;; * sesman (library)
;;   - https://github.com/vspinu/sesman
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "sesman")

;; * a.el (library)
;;   - https://github.com/plexus/a.el
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "a.el")

;; * parseclj (library)
;;   - https://github.com/clojure-emacs/parseclj
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "parseclj")

;; * dash (library)
;;   - https://github.com/magnars/dash.el
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "dash.el")

;; * with-editor (library)
;;   - https://github.com/magit/with-editor
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "with-editor")

;; * transient (library)
;;   - https://github.com/magit/transient
;;   - History
;;     - 2020-08-15 Create
(bk/add-load-path "dependencies" "transient/lisp")

;; * f.el (library)
;;   - https://github.com/rejeep/f.el
;;   - History
;;     - 2020-08-16 Create
(bk/add-load-path "dependencies" "f.el")

;; * s.el (library)
;;   - https://github.com/rejeep/s.el
;;   - History
;;     - 2020-08-16 Create
(bk/add-load-path "dependencies" "s.el")

;; * emacsql (library)
;;   - https://github.com/skeeto/emacsql
;;   - History
;;     - 2020-08-16 Create
(bk/add-load-path "dependencies" "emacsql")

;; * emacsql-sqlite3 (library)
;;   - https://github.com/cireu/emacsql-sqlite3
;;   - History
;;     - 2020-08-16 Create
(bk/add-load-path "dependencies" "emacsql-sqlite3")

;; * emacs-web-server (library)
;;   - https://github.com/skeeto/emacs-web-server
;;   - History
;;     - 2020-08-16 Create
(bk/add-load-path "dependencies" "emacs-web-server")

;; * posframe (library)
;;   - https://github.com/tumashu/posframe
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "posframe")

;; * jump.el (library)
;;   - https://github.com/eschulte/jump.el
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "jump.el")

;; * pkg-info (library)
;;   - https://github.com/emacsorphanage/pkg-info
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "pkg-info")

;; * epl (library)
;;   - https://github.com/cask/epl
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "epl")

;; * edit-indirect (library)
;;   - https://github.com/Fanael/edit-indirect
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "edit-indirect")

;; * ht.el (library)
;;   - https://github.com/Wilfred/ht.el
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "ht.el")

;; * pfuture (library)
;;   - https://github.com/Alexander-Miller/pfuture
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "pfuture")

;; * bui.el (library)
;;   - https://github.com/alezost/bui.el
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "bui.el")

;; * lsp-ui (library)
;;   - https://github.com/emacs-lsp/lsp-ui
;;   - History
;;     - 2020-08-18 Create
(bk/add-load-path "dependencies" "lsp-ui")

;; * tablist (library)
;;   - https://github.com/politza/tablist
;;   - History
;;     - 2020-09-04 Create
(bk/add-load-path "dependencies" "tablist")

;; * json-snatcher (library)
;;   - https://github.com/Sterlingg/json-snatcher
;;   - History
;;     - 2020-09-04 Create
(bk/add-load-path "dependencies" "json-snatcher")

;; * json-reformat (library)
;;   - https://github.com/gongo/json-reformat
;;   - History
;;     - 2020-09-04 Create
(bk/add-load-path "dependencies" "json-reformat")

;; * frame-cmds (library)
;;   - History
;;     - 2020-09-15 Create
(bk/add-load-path "dependencies" "frame-cmds")

;; * frame-fns (library)
;;   - History
;;     - 2020-09-15 Create
(bk/add-load-path "dependencies" "frame-fns")

;; * memoize (library)
;;   - https://github.com/skeeto/emacs-memoize
;;   - History
;;     - 2020-09-04 Created
(bk/add-load-path "dependencies" "emacs-memoize")

;; * hydra
;; - https://github.com/abo-abo/hydra
;; - History
;;   -  2020-08-18 Created
(bk/add-load-path "dependencies" "hydra")

;; * rainbow-identifiers
;; - https://github.com/Fanael/rainbow-identifiers
;; - History
;;   -  2020-09-25 Created
(bk/add-load-path "dependencies" "rainbow-identifiers")

;; * visual-fill-column
;; - https://github.com/joostkremers/visual-fill-column
;; - History
;;   -  2020-09-25 Created
(bk/add-load-path "dependencies" "visual-fill-column")

;; * gh.el
;; - https://github.com/sigma/gh.el
;; - History
;;   -  2020-10-01 Created
(bk/add-load-path "dependencies" "gh.el")

;; * marshal.el
;; - https://github.com/sigma/marshal.el
;; - History
;;   -  2020-10-01 Created
(bk/add-load-path "dependencies" "marshal.el")

;; * pcache
;; - https://github.com/sigma/pcache
;; - History
;;   -  2020-10-01 Created
(bk/add-load-path "dependencies" "pcache")

;; * logito
;; - https://github.com/sigma/logito
;; - History
;;   -  2020-10-01 Created
(bk/add-load-path "dependencies" "logito")

;; * emacs-emojify
;; - https://github.com/iqbalansari/emacs-emojify
;; - History
;;   -  2020-09-25 Created
(bk/add-load-path "dependencies" "emacs-emojify")
(bk-auto-loads "emojify" #'emojify-mode #'emojify-mode-line-mode)

;; * emacs-request
;; - https://github.com/tkf/emacs-request
;; - History
;;   -  2020-10-06 Created
(bk/add-load-path "dependencies" "emacs-request")

;; * emacs-deferred
;; - https://github.com/kiwanami/emacs-deferred
;; - History
;;   - 2020-10-14 Created
(bk/add-load-path "dependencies" "emacs-deferred")

;; * anaphora
;; - https://github.com/rolandwalker/anaphora
;; - History
;;   - 2020-10-14 Created
(bk/add-load-path "dependencies" "anaphora")

;; * emacs-websocket
;; - https://github.com/ahyatt/emacs-websocket
;; - History
;;   - 2020-10-14 Created
(bk/add-load-path "dependencies" "emacs-websocket")

;; * polymode
;; - https://github.com/polymode/polymode
;; - History
;;   - 2020-10-14 Created
(bk/add-load-path "dependencies" "polymode")

;; * htmlize
;; - History
;;   - 2020-10-19 Created
(bk/add-load-path "dependencies" "emacs-htmlize")

;; * parsebib
;; - https://github.com/joostkremers/parsebib
;; - History
;;   - 2020-10-19 Created
(bk/add-load-path "dependencies" "parsebib")

(bk/add-load-path "dependencies" "emacs-async")

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
