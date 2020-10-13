
(when (bk/add-load-path "langs/common-lisp" "slime")
  (bk-auto-loads "slime" #'slime #'slime-fancy #'slime-setup)
  (with-eval-after-load 'slime
    (setq inferior-lisp-program "sbcl")))
