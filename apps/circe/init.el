
;; * circe.el
;; - https://github.com/Silex/circe.el
;; - History
;;   -  2020-09-04 Created
(when (bk/add-load-path "apps/circe" "circe.el")
  (bk-auto-loads "circe" #'circe)
  (setq circe-network-options
      '(("Freenode"
         :tls t
         :nick "bartuka"
         :sasl-username "bartuka"
         :sasl-password "lkli3210"
         :channels ("#emacs")))))
