;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:34:35 (wand)>

;;; Code:

;; * ledger mode
;; - https://github.com/ledger/ledger-mode
;; - History
;;   -  2020-08-15 Created
;;   -  2020-08-18 Fix eval-after-load to ledger-modex
(defun bk/clean-ledger ()
  "Bring back timeline structure to the whole file."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

(defun bk-setup-feature-ledger ()
  "Customizations for ledger."
  (setq ledger-reports
        '(("netcash" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -R -X R$ --current bal ^assets:bank liabilities:card")
          ("networth" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^assets:bank liabilities equity:apartment")
          ("spent-vs-earned" "ledger [[ledger-mode-flags]] -f /home/wand/.ledger bal -X BRL --period=\"last 4 weeks\" ^Expenses ^Income --invert -S amount")
          ("budget" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^assets:bank:checking:budget liabilities:card")
          ("creta" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^expenses:car: ^equity:car")
          ("taxes" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -R -X R$ --current bal ^expenses:taxes")
          ("bal" "%(binary) -f %(ledger-file) bal")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(when (bk/add-load-path "apps/ledger" "ledger-mode")
  (bk-auto-loads "ledger-mode"
                 '("\\ledger$" . ledger-mode)
                 '("\\.ledger$" . ledger-mode))
  (set-register ?l '(file . "~/.ledger"))
  (with-eval-after-load 'ledger-mode
    (bk-setup-feature-ledger)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
