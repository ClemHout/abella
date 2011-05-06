;; abella-syntax.el --- Proof General instance for Abella - syntax file
;;
;; Copyright (C) 2011 Clement Houtmann.
;;
;; Author: Clement Houtmann <Clement.Houtmann@inria.fr>
;;


(defun make-regex (&rest args)
  (concat "\\<\\(" (regexp-opt args) "\\)\\>"))

(defun make-command-regex (&rest args)
  (concat "\\<\\(" (regexp-opt args) "\\)[^.]*."))

(require 'font-lock)

(defvar abella-script-font-lock-keywords
  (list
    (cons (make-command-regex "Set" "Query") font-lock-type-face)
    (cons (make-regex "Import" "Specification") font-lock-variable-name-face)
    (cons (make-command-regex "Type" "Kind" "Close") font-lock-keyword-face)
    (cons (make-command-regex "Define" "CoDefine") font-lock-keyword-face)
    (cons (make-command-regex "Theorem" "Split") font-lock-function-name-face)
    (cons (make-regex "skip") font-lock-warning-face))
  "Default highlighting for Abella Script mode")

(defvar abella-goals-font-lock-keywords
  (list
    (cons "Subgoal" font-lock-keyword-face))
  "Default highlighting for Abella Goals mode")

(defvar abella-response-font-lock-keywords
  (list
    (cons "Proof completed\." font-lock-function-name-face))
  "Default highlighting for Abella Response mode")



(setq abella-mode-syntax-table-entries
  (list ?_ "w"
    ?' "w"
    ?% "< b"
    ?\n "> b"
    ?. "."
    ?+ "."
    ?- "."
    ?= "."
    ?> "."
    ?< "."
    ?# "."
    ?\ "."))

(provide 'abella-syntax)
