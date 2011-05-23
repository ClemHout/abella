;; abella.el --- Proof General instance for Abella
;;
;; Copyright (C) 2011 Clement Houtmann.
;;
;; Author: Clement Houtmann <Clement.Houtmann@inria.fr>
;;

(eval-and-compile
  (require 'proof-site)			; compilation for abella
  (proof-ready-for-assistant 'abella))

(require 'proof)
(require 'proof-easy-config)		; easy configure mechanism
(require 'abella-syntax)

(proof-easy-config
 'abella "Abella"
 proof-prog-name		"abella"
 proof-terminal-string		"."
 proof-script-comment-start-regexp	"%"
 proof-script-fly-past-comments t
 proof-script-comment-end-regexp	"^[^ *%]"
 proof-completed-proof-behaviour 'closeany
 ;proof-goal-command-regexp	"Theorem [:ascii:]+:[:ascii:]+\."
 proof-assistant-home-page	 "http://abella.cs.umn.edu"
 proof-shell-annotated-prompt-regexp "^.* < "
 proof-shell-quit-cmd		 "Quit."
 proof-shell-start-goals-regexp	 ">>"
 proof-shell-end-goals-regexp	 "<<"
 proof-shell-restart-cmd	 "Reset."
 proof-shell-error-regexp	 "Error:.*\\|Syntax error\."
 proof-save-command-regexp	proof-no-regexp
 proof-find-and-forget-fn   'abella-find-and-forget-fn
 proof-script-syntax-table-entries  abella-mode-syntax-table-entries
 proof-script-font-lock-keywords  abella-script-font-lock-keywords
 proof-goals-font-lock-keywords  abella-goals-font-lock-keywords
 proof-response-font-lock-keywords  abella-response-font-lock-keywords
 ;proof-non-undoables-regexp	"undo\\|Back\\|Reset\\|abort\\|[a-z].*"
)

(provide 'abella)

(defun abella-count (span)
  (setq next (next-span span 'name))
  (if (eq next nil)
    1
    (+ 1 (abella-count next))))
  

(defun abella-find-and-forget-cmd (span)
  (setq cmd (span-property span 'cmd))
  (cond
    ((eq cmd nil) "helu") ; comment
    ((string-match "Specification.*" cmd) "Reset.")
    ((string-match "Theorem.*" cmd) "abort.")
    ((string-match
      "\\(Define\\|CoDefine\\|Kind\\|Type\\|Split\\|Close\\).*"
      cmd) "Back.")
    (t "undo.")))

(defun abella-find-and-forget-fn (span)
  (setq ans ())
  (while span
    (setq typ (span-property span 'type))
    (if (not (eq typ 'comment))
      (let ((current-cmd (abella-find-and-forget-cmd span)))
        (setq ans (cons current-cmd ans))))
    (setq span (next-span span 'type)))
  ans)

(defun proof-script-parse-find-comment-end ()
  "Find the end of the comment point is at the start of.  Nil if not found."
  (let ((notout t))
    ;; Find end of comment (NB: doesn't undertand nested comments)
    (while (and notout (re-search-forward
			proof-script-comment-end-regexp nil 'movetolimit))
      (setq notout (proof-buffer-syntactic-context)))
    ; hack to exclude the comment-end-regexp from the comment
    (re-search-forward
			proof-script-comment-end-regexp nil 'movetolimit -1)
    ; (end of the hack)
    (not (proof-buffer-syntactic-context))))



;;; abella.el ends here
