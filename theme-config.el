;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; ---------------------------------------
;; Theme configuration
;; ---------------------------------------

;; ---------------------------------------
;; Theme Variants
;; (setq doom-gruvbox-light-variant "hard")
;; ---------------------------------------

;; ---------------------------------------
;; Doom-modeline
(defun practicalli/setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'practicalli-modeline 'default))
;;
(with-eval-after-load 'doom-modeline
  (doom-modeline-def-modeline 'practicalli-modeline
    '(workspace-name window-number modals persp-name buffer-info matches remote-host vcs)
    '(misc-info repl lsp buffer-position))
  (practicalli/setup-custom-doom-modeline))

;; Segments:
;; checker = flycheck results (not working)
;; buffer-position
;; word-count - number of words in current buffer
;; parrot
;; selection-info
;; repl - shows status of Cloure repl (not working)
;; process ??
;; debug
;; misc-info  - used for keycast
;; ---------------------------------------
