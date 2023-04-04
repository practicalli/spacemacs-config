;; ---------------------------------------
;; Clojure configurations

;; ---------------------------------------
;; Safe structural editing for all major modes
(spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
;; for clojure layer only (comment out line above)
;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-clojure-mode)
;;
;; ---------------------------------------


;; ---------------------------------------
;; Clojure-mode

;; Do not indent single ; comment character
(add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

;; Auto-indent code automatically
;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; Lookup functions in Clojure - The Essentail Reference book
;; https://github.com/p3r7/clojure-essential-ref
(spacemacs/set-leader-keys "oh" 'clojure-essential-ref)



;; ---------------------------------------
;; Portal Data Inspector

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(spacemacs/set-leader-keys "opp" 'portal.api/open)
(spacemacs/set-leader-keys "opc" 'portal.api/clear)
(spacemacs/set-leader-keys "opD" 'portal.api/close)

;; ---------------------------------------


;; ---------------------------------------
;; Clojure Essentail Ref lookup

(spacemacs/set-leader-keys-for-minor-mode 'clojure-mode "hr" 'clojure-essential-ref)

;; ---------------------------------------

;; ---------------------------------------
;; Custom functions

;; toggle reader macro sexp comment
;; toggles the #_ characters at the start of an expression
(defun clojure-toggle-reader-comment-sexp ()
  (interactive)
  (let* ((point-pos1 (point)))
    (evil-insert-line 0)
    (let* ((point-pos2 (point))
           (cmtstr "#_")
           (cmtstr-len (length cmtstr))
           (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
           (point-movement (if (string= cmtstr line-start) -2 2))
           (ending-point-pos (+ point-pos1 point-movement 1)))
      (if (string= cmtstr line-start)
          (delete-char cmtstr-len)
        (insert cmtstr))
      (goto-char ending-point-pos)))
  (evil-normal-state))
;;
;; Assign keybinding to the toggle-reader-comment-sexp function
(define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)
;;
;;
;; Toggle view of a clojure `(comment ,,,) block'
(defun clojure-hack/toggle-comment-block (arg)
  "Close all top level (comment) forms. With universal arg, open all."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^(comment\\>" nil 'noerror)
      (call-interactively
       (if arg 'evil-open-fold
         'evil-close-fold)))))

(evil-define-key 'normal clojure-mode-map
  "zC" 'clojure-hack/toggle-comment-block
  "zO" (lambda () (interactive) (clojure-hack/toggle-comment-block 'open)))
;; ---------------------------------------

;; ---------------------------------------
