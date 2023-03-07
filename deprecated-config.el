;; Practicalli Spacemacs Config
;; - deprecated configuration


;; TODO: Review user configuration

;; (defun practicalli/setup-completion-for-lsp ()
  ;;   (remove-hook 'completion-at-point-functions #'cider-complete-at-point))

  ;; (add-hook 'cider-mode-hook 'practicalli/setup-completion-for-lsp)

  ;; (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))

  ;; (add-hook 'cider-mode-hook
  ;;           (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))


  ;; ---------------------------------------
  ;; Undo-tree configuration

  ;; Disable undo-tree and use Emacs native undo with evil-undo support
  ;; https://github.com/syl20bnr/spacemacs/issues/14406#issuecomment-851597233
  ;; (global-undo-tree-mode -1)
  ;; (evil-set-undo-system 'undo-redo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; ---------------------------------------
  ;; Keeping Helm history clean
  ;; (setq history-delete-duplicates t)
  ;; (setq extended-command-history
  ;;       (delq nil (delete-dups extended-command-history)))

  ;; ---------------------------------------



  ;; ---------------------------------------
  ;; Emacs text rendering optimizations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

  ;; Only render text left to right
  ;; (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Disable Bidirectional Parentheses Algorithm
  ;; (if (version<= "27.1" emacs-version)
  ;;     (setq bidi-inhibit-bpa t))

  ;; Files with known long lines
  ;; SPC f l to open files literally to disable most text processing

  ;; So long mode when Emacs thinks a file would affect performance
  ;; (if (version<= "27.1" emacs-version)
  ;;     (global-so-long-mode 1))

  ;; End of: Emacs text rendering optimizations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; User key bindings
  ;;
  ;; org-journal - create a new journal entry - `, j' in org-journal mode
  ;; (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
  ;;
  ;; Toggle workspaces forward/backwards
  ;; (spacemacs/set-leader-keys "ow" 'eyebrowse-next-window-config)
  ;; (spacemacs/set-leader-keys "oW" 'eyebrowse-last-window-config)

  ;; Revert buffer - loads in .dir-locals.el changes
  ;; (spacemacs/set-leader-keys "oR" 'revert-buffer)
  ;;
  ;; Keycast mode - show key bindings and commands in mode line
  ;; (spacemacs/set-leader-keys "ok" 'keycast-mode)

  ;; Replace Emacs Tabs key bindings with Workspace key bindings
  ;; (with-eval-after-load 'evil-maps
  ;;   (when (featurep 'tab-bar)
  ;;     (define-key evil-normal-state-map "gt" nil)
  ;;     (define-key evil-normal-state-map "gT" nil)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Over-ride Spacemacs defaults
  ;;
  ;; Set new location for file bookmarks, SPC f b
  ;; Default: SPACEMACSDIR.cache/bookmarks
  ;; (setq bookmark-default-file (concat dotspacemacs-directory "bookmarks"))
  ;;
  ;; Set new location for recent save files
  ;; Default: SPACEMACSDIR.cache/recentf
  ;; (setq recentf-save-file (concat dotspacemacs-directory "recentf") )
  ;;

;; dotspacemacs/user-init
;; custom theme modification
;; spacemacs - overriding default height of modeline
;; doom-gruvbox - subtle lsp symbol highlight
;; (setq-default
;;   theming-modifications
;;     '((spacemacs-light
;;         (mode-line :height 0.92)
;;         (mode-line-inactive :height 0.92))
;;       (doom-solarized-light
;;        (mode-line :height 0.92)
;;        (mode-line-inactive :height 0.92))
;;       (doom-gruvbox-light
;;        (lsp-face-highlight-read :background nil :weight bold)
;;        (command-log-command :foreground "firebrick")
;;        (command-log-key :foreground "dark magenta"))))



;; dotspacemacs/user-config

;; org-mode config

;; No longer using org-reveal
;; Org-reveal - define were reveal.js files can be found
;; Place reveal.js files in same directory as org-reveal content files)
;; (setq org-reveal-root "")

;; use org-re-reveal instead of org-reveal (which hasnt been updated in ages and breaks org-mode 9.2)
;; (use-package org-re-reveal :after org)
;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web-mode configuration
  ;;
  ;; Changing auto indent size for languages in html layer (web mode) to 2 (defaults to 4)
  ;; (defun web-mode-indent-2-hook ()
  ;;   "Indent settings for languages in Web mode, markup=html, css=css, code=javascript/php/etc."
  ;;   (setq web-mode-markup-indent-offset 2)
  ;;   (setq web-mode-css-indent-offset  2)
  ;;   (setq web-mode-code-indent-offset 2))
  ;;
  ;; (add-hook 'web-mode-hook  'web-mode-indent-2-hook)
  ;;
  ;; End of Web-mode configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shell configuration
  ;;
  ;; Use zsh for default multi-term shell
  ;; (setq multi-term-program "/usr/bin/zsh")
  ;;
  ;; End of Shell configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MacOSX
  ;; Disable touchpad zoom gestures
  ;;
  ;; (define-key global-map (kbd "<magnify-up>") nil)
  ;; (define-key global-map (kbd "<magnify-down>") nil)
  ;;
  ;; (defun practicalli-nothing ()
  ;;   (interactive)
  ;;   (message "Buttons are not toys") )
  ;;
  ;; (define-key global-map (kbd "<magnify-up>") 'practicalli-nothing)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spaceline Doom theme settings
  ;; https://seagle0128.github.io/doom-modeline/
  ;; Configuration set in layer variables
  ;;
  ;; Set height of the modeline - will resize to height of text
  ;; (setq doom-modeline-height 12)

  ;; The left hand bar in the modeline
  ;; setting to zero shows a large box outline
  ;; (setq doom-modeline-bar-width 1)

  ;; Determine style of current filename / path displayed
  ;; default: auto
  ;; (setq doom-modeline-buffer-file-name-style 'relative-to-project)

  ;; default perspective name displayed in the mode-line.
  ;; (setq doom-modeline-display-default-persp-name t)

  ;; Do not show buffer encoding
  ;; (setq doom-modeline-buffer-encoding nil)

  ;; display GitHub notifications (requires `ghub' package)
  ;; (setq doom-modeline-github t)
  ;; The interval of checking GitHub.
  ;; (setq doom-modeline-github-interval (* 30 60))

  ;; GNUs notifications - default t
  ;; (setq doom-modeline-gnus nil)

  ;; IRC notifications - default t
  ;; (setq doom-modeline-irc nil)

  ;; Environment versions - default t
  ;; (setq doom-modeline-env-version t)

  ;; Use ascii rather than icon for modal state (more specific)
  ;; Icon not changing for doom-solarized-light theme
  ;; - icon changes color for doom-gruvbox-light theme
  ;; (setq doom-modeline-modal-icon nil)
  ;; End of Spaceline Doom theme settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Configuration no longer used
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Workarounds and bug fixes - temporary hopefully
  ;;
  ;; From a community question - not advisable
  ;; Set a different key binding for evil lisp state
  ;; (with-eval-after-load 'evil-lisp-state
  ;;   (spacemacs/set-leader-keys "k" evil-lisp-state-map))
  ;;
  ;; evil-escape - switch between insert and normal state
  ;; (setq-default evil-escape-delay 0.2)
  ;; (setq-default evil-escape-key-sequence "fd")
  ;;
  ;; Undo history size limit, triggering garbage collection
  ;; Updating all defaults by a power of 10 (adding another zero at the end)
  ;; default in spacemacs is 80000
  ;; (setq undo-limit 400000)
  ;;
  ;; default in spacemacs is 120000
  ;; (setq undo-strong-limit 6000000)
  ;;
  ;; default in spacemacs is 12000000
  ;; (setq undo-strong-limit 60000000)
  ;;
  ;;
  ;; disable undo-tree as it seems to be loosing history
  ;; (global-undo-tree-mode -1)
  ;;
  ;; TODO: try explicitly saving history
  ;; (setq undo-tree-auto-save-history t)
  ;;
  ;; TODO: try setting undo-tree tmp files location
  ;; (setq undo-tree-history-directory-alist '(("." . "~/var/emacs/undo")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Literal Searching Configuration
  ;;
  ;; Literal search, rather than regex, in spacemacs search - helm-ag
  ;; (setq-default helm-grep-ag-command-option "-Q")
  ;;
  ;; End of Searching Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Systemd user service
  ;;
  ;; Use the exec-path-from-shell package to get PATH, MANPATH
  ;; and the environment variables from your zsh or bash rc-files.
  ;;
  ;; (setq exec-path-from-shell-variables
  ;;       (append exec-path-from-shell-variables
  ;;               (list "TERM"
  ;;                     "RUST_SRC_PATH"
  ;;                     "…"
  ;;                     )))
  ;; (exec-path-from-shell-initialize)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Neotree configuration
  ;;
  ;; Display neotree on the right rather than left (default)
  ;; (setq neo-window-position 'right)
  ;;
  ;; End of Neotree configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; old-school emacs style keybindings that i am replacing with nicer spacemacs alternatives
  ;;
  ;; jr0cket: text scaling keybindings - use spc z x =/-
  ;; (define-key global-map (kbd "c-+") 'text-scale-increase)
  ;; (define-key global-map (kbd "c--") 'text-scale-decrease)
  ;;
  ;; smartparens keybindings - use lisp-state, spc k menu instead
  ;; (define-key global-map (kbd "c-)") 'sp-forward-slurp-sexp)
  ;; (define-key global-map (kbd "c-(") 'sp-backward-slurp-sexp)
  ;; (define-key global-map (kbd "m-)") 'sp-forward-barf-sexp)
  ;; (define-key global-map (kbd "m-(") 'sp-backward-barf-sexp)
  ;;
  ;; jr0cket: keybindings for cycling buffers
  ;; use spc b n and spc b n instead
  ;; (global-set-key [c-prior] 'previous-buffer)
  ;; (global-set-key [c-next] 'next-buffer)
  ;;
  ;; jr0cket: remap multiple cursors to a pattern that is easier to remember
  ;; learn iedit mode instead (its fantastic)
  ;; (define-key global-map (kbd "c-c m c") 'mc/edit-lines)
  ;;
  ;; end of old-school bindings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
