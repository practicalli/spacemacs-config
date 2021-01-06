;;; Commentary:
;; helm-ghq.el provides a helm interface to "ghq".
;; Based on https://raw.githubusercontent.com/masutaka/emacs-helm-ghq/master/helm-ghq.el

(require 'helm)
(require 'helm-mode)
(require 'helm-files)

(defgroup helm-ghq nil
  "ghq with helm interface"
  :prefix "helm-ghq-"
  :group 'helm)

(defcustom helm-ghq-command-ghq
  "ghq"
  "*A ghq command"
  :type 'string
  :group 'helm-ghq)

(defcustom helm-ghq-command-ghq-arg-root
  '("root")
  "*Arguments for getting ghq root path using ghq command"
  :type '(repeqt string)
  :group 'helm-ghq)

(defcustom helm-ghq-command-ghq-arg-list
  '("list" "--full-path")
  "*Arguments for getting ghq list"
  :type '(repeqt string)
  :group 'helm-ghq)

(defcustom helm-ghq-command-ghq-arg-update-repo
  '("get" "-u")
  "*Arguments for updating a repository"
  :type '(repeqt string)
  :group 'helm-ghq)

(defcustom helm-ghq-command-git
  "git"
  "*A git command"
  :type 'string
  :group 'helm-ghq)

(defcustom helm-ghq-command-git-arg-root
  '("config" "ghq.root")
  "*Arguments for getting ghq root path using git command"
  :type '(repeqt string)
  :group 'helm-ghq)

(defcustom helm-ghq-command-git-arg-ls-files
  '("ls-files")
  "*Arguments for getting file list in git repository"
  :type '(repeqt string)
  :group 'helm-ghq)

(defcustom helm-ghq-command-hg
  "hg"
  "*A hg command"
  :type 'string
  :group 'helm-ghq)

(defcustom helm-ghq-command-hg-arg-ls-files
  '("manifest")
  "*Arguments for getting file list in hg repository"
  :type '(repeqt string)
  :group 'helm-ghq)

(defcustom helm-ghq-command-svn
  "svn"
  "*A svn command"
  :type 'string
  :group 'helm-ghq)

(defcustom helm-ghq-command-svn-arg-ls-files
  '("list" "--recursive")
  "*Arguments for getting files (and directories) list in svn repository"
  :type '(repeqt string)
  :group 'helm-ghq)

(defun helm-ghq--open-dired (file)
  (dired (file-name-directory file)))

(defvar helm-ghq--action
  '(("Open File" . find-file)
    ("Open File other window" . find-file-other-window)
    ("Open File other frame" . find-file-other-frame)
    ("Open Directory" . helm-ghq--open-dired)))

(defvar helm-ghq-source
  (helm-build-sync-source "ghq"
    :candidates #'helm-ghq--list-candidates
    :match #'helm-ghq--files-match-only-basename
    :filter-one-by-one #'helm-ghq--filter-one-by-one
    :keymap helm-generic-files-map
    :help-message helm-generic-file-help-message
    :action helm-ghq--action)
  "Helm source for ghq.")

(define-obsolete-variable-alias 'helm-source-ghq
  'helm-ghq-source "1.8.0")

(defun helm-ghq--files-match-only-basename (candidate)
  "Allow matching only basename of file when \" -b\" is added at end of pattern.
If pattern contain one or more spaces, fallback to match-plugin
even is \" -b\" is specified."
  (let ((source (helm-get-current-source)))
    (if (string-match "\\([^ ]*\\) -b\\'" helm-pattern)
        (progn
          (helm-attrset 'no-matchplugin nil source)
          (string-match (match-string 1 helm-pattern)
                        (helm-basename candidate)))
      ;; Disable no-matchplugin by side effect.
      (helm-aif (assq 'no-matchplugin source)
          (setq source (delete it source)))
      (string-match
       (replace-regexp-in-string " -b\\'" "" helm-pattern)
       candidate))))

(defun helm-ghq--filter-one-by-one (candidate)
  (if helm-ff-transformer-show-only-basename
      (cons (helm-basename candidate) candidate)
    candidate))

(defmacro helm-ghq--line-string ()
  `(buffer-substring-no-properties
    (line-beginning-position) (line-end-position)))

(defun helm-ghq--root-fallback ()
  (erase-buffer)
  (unless (zerop (apply #'process-file
      helm-ghq-command-git nil t nil
      helm-ghq-command-git-arg-root))
    (error "Failed: Can't find ghq root"))
  (goto-char (point-min))
  (expand-file-name (helm-ghq--line-string)))

(defun helm-ghq--root ()
  (with-temp-buffer
    (apply #'process-file
     helm-ghq-command-ghq nil t nil
     helm-ghq-command-ghq-arg-root)
    (goto-char (point-min))
    (let ((output (helm-ghq--line-string)))
      (if (string-match-p "\\`No help topic" output)
          (helm-ghq--root-fallback)
        (expand-file-name output)))))

(defun helm-ghq--list-candidates ()
  (with-temp-buffer
    (unless (zerop (apply #'call-process
        helm-ghq-command-ghq nil t nil
        helm-ghq-command-ghq-arg-list))
      (error "Failed: Can't get ghq list candidates"))
    (let ((paths))
      (goto-char (point-min))
      (while (not (eobp))
  (push (helm-ghq--line-string) paths)
        (forward-line 1))
      (reverse paths))))

(defun helm-ghq--ls-files ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (or (zerop (apply #'call-process
            helm-ghq-command-git nil '(t nil) nil
            helm-ghq-command-git-arg-ls-files))
    (zerop (apply #'call-process
            helm-ghq-command-svn nil '(t nil) nil
            helm-ghq-command-svn-arg-ls-files))
    (zerop (apply #'call-process
            helm-ghq-command-hg nil t nil
            helm-ghq-command-hg-arg-ls-files)))
      (error "Failed: Can't get file list candidates"))))

(defun helm-ghq--source (repo)
  (let ((name (file-name-nondirectory (directory-file-name repo))))
    (helm-build-in-buffer-source name
      :init #'helm-ghq--ls-files
      :action helm-ghq--action)))

(defun helm-ghq--repo-to-user-project (repo)
  (cond ((string-match "github.com/\\(.+\\)" repo)
         (match-string-no-properties 1 repo))
        ((string-match "code.google.com/\\(.+\\)" repo)
         (match-string-no-properties 1 repo))))

(defsubst helm-ghq--concat-as-command (args)
  (mapconcat 'identity args " "))

(defun helm-ghq--update-repository (repo)
  (let* ((user-project (helm-ghq--repo-to-user-project repo))
   (command (helm-ghq--concat-as-command
       (list
        helm-ghq-command-ghq
        (helm-ghq--concat-as-command
         helm-ghq-command-ghq-arg-update-repo)
        user-project))))
    (async-shell-command command)))

(defun helm-ghq--source-update (repo)
  (helm-build-sync-source "Update Repository"
    :candidates '(" ") ; dummy
    :action (lambda (_c)
              (helm-ghq--update-repository repo))))

;;;###autoload
(defun helm-ghq ()
  (interactive)
  (let ((repo (helm-comp-read "ghq-list: "
                              (helm-ghq--list-candidates)
                              :name "ghq list"
                              :must-match t)))
    (let ((default-directory (file-name-as-directory repo)))
      (helm :sources (list (helm-ghq--source default-directory)
                           (helm-ghq--source-update repo))
            :buffer "*helm-ghq-list*"))))

(provide 'helm-ghq)

;;; helm-ghq.el ends here
