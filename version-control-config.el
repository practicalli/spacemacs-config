;; ---------------------------------------
;; Version Control configuration - Git, etc
;;
;; https://develop.spacemacs.org/layers/+source-control/version-control/README.html
;; https://develop.spacemacs.org/layers/+source-control/git/README.html
;; Git Delta guide - https://dandavison.github.io/delta/
;; ---------------------------------------

;; ---------------------------------------
;; Spacemacs as $EDITOR (or $GIT_EDITOR) for commit messages
;; for `git commit` on command line
;; (global-git-commit-mode t)
;; ---------------------------------------


;; ---------------------------------------
;; Magit - forge configuration

;; Set locations of all your Git repositories
;; with a number to define how many sub-directories to search
;; `SPC g L' - list all Git repositories in the defined paths,
(setq magit-repository-directories
      '(("~/projects/" . 2)))

;; path for developer tokens (default ~/.authinfo)
;; Use XDG_CONFIG_HOME location or HOME
(setq auth-sources (list
                    (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                    "~/.authinfo.gpg"))


;; Number of topics shown, open and closed values
;; - negative number toggles view of closed topics
;; using `SPC SPC forge-toggle-closed-visibility'
;; - set closed to 0 to never show closed issues
;; (setq  forge-topic-list-limit '(100 . 0))
(setq  forge-topic-list-limit '(100 . -10))


;; GitHub user and organisation accounts owned
;; used by @ c f  to create a fork
(setq forge-owned-accounts
      '(("practicalli" "practicalli-john"
         "ClojureBridgeLondon" "ldnclj"
         "clojure-hacks")))

;; Blacklist Forge accounts
;; - over-rides forge-owned-accounts
;; (setq forge-owned-blacklist
;;       '(("bad-hacks" "really-bad-hacks")))
;; ---------------------------------------
