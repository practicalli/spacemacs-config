;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; Add tool tips to show doc string of functions
     ;; Show snippets in the autocompletion popup
     ;; Show suggestions by most commonly used
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     ;; To have auto-completion on as soon as you start typing
     ;; (auto-completion :variables auto-completion-idle-delay nil)

     ;; Enable clj-refactor tools
     (clojure :variables
              clojure-enable-clj-refactor t)
     ;; To add the sayid debugger, include the following as a variable above
     ;; clojure-enable-sayid t

     ;; Add the Joker linter for real-time linting in Clojure
     ;; Requires local install of Joker tool
     clojure-lint

     ;; Show commands as you type in a separate buffer
     command-log

     ;; Nyan cat tells you where you are in your file
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)

     ;; For Spacemacs configuration files and packages
     emacs-lisp

     ;; Include emojis into everything
     emoji

     ;; Open Magit git client full screen (quit restores previous layout)
     ;; Add github support (using magithub)
     ;; Highlight working copy changes
     (git :variables
          git-magit-status-fullscreen t
          git-enable-github-support t
          git-gutter-use-fringe t)

     ;; use magithub to talk to your github account and Github Gists
     github
     helm
     ;; (helm :variables
     ;;       helm-enable-auto-resize t
     ;;       helm-position 'top  ; top, bottom, left, right
     ;;       helm-use-frame-when-more-than-two-windows nil)
     html
     javascript
     markdown
     multiple-cursors
     treemacs
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support nil)
     (ranger :variables
             ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-cleanup-on-disable t
             ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-height 30
            shell-default-position 'bottom)     ;; SPC ' opens eshell in popup at bottome of Spacemacs

     spell-checking
     syntax-checking
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(org-re-reveal
                                      kaolin-themes
                                      ubuntu-theme)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 15)
                                (projects . 9)
                                (bookmarks . 24))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message "Scratch Buffer in Org-mode"

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         kaolin-valley-dark
                         kaolin-light
                         ubuntu)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Ubuntu Mono"
                               :size 24
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   org-mode
                                                   pdf-view-mode
                                                   text-mode
                               :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S: %a"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; old-school emacs style keybindings that i am replacing with nicer spacemacs alternatives

  ;; jr0cket: text scaling keybindings - use spc z x =/-
  ;; (define-key global-map (kbd "c-+") 'text-scale-increase)
  ;; (define-key global-map (kbd "c--") 'text-scale-decrease)

  ;; smartparens keybindings - use lisp-state, spc k menu instead
  ;; (define-key global-map (kbd "c-)") 'sp-forward-slurp-sexp)
  ;; (define-key global-map (kbd "c-(") 'sp-backward-slurp-sexp)
  ;; (define-key global-map (kbd "m-)") 'sp-forward-barf-sexp)
  ;; (define-key global-map (kbd "m-(") 'sp-backward-barf-sexp)

  ;; jr0cket: keybindings for cycling buffers
  ;; use spc b n and spc b n instead
  ;; (global-set-key [c-prior] 'previous-buffer)
  ;; (global-set-key [c-next] 'next-buffer)

  ;; jr0cket: remap multiple cursors to a pattern that is easier to remember
  ;; learn iedit mode instead (its fantastic)
  ;; (define-key global-map (kbd "c-c m c") 'mc/edit-lines)

  ;; end of old-school bindings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; evil-smartparens

  ;; https://github.com/expez/evil-smartparens
  ;; https://hindol.github.io/blog/fixing-smartparens-in-spacemacs-evil-mode/

  ;; add evil-smartparens-mode to smartparens
  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

  ;; end of evil-smartparens
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; evil-cleverparens - now part of the clojure layer (develop branch)

  ;; use the evil-cleverparens layer
  ;; https://github.com/luxbock/evil-cleverparens
  ;; https://github.com/luxbock/evil-cleverparens

  ;; add evil-cleverparens to clojure-mode
  ;; (spacemacs/toggle-evil-cleverparens-on)
  ;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  ;; end of evil-smartparens
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; exclude sayid as it currently does not support nrepl 0.4
  ;;
  ;; Temporary fix
  ;; (setq sayid-inject-dependencies-at-jack-in nil)
  ;; issue raised: https://github.com/syl20bnr/spacemacs/issues/11146
  ;;
  ;; pull request merged into develop to switch sayid off by default
  ;; https://github.com/bpiel/sayid/pull/40
  ;; enable sayid by adding this code to the .spacemacs dotspacemacs/layers configuration
  ;;   dotspacemacs-configuration-layers
  ;;    '(
  ;;       (clojure :variables clojure-enable-sayid t)
  ;;     )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; cider-refactor to be off by default
  ;;
  ;; @plexus to create a pull Request to disable clj-refactor by default
  ;; - it seems not many people use it and there are version conflicts occasionally
  ;;
  ;; Add this code to the .spacemacs configuration file
  ;;   dotspacemacs-configuration-layers
  ;;    '(
  ;;       (clojure :variables clojure-enable-clj-refactor t)
  ;;     )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; shell configuration

  ;; use zsh for default multi-term shell
  (setq multi-term-program "/usr/bin/zsh")

  ;; end of shell configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; version control configuration - git, etc

  ;; diff-hl - diff hightlights in right gutter as you type
  (diff-hl-flydiff-mode)

  ;; load in magithub features after magit package has loaded
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t))

  ;; use spacemacs as the $editor (or $git_editor) for git commits messages
  ;; when using git commit on the command line
  (global-git-commit-mode t)

  ;; end of version control configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-mode configuration

  ;; i should write a toggle function to show descriptive or literate links in org-mode
  ;;(setq org-descriptive-links nil)

  ;; org-reveal - define were reveal.js files can be found
  ;; (i place reveal.js files in same directory as i write the org files)
  (setq org-reveal-root "")

  ;; define the location of the file to hold tasks
  (with-eval-after-load 'org
    (setq org-default-notes-file "~/dropbox/todo-list.org"))

  ;; define a kanban style set of stages for todo tasks
  (with-eval-after-load 'org
    (setq org-todo-keywords
         '((sequence "todo" "doing" "blocked" "review" "|" "done" "archived"))))

  ;; setting colours (faces) for todo states to give clearer view of work
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
         '(("todo" . org-warning)
           ("doing" . "yellow")
           ("blocked" . "red")
           ("review" . "orange")
           ("done" . "green")
           ("archived" .  "blue"))))

  ;; progress logging
  ;; when a todo item enters done, add a closed: property with current date-time stamp
  (with-eval-after-load 'org
    (setq org-log-done 'time))

  ;; markdown mode hook for orgtbl-mode minor mode
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)

  ;; turn on visual-line-mode for org-mode only
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)

  ;; end of org-mode configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; searching configuration

  ;; literal search, rather than regex, in spacemacs search - helm-ag
  (setq-default helm-grep-ag-command-option "-q")

  ;; end of searching configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; web-mode configuration

  ;; update: variables in the html layer can be used rather than a hook

  ;; changing auto indent size for languages in html layer (web mode) to 2 (defaults to 4)
  ;; (defun jr0cket-web-mode-indent-hook ()
  ;;   "indent settings for languages in web mode, markup=html, css=css, code=javascript/php/etc."
  ;;   (setq web-mode-markup-indent-offset 2)
  ;;   (setq web-mode-css-indent-offset  2)
  ;;   (setq web-mode-code-indent-offset 2))

  ;; (add-hook 'web-mode-hook  'jr0cket-web-mode-indent-hook)

  ;; end of web-mode configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; clojure configurations


  ;; enable safe structural editing in evil (clojure layer - evil-cleverparens)
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-clojure-mode)

  ;; pretty print in clojure to use the fast idiomatic pretty-printer. this is approximately 5-10x faster than clojure.core/pprint
  (setq cider-pprint-fn 'fipp)

  ;; configure clojurescript-jack-in to use the helper functions provided by lein-figwheel template
  ;; https://github.com/bhauman/lein-figwheel
  ;; fig-start will start figwheel and compile the clojurescript application
  ;; cljs-repl will connect emacs buffer to clojurescript repl created by figwheel
  ;;
  ;; without this configuration, emacs command clojurescript-jack-in defaults to jvm rhino repl
  ;; if using a different clojurescript template you may require different function calls in the do expression
  ;; alternatively: set via m-x customize-variable cider-cljs-lein-repl
  (setq cider-cljs-lein-repl
       "(do
          (user/fig-start)
          (user/cljs-repl))")

  ;; if you are not using figwheel template to configure funcitons in dev/core.clj
  ;; then use the full function calls
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;          (figwheel-sidecar.repl-api/start-figwheel!)
  ;;          (figwheel-sidecar.repl-api/cljs-repl))")


  ;; repl history keybindings - not used - use s-<up> and s-<down> which are the defaults
  ;; (add-hook 'cider-repl-mode-hook
  ;;           '(lambda ()
  ;;              (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  ;;              (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)))


  ;; hook for command-line-mode - shows keybindings & commands in separate buffer
  ;; load command-line-mode when opening a clojure file
  ;; (add-hook 'clojure-mode-hook 'command-log-mode)

  ;; turn on command-log-mode when opening a source code or text file
  ;; (add-hook 'prog-mode-hook 'command-log-mode)
  ;; (add-hook 'text-mode-hook 'command-log-mode)

  ;; toggle reader macro sexp comment
  ;; toggles the #_ characters at the start of an expression
  (defun clojure-toggle-reader-comment-sexp ()
    (interactive)
    (let* ((point-pos1 (point)))
      (evil-insert-line 0)
      (let* ((point-pos2 (point))
             (cmtstr "#_")
             (cmtstr-len (length cmtstr))
             (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len))))
        (if (string= cmtstr line-start)
            (delete-char cmtstr-len)
          (insert cmtstr))
        (goto-char point-pos1))))


  (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)

  ;; end of clojure configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; systemd user service

  ;; use the exec-path-from-shell package to get path, manpath
  ;; and the environment variables from your zsh or bash rc-files.

  ;; (setq exec-path-from-shell-variables
  ;;       (append exec-path-from-shell-variables
  ;;               (list "term"
  ;;                     "rust_src_path"
  ;;                     "…"
  ;;                     )))
  ;; (exec-path-from-shell-initialize)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; neotree configuration

  ;; display neotree on the right rather than left (default)
  ;; (setq neo-window-position 'right)

  ;; end of neotree configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; golden ratio adjustment
  ;; (setq golden-ratio-adjust-factor 1.0
  ;;       golden-ratio-wide-adjust-factor 1.2)

  ;; end of golden ratio adjustment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; unwanted features / bug workarounds

  ;; opening recent files on spacemacs home page with mouse click
  ;; pastes contents of kill ring once file is open

  (define-key spacemacs-buffer-mode-map [down-mouse-1] nil)


  ;; helm opens a new frame when cursor in a buffer positioned underneath another
  ;; see my gist for details to add...
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; elpa stable repository
  ;; if you want to disable the elpa stable repository put this in your dotfile in the user-init function:                                                           │
  ;; (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
  ;;   ("org" . "orgmode.org/elpa/") ("gnu" . "elpa.gnu.org/packages/")))


  )   ;; End of dot-spacemacs/user-config


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (helm-git-grep string-inflection org-download impatient-mode helm-company evil-ediff editorconfig cider sesman clojure-mode iedit helm markdown-mode alert magit org-plus-contrib yasnippet-snippets yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit symon spaceline-all-the-icons smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters queue pug-mode prettier-js popwin persp-mode pcre2el password-generator paradox ox-reveal ox-gfm overseer orgit org-projectile org-present org-pomodoro org-mime org-bullets org-brain open-junk-file neotree nameless multi-term move-text mmm-mode markdown-toc magithub magit-svn magit-gitflow magit-gh-pulls macrostep lorem-ipsum log4e livid-mode linum-relative link-hint less-css-mode json-navigator json-mode js2-refactor js-doc indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gntp gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump dotenv-mode diminish diff-hl define-word counsel-projectile company-web company-tern company-statistics company-quickhelp company-emoji command-log-mode column-enforce-mode color-identifiers-mode clojure-snippets clojure-cheatsheet clj-refactor clean-aindent-mode cider-eval-sexp-fu centered-cursor-mode browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
