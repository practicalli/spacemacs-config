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
     ;;
     ;; Layers added in alphabetic order


     ;; Enable asciidoc layer for editing asciidoc content
     ;; Useful for docs.cider.mx editing
     asciidoc

     ;; Add tool tips to show doc string of functions
     ;; Show snippets in the autocompletion popup
     ;; Show suggestions by most commonly used
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     ;; To have auto-completion on as soon as you start typing
     ;; (auto-completion :variables auto-completion-idle-delay nil)

     ;; https://develop.spacemacs.org/layers/+lang/clojure/README.html
     (clojure :variables
              clojure-toplevel-inside-comment-form t
              cider-overlays-use-font-lock t
              clojure-enable-linters 'clj-kondo
              cider-preferred-build-tool 'clojure-cli)

     ;; SPC a L displays key and command history in a separate buffer
     command-log

     ;; Nyan cat tells you where you are in your file
     ;; :variables
     ;; colors-enable-nyan-cat-progress-bar (display-graphic-p)
     colors

     ;; Tools to work with comma separate values
     ;; Used for data science files
     ;; https://develop.spacemacs.org/layers/+lang/csv/README.html
     csv

     ;; For Spacemacs configuration files and packages
     emacs-lisp

     ;; Include emojis into everything
     emoji

     ;; SPC g s opens Magit git client full screen (q restores previous layout)
     ;; refine hunk 'all highlights characters changed on each line
     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk 'all)

     ;; SPC g h to use GitHub repositories
     ;; SPC g g to use GitHub Gists
     github

     ;; graphviz - open-source graph declaration system
     ;; Used to generated graphs of Clojure project dependencies
     ;; https://develop.spacemacs.org/layers/+lang/graphviz/README.html
     graphviz

     ;; GNU Global is a source code tagging system
     ;; It queries symbol locations in source code, such as definitions or references
     ;; `sudo apt install ctags` for Clojure support
     ;; https://develop.spacemacs.org/layers/+tags/gtags/README.html
     ;; (gtags :variables
     ;;        gtags-enable-by-default t)

     ;; helm-follow-mode sticky - remembers use of C-c C-f
     ;; - follow mode previews when scrolling through a helm list
     ;; (setq helm-follow-mode-persistent t)
     (helm :variables
           helm-follow-mode-persistent t)

     html
     ;; javascript
     json

     ;; Clojure specific configuration in dotspacemacs/user-config
     ;; lsp

     markdown

     ;; Editing multiple lines of text concurrently
     ;; `g r' menu in Emacs normal state
     multiple-cursors

     ;; Configuration: https://github.com/seagle0128/doom-modeline#customize
     (spacemacs-modeline :variables
                         doom-modeline-height 12
                         doom-modeline-major-mode-color-icon t
                         doom-modeline-buffer-file-name-style 'relative-to-project
                         doom-modeline-display-default-persp-name t
                         doom-modeline-minor-modes nil
                         doom-modeline-modal-icon nil)

     ;; buffer-position word-count parrot selection-info

     ;; Spacemacs Org mode
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-want-todo-bindings t
          org-enable-org-journal-support t
          org-journal-dir "~/projects/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format ""
          org-journal-carryover-items "TODO=\"TODO\"|TODO=\"DOING\"|TODO=\"BLOCKED\"|TODO=\"REVIEW\"")


     ;; Text-based file manager with preview
     ;; SPC a r
     (ranger :variables
             ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-cleanup-on-disable t
             ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))

     ;; SPC ' runs eshell in a popup buffer
     ;; To run your terminal shell, add
     ;; shell-default-shell 'multi-term
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)

     ;; spacemacs-layouts layer added to set variables
     ;; SPC TAB restricted to current layout buffers
     ;; Kill buffers when killing layer - SPC l x
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove 'kill-weak)

     ;; Spell as you type with Flyspell package,
     ;; requires external command - ispell, hunspell, aspell
     ;; SPC S menu, SPC S s to check current word
     spell-checking

     ;; Use original flycheck fringe bitmaps
     (syntax-checking :variables
                      syntax-checking-use-original-bitmaps t)

     ;; Visual file manager - `SPC p t'
     ;; treemacs-no-png-images t removes file and directory icons
     (treemacs :variables
               treemacs-indentation 1
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t)

     ;; Customise the Spacemacs themes
     ;; https://develop.spacemacs.org/layers/+themes/theming/README.html
     ;; Code in dotspacemacs/user-init to reduce size of modeline
     ;; theming

     ;; Support font ligatures (fancy symbols) in all modes
     ;; 'prog-mode for only programming languages
     ;; including text-mode may cause issues with org-mode and magit
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(prog-mode))

     ;; Highlight changes in buffers
     ;; SPC g . transient state for navigating changes
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     yaml

     ) ;; End of dotspacemacs-configuration-layers


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

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

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

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

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner "~/.spacemacs.d/banners/practicalli-logo.svg"

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 9)
                                (todos . 9)
                                (projects . 7)
                                (bookmarks . 24))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'org-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message "Scratch Buffer in Org-mode"

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)

   dotspacemacs-themes '(doom-gruvbox-light
                         doom-solarized-light
                         doom-sourcerer
                         kaolin-valley-dark
                         doom-solarized-dark
                         spacemacs-light
                         spacemacs-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Fira Code"
                               :size 16.0
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Global"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

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
   dotspacemacs-loading-progress-bar nil

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

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100

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
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:visual t
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   pdf-view-mode
                               :size-limit-kb 1000)


   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert mode). Currently the keyboard layouts
   ;; (qwerty-us qwertz-de) are supported.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

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

  ;; custom theme modification
  ;; - overriding default height of modeline
  (setq-default
    theming-modifications
      '((spacemacs-light
          (mode-line :height 0.92)
          (mode-line-inactive :height 0.92))
        (doom-solarized-light
         (mode-line :height 0.92)
         (mode-line-inactive :height 0.92))
        (doom-gruvbox-light
         (mode-line :height 0.80)
         (mode-line-inactive :height 0.92))))

  )  ;; End of dotspacemacs/user-int

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
  ;; Emacs text rendering optimizations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

  ;; Only render text left to right
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Disable Bidirectional Parentheses Algorithm
  (if (version<= "27.1" emacs-version)
      (setq bidi-inhibit-bpa t))

  ;; Files with known long lines
  ;; SPC f l to open files literally to disable most text processing

  ;; So long mode when Emacs thinks a file would affect performance
  (if (version<= "27.1" emacs-version)
      (global-so-long-mode 1))

  ;; End of: Emacs text rendering optimizations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Doom theme settings
  (setq doom-gruvbox-light-variant "hard")
  ;;
  (defun practicalli/setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'practicalli-modeline 'default))
  ;;
  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-modeline 'practicalli-modeline
      '(workspace-name window-number modals persp-name buffer-info remote-host vcs)
      '(repl debug lsp process matches checker buffer-position word-count parrot selection-info misc-info))
    (practicalli/setup-custom-doom-modeline))
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; User key bindings
  ;;
  ;; org-journal user keybinding
  ;; - create a new journal entry
  (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
  ;;
  ;; Toggle workspaces forward/backwards
  (spacemacs/set-leader-keys "ow" 'eyebrowse-next-window-config)
  (spacemacs/set-leader-keys "oW" 'eyebrowse-last-window-config)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Over-ride Spacemacs defaults
  ;;
  ;;
  ;; Set new location for file bookmarks, SPC f b
  ;; Default: ~/.emacs.d/.cache/bookmarks
  (setq bookmark-default-file "~/.spacemacs.d/bookmarks")
  ;;
  ;;
  ;; Set new location for recent save files
  ;; Default: ~/.emacs.d/.cache/recentf
  (setq bookmark-default-file "~/.spacemacs.d/recentf")
  ;;
  ;;
  ;; native line numbers taking up lots of space?
  (setq-default display-line-numbers-width nil)
  ;;
  ;;
  ;; replace / search with helm-swoop in Evil normal state
  (evil-global-set-key 'normal "/" 'helm-swoop)
  ;;
  ;;
  ;; Do not highlight trailing whitespace
  ;; - whitespace deleted on save using: dotspacemacs-whitespace-cleanup 'all
  (setq spacemacs-show-trailing-whitespace nil)
  ;;
  ;;
  ;; Open ranger with the minus keybinding - not working
  ;; Currently opens with deer
  ;; (setq ranger-enter-with-minus t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit - forge configuration
  ;;
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  (setq auth-sources '("~/.authinfo.gpg"))
  ;;
  ;; Configure number of topics show, open and closed
  ;; use negative number to toggle the view of closed topics
  ;; using `SPC SPC forge-toggle-closed-visibility'
  (setq  forge-topic-list-limit '(100 . -10))
  ;; set closed to 0 to never show closed issues
  ;; (setq  forge-topic-list-limit '(100 . 0))
  ;;
  ;; GitHub user and organization accounts owned
  ;; used by @ c f  to create a fork
  (setq forge-owned-accounts
        '(("practicalli" "jr0cket"
           "ClojureBridgeLondon" "ldnclj"
           "clojure-hacks"
           "reclojure")))
  ;; To blacklist specific accounts,
  ;; over-riding forge-owned-accounts
  ;; (setq forge-owned-blacklist
  ;;       '(("bad-hacks" "really-bad-hacks")))
  ;;
  ;; End of Magit - forge configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Safe structural editing
  ;; for all major modes
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
  ;; for clojure layer only (comment out line above)
  ;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-clojure-mode)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Version Control configuration - Git, etc
  ;;
  ;; diff-hl - diff hightlights in right gutter as you type
  (diff-hl-flydiff-mode)
  ;;
  ;; Load in magithub features after magit package has loaded
  ;; (use-package magithub
  ;;   :after magit
  ;;   :config (magithub-feature-autoinject t))
  ;;
  ;; Use Spacemacs as the $EDITOR (or $GIT_EDITOR) for git commits messages
  ;; when using git commit on the command line
  (global-git-commit-mode t)
  ;;
  ;; Set locations of all your Git repositories
  ;; with a number to define how many sub-directories to search
  ;; `SPC g L' - list all Git repositories in the defined paths,
  (setq magit-repository-directories
        '(("~/.emacs.d"  . 0)
          ("~/projects/" . 2)))
  ;;
  ;; end of version control configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-mode configuration
  ;;
  ;; I should write a toggle function to show descriptive or literate links in Org-mode
  ;;(setq org-descriptive-links nil)
  ;;
  ;; Org-reveal - define were reveal.js files can be found
  ;; (I place reveal.js files in same directory as I write the org files)
  (setq org-reveal-root "")
  ;;
  ;; Define the location of the file to hold tasks
  (with-eval-after-load 'org
    (setq org-default-notes-file "~/Dropbox/todo-list.org"))
  ;;
  ;; Define a kanban style set of stages for todo tasks
  (with-eval-after-load 'org
    (setq org-todo-keywords
         '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))))
  ;;
  ;; The default keywords all use the same colour.
  ;; Make the states easier to distinguish by using different colours
  ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
  ;; Setting colours (faces) using the `org-todo-keyword-faces' defcustom function
  ;; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el#L376
  ;; Using `with-eval-after-load' as a hook to call this setting when org-mode is run
  ;;
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          '(("TODO" . "SlateGray")
            ("DOING" . "DarkOrchid")
            ("BLOCKED" . "Firebrick")
            ("REVIEW" . "Teal")
            ("DONE" . "ForestGreen")
            ("ARCHIVED" .  "SlateBlue"))))
  ;;
  ;;
  ;; Set TODO keyword faces if over-ridden by theme.
  (defun practicalli/set-todo-keyword-faces ()
    (interactive)
    (setq hl-todo-keyword-faces
          '(("TODO" . "SlateGray")
            ("DOING" . "DarkOrchid")
            ("BLOCKED" . "Firebrick")
            ("REVIEW" . "Teal")
            ("DONE" . "ForestGreen")
            ("ARCHIVED" .  "SlateBlue"))))
  ;;
  ;;
  ;; Progress Logging
  ;; When a TODO item enters DONE, add a CLOSED: property with current date-time stamp
  (with-eval-after-load 'org
    (setq org-log-done 'time))
  ;;
  ;;
  ;; customize org-mode's checkboxes with unicode symbols
  (add-hook
   'org-mode-hook
   (lambda ()
     "Beautify Org Checkbox Symbol"
     (push '("[ ]" . "☐") prettify-symbols-alist)
     (push '("[X]" . "☑" ) prettify-symbols-alist)
     (push '("[-]" . "❍" ) prettify-symbols-alist)
     (prettify-symbols-mode)))
  ;;
  ;; Markdown mode hook for orgtbl-mode minor mode
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  ;;
  ;; Turn on visual-line-mode for Org-mode only
  ;; (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  ;;
  ;; use org-re-reveal instead of org-reveal (which hasnt been updated in ages and breaks org-mode 9.2)
  ;; (use-package org-re-reveal :after org)
  ;;
  ;; End of Org-mode Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure configurations
  ;;
  ;;
  ;; CIDER 0.23 Lima release options
  ;; Configure the position of evaluation result
  ;; By default the result displays at the end of the current line
  ;; Set cider-result-overlay-position to `at-point' to display results right after the expression evaluated
  ;; Useful for evaluating nexsted expressions with `, e e'
  (setq cider-result-overlay-position 'at-point)
  ;;
  ;;
  ;; Pretty print in Clojure to use the Fast Idiomatic Pretty-Printer. This is approximately 5-10x faster than clojure.core/pprint
  (setq cider-pprint-fn 'fipp)
  ;;
  ;;
  ;; Indentation of function forms
  ;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
  (setq clojure-indent-style 'align-arguments)
  ;;
  ;; Vertically align s-expressions
  ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
  (setq clojure-align-forms-automatically t)
  ;;
  ;; Auto-indent code automatically
  ;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  ;;
  ;;
  ;; Local Clojure and Java sources
  ;; Extract the clojure-x.x.x-sources.jar and Java src.zip files
  ;; Extracted files enable use of search tools (ripgrep, ag).
  ;; https://docs.cider.mx/cider/config/basic_config.html#_use_a_local_copy_of_the_java_source_code
  ;; (setq cider-jdk-src-paths '("~/projects/java/clojure-1.10.1-sources"
  ;;                             "~/projects/java/openjdk-11/src"))
  ;;
  ;;
  ;; anakondo - static analysis using clj-kondo
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; https://github.com/didibus/anakondo
  ;; Provides auto-completion without the need for a REPL
  ;; Add anakondo to `dotspacemacs-additional-packages` list
  ;;
  ;; `SPC SPC anakondo-minor-mode' to run manually for the current project.
  ;;
  ;; Commented until static analysis is an optional or background process
  ;; https://github.com/didibus/anakondo/issues/1
  ;;
  ;; Lazy load of anakondo until Clojure buffer is used
  ;; (autoload 'anakondo-minor-mode "anakondo")
  ;;
  ;; Enable anakondo-minor-mode in all Clojure buffers
  ;; (add-hook 'clojure-mode-hook #'anakondo-minor-mode)
  ;; Enable anakondo-minor-mode in all ClojureScript buffers
  ;; (add-hook 'clojurescript-mode-hook #'anakondo-minor-mode)
  ;; Enable anakondo-minor-mode in all cljc buffers
  ;; (add-hook 'clojurec-mode-hook #'anakondo-minor-mode)
  ;;
  ;;
  ;;
  ;; LSP server for Clojure with clj-kondo
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; An alternative approach to the Clojure layer variable clojure-enable-linters 'clj-kondo
  ;; for those environments where the clj-kondo binary does not run (eg. graal).
  ;; Uses a custom script to run the clj-kondo-lsp-server.jar which should be added
  ;; to the operating system path and include:
  ;; java -jar ~/path/to/clj-kondo-lsp-server-standalone.jar
  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :hook ((clojure-mode . lsp))
  ;;   :commands lsp
  ;;   :custom ((lsp-clojure-server-command '("clojure-lsp-server-clj-kondo")))
  ;;   :config (dolist  (m '(clojure-mode clojurescript-mode))
  ;;             (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))
  ;;
  ;;
  ;; TODO: review this binding - gives poor user experience
  ;; Multi-line editing in the REPL buffer
  ;; `RTN` creates a new line, `C-RTN` evaluates the code
  ;; Multi-line editing in the REPL buffer
  ;; (add-hook 'cider-repl-mode-hook
  ;;           '(lambda ()
  ;;              (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
  ;;              (define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)))
  ;;
  ;;
  ;; TODO: review this binding
  ;; repl history keybindings - not used - use s-<up> and s-<down> which are the defaults
  ;; (add-hook 'cider-repl-mode-hook
  ;;           '(lambda ()
  ;;              (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  ;;              (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)))
  ;;
  ;;
  ;; hook for command-line-mode - shows keybindings & commands in separate buffer
  ;; load command-line-mode when opening a clojure file
  ;; (add-hook 'clojure-mode-hook 'command-log-mode)
  ;;
  ;; turn on command-log-mode when opening a source code or text file
  ;; (add-hook 'prog-mode-hook 'command-log-mode)
  ;; (add-hook 'text-mode-hook 'command-log-mode)
  ;;
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
  ;; Evaluate code when it is contained in a (comment (,,,))
  ;; 24th sept - didnt work, even after updating spacemacs and packages
  ;; (setq cider-eval-toplevel-inside-comment-form t)
  ;;
  ;; (add-hook 'clojure-mode-hook
  ;;           '(setq cider-eval-toplevel-inside-comment-form t))
  ;;
  ;;
  ;; Toggle view of a clojure `(comment ,,,) block'
  ;;
  (defun clojure-hack/toggle-comment-block (arg)
    "Close all top level (comment) forms. With universal arg, open all."
    (interactive "P")
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^(comment\\>" nil 'noerror)
        (call-interactively
         (if arg 'evil-open-fold
           'evil-close-fold)))))
  ;;
  (evil-define-key 'normal clojure-mode-map
    "zC" 'clojure-hack/toggle-comment-block
    "zO" (lambda () (interactive) (clojure-hack/toggle-comment-block 'open)))
  ;;
  ;;
  ;; Experiment: Start Clojure REPL with a specific profile
  ;; https://stackoverflow.com/questions/18304271/how-do-i-choose-switch-leiningen-profiles-with-emacs-nrepl
  ;;
  ;; (defun start-cider-repl-with-profile ()
  ;;   (interactive)
  ;;   (letrec ((profile (read-string "Enter profile name: "))
  ;;            (lein-params (concat "with-profile +" profile " repl :headless")))
  ;;     (message "lein-params set to: %s" lein-params)
  ;;     (set-variable 'cider-lein-parameters lein-params)
  ;;     (cider-jack-in)))
  ;;
  ;; My altered more idiomatic version, hopefully
  ;; - seems to be a bug...
  ;; (defun start-cider-repl-with-profile (profile)
  ;;   (interactive "sEnter profile name: ")
  ;;   (letrec ((lein-params (concat "with-profile +" profile " repl :headless")))
  ;;     (message "lein-params set to: %s" lein-params)
  ;;     (set-variable 'cider-lein-parameters lein-params)
  ;;     (cider-jack-in)))
  ;;
  ;;
  ;; Hook for command-log-mode
  ;; shows keybindings & commands in separate buffer
  ;; Load command-log-mode when opening a clojure file
  ;; (add-hook 'clojure-mode-hook 'command-log-mode)
  ;;
  ;; Turn on command-log-mode when opening a source code or text file
  ;; (add-hook 'prog-mode-hook 'command-log-mode)
  ;; (add-hook 'text-mode-hook 'command-log-mode)
  ;;
  ;;
  ;; end of clojure configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web-mode configuration
  ;;
  ;; Changing auto indent size for languages in html layer (web mode) to 2 (defaults to 4)
  (defun web-mode-indent-2-hook ()
    "Indent settings for languages in Web mode, markup=html, css=css, code=javascript/php/etc."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset  2)
    (setq web-mode-code-indent-offset 2))
  ;;
  (add-hook 'web-mode-hook  'web-mode-indent-2-hook)
  ;;
  ;; End of Web-mode configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eshell visual enhancements
  ;;
  ;; Add git status visual labels
  ;;
  (require 'dash)
  (require 's)
  ;;
  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))
  ;;
  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))
  ;;
  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))
  ;;
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))
  ;;
  ;;
  ;; Looking for unicode icons on Emacs
  ;; `list-character-sets' and select unicode-bmp
  ;; scroll through bitmaps list to find the one you want
  ;; some bitmaps seem to change
  ;;
  ;; "\x26A5 "  (female-male symbol)
  ;; "\xf394"   (non-binary)
  ;; "\xf105"     (docker - changes)
  ;; "\xf105"   (leiningen - changes)
  ;; "\xe919"   (clojure logo - ??)
  ;; "\xf104"   (clojurescript logo - changes)
  ;; "\xf09b"   (github octocat)
  ;; "\xf397"  (git branch)
  ;; "\xf126"    (was git fork, changes..)
  ;; "\xf1d3"  ;  (git icon - changes)
  ;; "\xf5b0"   (git merge)
  ;; "\xf07b" 
  ;; "\xf114"   (closed folder - changes)
  ;; "\xf115"   (open folder - changes)
  ;; "\xf074" 
  ;; "\xe97c" 
  ;; "\xe943"  
  ;; "\xe566"  
  ;; "\xe422"  
  ;; "\xe907"  ; 
  ;; "\xe91b"  ;  
  ;; "\xf126"    (was git fork, changes..)
  ;; "\xf1d3"  ;  (git icon - changes)
  ;;
  ;;
  (esh-section esh-dir
               "\xf07c"  ;  (faicon folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "olive" :bold bold :underline t))
  ;;
  (esh-section esh-git
               "\xf397"  ;  (git branch icon)
               (magit-get-current-branch)
               '(:foreground "maroon"))
  ;;
  ;; (esh-section esh-python
  ;;              "\xe928"  ;  (python icon)
  ;;              pyvenv-virtual-env-name)
  ;;
  (esh-section esh-clock
               ""  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))
  ;;
  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))
  ;;
  ;;
  ;; "\xf0c9"  ;  (list icon)
  (esh-section esh-num
               "\x2130"  ;  ℰ (eshell icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))
  ;;
  ;; Separator between esh-sections
  (setq esh-sep " ")  ; or " | "
  ;;
  ;; Separator between an esh-section icon and form
  (setq esh-section-delim "")
  ;;
  ;; Eshell prompt header
  (setq esh-header "\n ")  ; or "\n┌─"
  ;;
  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp " \x2130 ")   ; or "└─> "
  (setq eshell-prompt-string " \x2130 ")   ; or "└─> "
  ;;
  ;; Choose which eshell-funcs to enable
  ;; (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))
  ;; (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))
  (setq eshell-funcs (list esh-dir esh-git))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

  ;; End of Eshell
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



  ;; Hacking spacebind
  ;; TODO: How to over-ride name of spacebind defined key?
  ;; The following does not work
  ;; (spacemacs/set-leader-keys "b C-d" 'spacemacs/kill-matching-buffers-rudely "Hackme")
  ;; (spacemacs/set-leader-keys "b C-d" 'spacemacs/kill-matching-buffers-rudely "Hackme")

  ;; (spacemacs|spacebind
  ;;  "Encrypt / decrypt files with Easy PG"
  ;;  :global
  ;;  (("b" "Buffers"
  ;;    ("C-d" spacemacs/kill-matching-buffers-rudely "Rudely"))))

;; (spacemacs|spacebind
;;  "Compare buffers, files and directories."
;;  :global
;;  (("TAB" spacemacs/alternate-buffer "Last buffer")
;;   ("b" "Buffers"
;;    ("C-e" spacemacs/kill-matching-buffers-rudely "Kill rudely..."))))


  ;; (spacemacs|spacebind
  ;;  "Encrypt / decrypt files with Easy PG"
  ;;  :global
  ;;  (("a" "applications"
  ;;    ("g"  "easy pg"
  ;;     ("d" epa-decrypt-file "Decrypt file to...")
  ;;     ("D" epa-delete-keys  "Delete keys...")
  ;;     ("e" epa-encrypt-file "Encrypt file...")
  ;;     ("i" epa-insert-keys  "Insert keys...")
  ;;     ("k" epa-list-keys "List keys...")
  ;;     ("K" epa-list-secret-keys "List secret keys...")
  ;;     ("x" epa-export-keys "Export keys...")
  ;;     ("s"  "sign"
  ;;      ("f" epa-sign-file "Sign file...")
  ;;      ("m" epa-sign-mail "Sign mail...")
  ;;      ("r" epa-sign-region "Sign region..."))
  ;;     ("v"  "verify"
  ;;      ("f" epa-verify-file "Verify file...")
  ;;      ("r" epa-verify-region "Verify region...")
  ;;      ("c" epa-verify-cleartext-in-region "Verify cleartext region..."))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; (add-hook 'persp-mode-hook
  ;;           (lambda ()
  ;;             (persp-load-state-from-file (expand-file-name "~/.emacs.d/.cache/layouts/persp-my-layout"))))
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Configuration no longer used
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Workarounds and bug fixes - temporary hopefully
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
  ;; Which-key now using a different sorting order for keybindings
  ;; which-key-sort-order 'which-key-prefix-then-key-order
  ;; https://github.com/syl20bnr/spacemacs/commit/ab3511cfb55aadaa7a13be03356917cca3071c02
  ;; (setq which-key-sort-order 'which-key-key-order-alpha)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spell checking
  ;; merged into Spacemacs `develop'
  ;;
  ;; Add keybinding to correct current word under the cursor
  ;; to the existing spelling menu, `S'
  ;; (spacemacs/set-leader-keys "Ss" 'flyspell-correct-at-point)
  ;;
  ;; Or in the user-binding menu
  ;; (spacemacs/set-leader-keys "os" 'flyspell-correct-at-point)
  ;;
  ;; Documentation:
  ;; http://develop.spacemacs.org/doc/DOCUMENTATION.html#binding-keys
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Redundant configurations - Clojure
  ;;
  ;; disable the new enhanced ClojureScript code completion
  ;; Use the ClojureScript completion from earlier versions of CIDER if enhanced cljs completion is causing issues
  ;;
  ;; (setq cider-enhanced-cljs-completion-p nil)
  ;;
  ;; End of CIDER 0.23 Lima release options
  ;;
  ;;
  ;; In clojure-mode, treat hyphenated words as a single word.
  ;; Using `w' in Evil normal words gets stuck on names containing `->'
  ;; (add-hook 'clojure-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))
  ;; (add-hook 'clojure-mode-hook #'(lambda ()
  ;;                                  (dolist (c (string-to-list "-_>?:"))
  ;;                                    (modify-syntax-entry c "w"))))
  ;;
  ;; Alternative approach - using subword-mode
  ;;
  ;; Enabling CamelCase support for editing commands
  ;; https://cider.readthedocs.io/en/latest/additional_packages/#subword-mode
  ;; (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;;
  ;;
 ;;
  ;;
  ;;
  ;; Linting with clj-kondo
  ;; https://github.com/borkdude/clj-kondo/blob/master/doc/editor-integration.md#spacemacs
  ;;
  ;; Using clj-kondo by itself
  ;; (use-package clojure-mode
  ;;   :ensure t
  ;;   :config
  ;;   (require 'flycheck-clj-kondo))

  ;; Using clj-kondo with joker
  ;; (use-package clojure-mode
  ;;   :ensure t
  ;;   :config
  ;;   (require 'flycheck-joker)
  ;;   (require 'flycheck-clj-kondo)
  ;;   (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  ;;     (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
  ;;   (dolist (checkers '((clj-kondo-clj . clojure-joker)
  ;;                       (clj-kondo-cljs . clojurescript-joker)
  ;;                       (clj-kondo-cljc . clojure-joker)
  ;;                       (clj-kondo-edn . edn-joker)))
  ;;     (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))


  ;; TODO: Spacemacs pull request with these keybindings, updating REPL intro text with details
  ;; You can remove this message with the <M-x cider-repl-clear-help-banner> command.
  ;; You can disable it from appearing on start by setting
  ;; ‘cider-repl-display-help-banner’ to nil.
  ;; Cannot set the banner to another text, its hard coded in CIDER code.
  ;; (setq cider-repl-display-help-banner "Evaluate in the source code buffer for fun and profit!")


  ;; Merged into develop
  ;; (spacemacs/set-leader-keys-for-major-mode 'clojure "ei" 'cider-interrupt)

  ;; Experiment: Turn on all font locking options for Clojure
  ;; (setq cider-font-lock-dynamically t)
  ;;
  ;; configure clojurescript-jack-in to use the helper functions provided by lein-figwheel template
  ;; https://github.com/bhauman/lein-figwheel
  ;; fig-start will start figwheel and compile the clojurescript application
  ;; cljs-repl will connect emacs buffer to clojurescript repl created by figwheel
  ;;
  ;;
  ;; TODO: review this binding - CIDER takes care of this now
  ;; without this configuration, emacs command clojurescript-jack-in defaults to jvm rhino repl
  ;; if using a different clojurescript template you may require different function calls in the do expression
  ;; alternatively: set via m-x customize-variable cider-cljs-lein-repl
  ;; TODO: Is cider-cljs-lein-repl still required to be set?
  ;; Or is this just specific to those projects that have a user/fig-start and user/cljs-repl functions
  ;; (setq cider-cljs-lein-repl
  ;;      "(do
  ;;         (user/fig-start)
  ;;         (user/cljs-repl))")
  ;;
  ;; if you are not using figwheel template to configure funcitons in dev/core.clj
  ;; then use the full function calls
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;          (figwheel-sidecar.repl-api/start-figwheel!)
  ;;          (figwheel-sidecar.repl-api/cljs-repl))")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; unwanted features / bug workarounds
  ;;
  ;; opening recent files on spacemacs home page with mouse click
  ;; pastes contents of kill ring once file is open
  ;;
  ;; (define-key spacemacs-buffer-mode-map [down-mouse-1] nil)
  ;;
  ;;
  ;; (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)
  ;;
  ;; (define-key global-map (kbd "SPC S s") 'flyspell-correct-at-point)
  ;;
  ;; (define-key markdown-mode-map (kbd "SPC S s") #'flyspell-correct-at-point)
  ;;
  ;; helm opens a new frame when cursor in a buffer positioned underneath another
  ;; see my gist for details to add...
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; elpa stable repository
  ;; if you want to disable the elpa stable repository put this in your dotfile in the user-init function:
  ;; (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
  ;;   ("org" . "orgmode.org/elpa/") ("gnu" . "elpa.gnu.org/packages/")))
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
  ;; evil-cleverparens - now part of the clojure layer (develop branch)
  ;;
  ;; use the evil-cleverparens layer
  ;; https://github.com/luxbock/evil-cleverparens
  ;;
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


  )   ;; End of dot-spacemacs/user-config


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
