# Practicalli Spacemacs configuration

```none
██████╗ ██████╗  █████╗  ██████╗████████╗██╗ ██████╗ █████╗ ██╗     ██╗     ██╗
██╔══██╗██╔══██╗██╔══██╗██╔════╝╚══██╔══╝██║██╔════╝██╔══██╗██║     ██║     ██║
██████╔╝██████╔╝███████║██║        ██║   ██║██║     ███████║██║     ██║     ██║
██╔═══╝ ██╔══██╗██╔══██║██║        ██║   ██║██║     ██╔══██║██║     ██║     ██║
██║     ██║  ██║██║  ██║╚██████╗   ██║   ██║╚██████╗██║  ██║███████╗███████╗██║
╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝
```


User configuration to support the [Practicalli Spacemacs book](https://practical.li/spacemacs).

- recommended layers for use for enhanced Clojure development experience
- numerous tweaks for general [Spacemacs](https://github.com/syl20bnr/spacemacs/) usage
- snippets (code and configuration templates) for clojure and markdown languages

[![Spacemacs Practicalli - Interactive Clojure development with Emacs and CIDER](https://raw.githubusercontent.com/practicalli/graphic-design/live/book-covers/practicalli-spacemacs-book-banner.png)](https://practical.li/spacemacs)

# Requirements

[Practicalli Spacemacs - install spacemacs](https://practical.li/spacemacs/install-spacemacs/install/) details installation and pre-install requirements.


## Feedback & Contributing

Please follow the [contributing guide for all Practicalli books and configurations](https://practical.li/spacemacs/introduction/contributing/).  Thank you.


## Configuration Design

`dotspacemacs/user-config` section includes additional configuration and is defined across several files to make it easier to manage updates to the configuration over time and avoid merging changes with your own customisation.

Loaded configuration files:

* `clojure-config.el` - clojure-mode options, evil-cleverparents enable, portal tap> on nrepl & keybindings, custom elisp functions
* `theme-config.el` - theme and mode-line configuration
* `org-config.el` - notes and task faces and workflow
* `version-control-config.el` - git, Magit and Forge configuration (predominantly forge config)
* `user-config.el` - general config tweaks

> `eshell-config.el` defines a custom prompt for eshell, although this configuration file is not loaded.  Practicalli now uses vterm to use the operating system shell in a terminal popup window.

The `load-file` function includes the code from each file during startup.  Comment the `load-file` expression if that configuration is not required, or add your own configuration files to easily extend the Practicalli configuration without having to merge changes.

`.spacemacs.d/init.el` is main Spacemacs configuration file (although a $HOME/.spacemacs) file will supersede this configuration and should therefore be removed.


## Getting help

Ask questions via the [#spacemacs channel on the Clojurians slack](https://app.slack.com/client/T03RZGPFR/C099W16KZ)

> Join the [Clojurians Slack community](http://clojure.net/)


Thank you

[practical.li](https://practical.li/)
