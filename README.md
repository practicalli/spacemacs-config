# Practicalli Spacemacs (develop) configuration

This GitHub repository contains the current configuration to support the [Practicalli Spacemacs book](https://practical.li/spacemacs).  It contains the layers recommended for use for enhanced Clojure development experience as well as numerous tweaks for general [Spacemacs](https://github.com/syl20bnr/spacemacs/) usage.  The configuration also contains a number of snippets (code and configuration templates) for specific languages.

[![Spacemacs Practicalli - Interactive Clojure development with Emacs and CIDER](https://raw.githubusercontent.com/practicalli/graphic-design/live/book-covers/practicalli-spacemacs-book-banner.png)](https://practical.li/spacemacs)

# Requirements

* Spacemacs Develop (now the default branch)
* Emacs 27.2, 28.x or 29.0.50 (emacs-snapshot)
* [Fira Code fonts](https://github.com/tonsky/FiraCode)

> Ligature support works best in Emacs 28.x.  If experiencing errors in Emacs 27.x and cannot upgrade, commend the `unicode` layer configuration in `dotspacemacs-configuration-layers`
>
> Emacs 29.x works with ligatures, although each time Spacemacs starts a cache of the Unicode bindings is created, adding 20-30 seconds to startup time.  This seems to be due to an issue with pcache and Emacs 29.x


## Freedesktop.org XDG_CONFIG_HOME supported

`SPACEMACSDIR` set to `$XDG_CONFIG_HOME/spacemacs` allows your Spacemacs configuration to be used from the standard Freedesktop.org configuration location, by default `$HOME/.config`.

Set `SPACEMACSDIR` and `$XDG_CONFIG_HOME` via the `.profile` file for of your account on your Operating System, which supports desktop applicaiton launchers and terminal shells. Or to support only terminal shells, add those variables to the shell resource files (e.g. `.bashrc`, `.zshrc`, `.zshenv`) of your operating system.


## Recommended Command Line tools

There are several [command line tools that should be installed](https://practicalli.github.io/spacemacs/before-you-start/recommended-command-line-tools.html) to support the use of the Practicalli Spacemacs configuration.


## Using this configuration directly

Visit GitHub and fork the practicalli/spacemacs.d repository.  A fork is recommended so you can add your own customisations and save them to your own repository.

In a terminal window, use git clone to copy your fork to your computer.

```git clone git@github.com:practicalli/spacemacs.d.git ~/.spacemacs.d```

If you are happy to use my Spacemacs configuration as it is, then remove your `~/.spacemacs` file and start Emacs.

> Configuration is optimised for languages that are read left to right.  Change the configuration to use `(setq-default bidi-paragraph-direction 'right-to-left)` if you use right to left read languages.  Comment out this configuration if you use both.


## Use this configuration as an example

Clone this repository and review the configurations I have chosen, not everything may be to your preferred way of working.

The recommended approach is to use a diff tool, such as `ediff` in Emacs, `SPC D f` in Spacemacs, to compare my configuration with your own existing configuration.


## The main configuration file

The file `.spacemacs.d/init.el` is read by Spacemacs if the `.spacemacs` file does not exist.  You can used either file as your main Spacemacs configuration file (but obviously not both at the same time).  In this repository, the `.spacemacs` file is a symbolic link to `.spacemacs/init.el`.


## Getting help
Please join the [Clojurians Slack community](http://clojure.net/) and ask questions in either the `#practicalli` or `#spacemacs` channels.


Thank you

[@practical_li](https://twitter.com/practical_li)
