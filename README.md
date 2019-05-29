# Practicalli Spacemacs configuration

This GitHub repository contains the current configuration to support the [Practicalli Spacemacs book](https://practicalli.github.io/spacemacs).  It contains the layers recommended for use for enhanced Clojure development experience as well as numerous tweaks for general [Spacemacs](https://github.com/syl20bnr/spacemacs/) usage.  The configuration also contains a number of snippets (code and configuration templates) for specific languages.

[![Practicalli Spacemacs book cover](https://practicalli.github.io/images/practicalli-spacemacs-book-cover.png)](https://practicalli.github.io/spacemacs)

It is not meant to be used for your own configuration, rather an example of some of the configuration options you could add to your own `~/.spacemacs` file.


## Using this configuration

Clone this repository and review the configurations I have chosen, not everything may be to your preferred way of working.

The recommended approach is to use a diff tool, such as `ediff` in Emacs, to compare my configuration with your own existing configuration.

I don't recommend you blindly copy it into your user directory.


## The main configuration file

The file `.spacemacs.d/init.el` is read by Spacemacs if the `.spacemacs` file does not exist.  You can used either file as your main Spacemacs configuration file (but obviously not both at the same time).  In this repository, the `.spacemacs` file is a symbolic link to `.spacemacs/init.el`.

## Getting help

Please join the [Clojurians Slack community](http://clojure.net/) and ask questions in either the `#practicalli` or `#spacemacs` channels.


Thank you

[@practical_li](https://twitter.com/practical_li)
