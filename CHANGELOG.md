# Changelog

# Unreleased

# 2023-04-04
## Added
- [Clojure Essentail Reference Emacs package](https://github.com/p3r7/clojure-essential-ref) to look up functions in book of same name, added key binding in Clojure mode `, h r`
- snippet: mkdocs fontawesome icons: book, github, youtube
- snippet: mkdocs full image form
- snippet: link to Practicalli spacemacs and doom emacs configurations

## Changed
- comment magit-delta-plugin as unstaged changes are not show if [delta](https://github.com/dandavison/delta) binary is not on the Emacs PATH, leave for user to enable and install tool
- snippet: comment header and section using --- lines rather than ;;;


# 2023-03-13
## Changed
- disable lsp hover and associated markdown doc generation call - enable clojure-mode to evaluate forms in rich comment as top level form

# 2023-03-08
## Changed
* renamed default branch to main
* renamed GitHub repository to practicalli/spacemacs-config
* [#7](https://github.com/practicalli/spacemacs-config/pull/7) Refactor dotspacemacs/user-config into individual files
* Update clojure and autocomplete layer variables for optimal use with LSP
* Update LSP variables to include peek menus for references, symbols, etc
* update git layer with git-delta plugin for enhanced diff highlights
* spacemacs-modeline - hide evil state icon (use cursor colour & shape for effective state tracking)
* Move unused configuration to deprecated-config.el

# 2022-12-26
## Added
* Spacemacs template updates
* MkDocs snippets
