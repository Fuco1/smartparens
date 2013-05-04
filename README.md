# News

#### Experimental support for pairs with same opening and closing delimiter

Smartparens now support pairs with same opening and closing delimiter! That is, pairs like `$$` in latex are now recognized as any other `sexp`. There are still some problems in modes where the delimiters serve multiple purposes, like `*` in markdown (unary list item, binary emphatic text). Use the variable `sp-navigate-consider-stringlike-sexp` to set the major modes where these should be supported. `latex-mode` is added by default.

#### Experimental SGML tags support

Smartparens now has fully transparent support of sgml tags (html, xml etc.). You can use all the operations on these as if they were normal paired expressions. Use the variable `sp-navigate-consider-sgml-tags` to set the major modes where tags should be supported. `html-mode` is added by default.

#### Documentation moved

Since the readme file spanned more than 600 lines and grew very unorganized and difficult to manage, documentation has moved to [github smartparens wiki][wiki].

#### Default configuration

The default configuration was moved into a separate file. If you wish to use the default configuration as a basis for your own additional customization, add:

```scheme
(require 'smartparens-config)
```

in your configuration files (e.g. init.el) to load it. There are also files with additional configuration for specific modes, such as `smartparens-latex.el`. The usage is similar as with the default config.

# About Smartparens

Smartparens is modern minor mode for Emacs that *deals with parens pairs and tries to be smart about it*. It is a unification and enhancement effort to combine funcitonality of several existing packages in a single, common and straightforward way (and most of all *compatible*). These packages include [autopair](https://github.com/capitaomorte/autopair), [textmate](http://code.google.com/p/emacs-textmate/), [wrap-region](https://github.com/rejeep/wrap-region), [paredit](http://emacswiki.org/emacs/ParEdit) and others with similar philosophies. It also adds support for many more features. [Here's][wiki-what] a highlight of some features, for a complete list and detailed documentation look in the [manual][wiki-new].

For the complete picture of what is it about, visit the [documentation wiki][wiki].

[wiki]: https://github.com/Fuco1/smartparens/wiki
[wiki-what]: https://github.com/Fuco1/smartparens/wiki#wiki-what-is-this-package-about?
[wiki-new]: https://github.com/Fuco1/smartparens/wiki#wiki-information-for-new-users

# Support the project

If you want to support this project, you can do it in the following ways:

* Contribute code. If you have an idea that is not yet implemented and will benefit smartparens, feel free to implement it and submit a pull request. If you have any concerns whether your contribution will be accepted, ask beforehand. You can email the author or [start an issue](https://github.com/Fuco1/smartparens/issues/new) on the tracker.
* Contribute ideas. Even if you can't code Emacs LISP, you can still contribute valuable ideas for other programmers to implement. Simply [start new issue](https://github.com/Fuco1/smartparens/issues/new) on the tracker and submit your suggestion.
* You can make a [financial donation](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C) through PayPal. As much as I hate to ask for money, I need to eat too. I currently double-major in computer science and mathematics, having no full-time job as the school eats most my day time. If you like smartparens and can spare a modest amount on a donation, feel free to do so. Regardless of the donations, smartparens will always be free both as in beer and as in speech.
