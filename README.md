Since the readme file spanned more than 600 lines and grew very unorganized and difficult to manage, documentation has moved to [github smartparens wiki][wiki].

# The changes in commit 168 broke the backward compatibility of the configuration interface. Please, check the documentation on wiki and update your configuration.

The default configuration was moved into a separate file. If you wish to use the default configuration as a basis for your own additional customization, add:

```scheme
(require 'smartparens-config)
```

in your configuration files (e.g. init.el) to load it.

I'm very sorry for the trouble, but the new interface is much more flexible and makes it much easier to configure SP and also to add future features. Thanks for understanding.

# About Smartparens

Smartparens is modern minor mode for Emacs that *deals with parens pairs and tries to be smart about it*. It is an unification and enhancement effort to combine funcitonality of several existing packages in a single, common and straightforward way (and most of all *compatible*). These packages include [autopair](https://github.com/capitaomorte/autopair), [textmate](http://code.google.com/p/emacs-textmate/), [wrap-region](https://github.com/rejeep/wrap-region), [paredit](http://emacswiki.org/emacs/ParEdit) and others with similar philosophies. It also adds support for many more features. [Here's][wiki-what] a highlight of some features, for a complete list and detailed documentation look in the [manual][wiki-new].

For the complete picture of what is it about, visit the [documentation wiki][wiki].

[wiki]: https://github.com/Fuco1/smartparens/wiki
[wiki-what]: https://github.com/Fuco1/smartparens/wiki#wiki-what-is-this-package-about?
[wiki-new]: https://github.com/Fuco1/smartparens/wiki#wiki-information-for-new-users
