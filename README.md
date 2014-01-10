# Smartparens [![Build Status](https://travis-ci.org/Fuco1/smartparens.png?branch=master)](https://travis-ci.org/Fuco1/smartparens) [![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C)

Smartparens is minor mode for Emacs that *deals with parens pairs and tries to be smart about it*. It started as a unification effort to combine functionality of several existing packages in a single, compatible and extensible way to deal with parentheses, delimiters, tags and the like. Some of these packages include [autopair](https://github.com/capitaomorte/autopair), [textmate](http://code.google.com/p/emacs-textmate/), [wrap-region](https://github.com/rejeep/wrap-region), [electric-pair-mode](http://www.emacswiki.org/emacs/ElectricPair), [paredit](http://emacswiki.org/emacs/ParEdit) and others. With the basic features found in other packages it also brings many improvements as well as completely new features. [Here's][wiki-what] a highlight of some features, for a complete list and detailed documentation look in the [manual][wiki-new].

For the complete documentation visit the [documentation wiki][wiki].

[wiki]: https://github.com/Fuco1/smartparens/wiki
[wiki-what]: https://github.com/Fuco1/smartparens/wiki#wiki-what-is-this-package-about?
[wiki-new]: https://github.com/Fuco1/smartparens/wiki#wiki-information-for-new-users

# Default configuration

Smartparents provides many options to customize most aspects of its working.  After loading `smartparens.el`, you can customize many of these options using the customization interface: `M-x customize-group RET smartparens RET`.  Smartparens also ships with additional recommended (default) configuration, which is separated in file `smartparens-config.el` to make the maintainance easier.  If you wish to load the default configuration, add:

```scheme
(require 'smartparens-config)
```

in your configuration files (e.g. init.el) to load it.  There are also files with additional configuration for specific modes, such as `smartparens-latex.el` or `smartparens-ruby.el`.  You can load them the same way as the default config file.  Note however that the `smartparens-config.el` file will auto-load all the mode-speceific customizations.  It is a good idea to require this file if you are a new user, and later add or remove the options you don't like.

*Note: `smartparens-config` automatically loads the core smartparens files, so you don't have to require those separately.*

# Support the project

If you want to support this project, you can do it in the following ways:

* Contribute code. If you have an idea that is not yet implemented and will benefit smartparens, feel free to implement it and submit a pull request. If you have any concerns whether your contribution will be accepted, ask beforehand. You can email the author or [start an issue](https://github.com/Fuco1/smartparens/issues/new) on the tracker.
* Contribute ideas. Even if you can't code Emacs LISP, you can still contribute valuable ideas for other programmers to implement. Simply [start new issue](https://github.com/Fuco1/smartparens/issues/new) on the tracker and submit your suggestion.
* You can make a [financial donation](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C) through PayPal. I currently major in mathematics, having no full-time job as the school (and emacs :) eats most my day time. If you like smartparens or want a specific feature to be implemented and can spare a modest amount on a donation, feel free to do so. Regardless of the donations, smartparens will always be free both as in beer and as in speech.
