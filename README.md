Since the readme file spanned more than 600 lines and grew very unorganized and difficult to manage, documentation has moved to [github smartparens wiki][wiki].

# The changes in commit 168 broke the backward compatibility of the configuration interface. Please, check the documentation on wiki and update your configuration.

The default configuration was moved into a separate file. If you wish to use the default configuration as a basis for your own additional customization, add:

```scheme
(require 'smartparens-config)
```

in your configuration files (e.g. init.el) to load it.

I'm very sorry for the trouble, but the new interface is much more flexible and makes it much easier to configure SP and also to add future features. Thanks for understanding.

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
* You can make a financial donation. As much as I hate to ask for money, I need to eat too. I currently double-major in computer science and mathematics, having no full-time job as the school eats most my day time. If you like smartparens and can spare a modest amount on a donation, feel free to do so. Regardless of the donations, smartparens will always be free both as in beer and as in speech.

<form action="https://www.paypal.com/cgi-bin/webscr" method="post">
<input type="hidden" name="cmd" value="_s-xclick">
<input type="hidden" name="hosted_button_id" value="CEYP5YVHDRX8C">
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1">
</form>
