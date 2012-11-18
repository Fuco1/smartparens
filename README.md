smartparens
===========

Modern lightweight smart parens/auto-insert/wrapping package for Emacs. This package combines functionality of packages like [autopair](https://github.com/capitaomorte/autopair), [textmate](http://code.google.com/p/emacs-textmate/), [wrap-region](https://github.com/rejeep/wrap-region), partially [paredit](http://emacswiki.org/emacs/ParEdit) and others. It adds support for many more features, some including:

* [x] support for pairs of any length (currently up to 10 characters), for example `"\\\\(" "\\\\)` for automatic insertion of quoted parens in elisp regexp. These are fully user definable and customizable. Pairs can be same or different for opening and closing part.
* [x] inteligent handling of closing pair. If user types `(`, `(|)` is inserted. If he then types `word)` the result is `(word)|` not `(word)|)`. This behaviour is cancelled if user moves backwards during editing or move point outside of the pair.
* [x] automatic deletion of whole pairs. With pair `("\{" "\}")` (LaTeX literal brackets), `\{|\}` and backspace will remove both of the pairs. `\{\}|` and backspace will remove the whole closing pair. `\{|` and backspace will remove the whole opening pair.
* [x] when followed by the same opening pair or word, do not insert the whole pair. That is: `|()` followed by `(` will produce `(|()` instead of `(|)()`. Similarly, `|word` followed by `(` will produce `(|word`.
* wraps region in defined pairs or defined tag pairs for "tag-modes" (xml/html...).
  * Different tags are supported, for example, languages that would use `{tag}` instead of `<tag>` or different opening pair and closing pair syntax, for example opening with `(tag` and closing with `)` (a.k.a. s-expression)
* automatically escape strings if wrapped with another string. `this "string"` turns to `"this \"string\""` automaticaly.
* automatically escape typed quotes inside a string

**All features** are fully customizable. You can turn every behaviour on or off for best user experience (yay buzzwords).

This is a developement version NOT ready for use yet. Features marked with [x] are somewhat completed.

Installation
===========

The basic setup is as follows:

    (require 'smartparens)
    (smartparens-global-mode 1)

You can disable smartparens in specific global modes by customizing `sp-ignore-mode-list`.

Pair management
===========

To define new pair, you can use the `sp-add-pair` function. Example:

    (sp-add-pair "\{" "\}") ;; latex literal brackes (included by default)
    (sp-add-pair "@@" ";") ;; what is this I have no idea...
    (sp-add-pair "{-" "-}") ;; haskell multi-line commands

Pairs defined by this function are used both for wrapping and auto insertion. However, you can disable certain pairs for auto insertion and only have them for wrapping, or other way.

Pairs have to be **prefix-free**, that means no opening pair should be a prefix of some other pair. This is reasonable and in fact necessary for correct function. For example, with autoinsertion of pair `"("  ")"` and pair `"(/"  "/)"` (which has as a prefix the one parens version), the program wouldn't know you might want to insert the longer version and simply inserts `(|)`. This can techincally be fixed with "look-ahead" and then backward alteration of input text, but it will be confusing and probably not very useful anyway.

(Removing is not yet supported. Optional arguments will be added to `sp-add-pair` to automatically disable pairs for autoinsertion/wrapping)

Auto pairing
===========

Autopairing of each pair can be enabled or disabled by variety of permissions. The basic order of evaluation is:

1. Globally allowed - each pair is by default allowed in every major mode
2. Locally banned - you can disable specific pair in specific major modes (for example ' pairing in lisp-related modes)
3. Globally banned - you can disable auto-pairing of a pair globally. The same pair can still be used for wrapping.
4. Locally allowed - you can enable a pair only in specific major modes. In other modes, it will be disabled automatically.

Each of the "next" levels overrides the previous.

(customization for this option is not implemented yet)

You can change many behaviours of the autopairing, see `customize-group` for available options.
