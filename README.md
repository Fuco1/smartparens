smartparens
===========

Modern lightweight smart parens/auto-insert/wrapping package for Emacs. This package combines functionality of packages like [autopair](https://github.com/capitaomorte/autopair), [textmate](http://code.google.com/p/emacs-textmate/), [wrap-region](https://github.com/rejeep/wrap-region), partially [paredit](http://emacswiki.org/emacs/ParEdit) and others. It adds support for many more features, some including:

* [x] support for pairs of any length (currently up to 10 characters), for example `"\\\\(" "\\\\)"` for automatic insertion of quoted parens in elisp regexp. These are fully user definable and customizable. Pairs can be same or different for opening and closing part.
* [x] inteligent handling of closing pair. If user types `(`, `(|)` is inserted. If he then types `word)` the result is `(word)|` not `(word)|)`. This behaviour is cancelled if user moves backwards during editing or move point outside of the pair.
* [x] automatic deletion of whole pairs. With pair `("\{" "\}")` (LaTeX literal brackets), `\{|\}` and backspace will remove both of the pairs. `\{\}|` and backspace will remove the whole closing pair. `\{|` and backspace will remove the whole opening pair.
* [x] when followed by the same opening pair or word, do not insert the whole pair. That is: `|()` followed by `(` will produce `(|()` instead of `(|)()`. Similarly, `|word` followed by `(` will produce `(|word`.
* [x] wraps region in defined pairs or defined tag pairs for "tag-modes" (xml/html...). (tags not implemented yet).
  * Different tags are supported, for example, languages that would use `{tag}` instead of `<tag>` or different opening pair and closing pair syntax, for example opening with `(tag` and closing with `)` (a.k.a. s-expression)
* automatically escape strings if wrapped with another string. `this "string"` turns to `"this \"string\""` automaticaly.
* [x] automatically escape typed quotes inside a string

**All features** are fully customizable via `M-x customize-group smartparens`. You can turn every behaviour on or off for best user experience (yay buzzwords).

This is still a developement pre 1.0 version. Features marked with [x] are fully implemented. Free to install it and report bugs or new features :)

Installation
===========

The basic setup is as follows:

    (require 'smartparens)
    (smartparens-global-mode 1)

If you've installed this as a package, you don't need to require it, as there is an autoload on `smartparens-global-mode`.

You can disable smartparens in specific global modes by customizing `sp-ignore-mode-list`. Of course, you can also only turn it on in specific modes via the hook mechanisms.

This package *depends* on [dash](https://github.com/magnars/dash.el). If you've installed smartparens via package-install, it should resolve dependencies automatically (dash is on melpa and marmalade). If not, you'd need to install it manually. See the installation information on their homepage.

*(Note: smartparens is not yet available as package, so you need to do manual installation for now)*

If you use `delete-selection-mode`, you **MUST** disable it and set appropriate emulation by smartparens. See the Wrapping section for more info.

See the last section for an example configuration.

Pair management
===========

To define new pair, you can use the `sp-add-pair` function. Example:

    (sp-add-pair "\{" "\}") ;; latex literal brackes (included by default)
    (sp-add-pair "@@" ";") ;; what is this I have no idea...
    (sp-add-pair "{-" "-}") ;; haskell multi-line comments

Pairs defined by this function are used both for wrapping and auto insertion. However, you can disable certain pairs for auto insertion and only have them for wrapping, or other way.

You can also automatically disable pair autoinsertion for newly added pair in certain major modes. Simply add the list of modes as additional arguments. You can also just specify the modes as arguments themselves:

    (sp-add-pair "\{" "\}" 'c-mode 'java-mode) ;; will disable \{ pair in c-mode and java-mode
    (sp-add-pair "@@" ";" '(c-mode java-mode)) ;; will disable @@ pair in c-mode and java-mode
    (sp-add-pair "{-" "-}" 'c-mode '(emacs-lisp-mode)) ;; will disable {- pair in c-mode and emacs-lisp-mode

The calling conventions will probably change after the wrapping is done. The first optional argument will serve for insertion ban, the rest will be for wrapping ban. Therefore, use the style from 2nd line (specify bans as a list).

Pairs have to be **prefix-free**, that means no opening pair should be a prefix of some other pair. This is reasonable and in fact necessary for correct function. For example, with autoinsertion of pair `"("  ")"` and pair `"(/"  "/)"` (which has as a prefix the one parens version), the program wouldn't know you might want to insert the longer version and simply inserts `(|)`. This can techincally be fixed with "look-ahead" and then backward alteration of input text, but it will be confusing and probably not very useful anyway.

*(I’ve found a way to fix this, so the prefix-free requirement will probably be removed in the future.)*

Pairs included by default:

    ("\\\\(" "\\\\)") ;; emacs regexp parens
    ("\\{" "\\}")
    ("\\(" "\\)")
    ("\\\"" "\\\"")
    ("/*" "*/")
    ("\"" "\"")
    ("'" "'")
    ("(" ")")
    ("[" "]")
    ("{" "}")
    ("`" "'") ;; tap twice for TeX double quote

You can remove pairs by calling `sp-remove-pair`. This will also automatically delete any assigned permissions!

    (sp-remove-pair "\{")
    (sp-remove-pair "'")

*(Customized pairs for major-modes will probably be supported too. This means overwriting a default pair. For example changing `' into \`\` in markdown-mode. I'm not sure on the actual mechanism, but I'd like the simplest possible one)*

Auto pairing
===========

Autopairing of each pair can be enabled or disabled by variety of permissions. The basic order of evaluation is:

1. Globally allowed - each pair is by default allowed in every major mode.
2. Locally banned - specific pair won't auto-pair in specific major modes (for example ' pairing in lisp-related modes).
3. Globally banned - specific pair won't auto-pair globally (= disabled in all major modes). The same pair can still be used for wrapping.
4. Locally allowed - specific pair only auto-pairs in specific major modes. In other modes, it will be disabled automatically.

Each of the "next" levels overrides the previous.

You can add local bans for a pair with `sp-add-local-ban-insert-pair` function:

    (sp-add-local-ban-insert-pair "'" '(ielm-mode calc-mode)) ;; disable '' pair in ielm/calc
    (sp-add-local-ban-insert-pair "\{" '(markdown-mode)) ;; disable \{\} in markdown mode

You can remove local bans with `sp-remove-local-ban-insert-pair` function. If called with no argument, remove all the modes.

    (sp-remove-local-ban-insert-pair "'") ;; re-enable '' in all the modes -- that is, remove all the bans
    (sp-remove-local-ban-insert-pair "\{" '(markdown-mode)) ;; re-enable \{\} in markdown mode, keep the rest of the bans

Similar functions work for the allow list. They are called `sp-add-local-allow-insert-pair` and `sp-remove-local-allow-insert-pair`. The calling conventions are the same.

In addition to these restrictions, you can also disable all or specific pairs only inside comments and strings (strings from now on) or only in code (everything except strings). For example, the `'  '` pair is really annoying in strings, since it's used as apostrophe in english and other languages. Likewise, `\` '` is annoying inside lisp code (backtick is used in macros), but is used in emacs lisp documentation.

By default, auto-pairing is allowed in both strings and code. The order of evaluation is as follows:

1. Allowed in this mode? (see ban/allow mechanics above).
2. Locally banned in strings - specific pair won't auto-pair in strings in specific major modes.
3. Globally banned - specific pair will never auto-pair in strings.
4. Locally allowed - specific pair will only auto-pair in strings in specific major modes.

The same hierarchy works for banning/allowing insertion in code. Note that if you disable insertion in comments and also in code, you might consider disabling the pair by the regular ban/allow mechanism, it will make for cleaner configuration :)

The functions used for these operations are:

    (sp-add-local-ban-insert-pair-in-string)
    (sp-add-local-allow-insert-pair-in-string)
    (sp-remove-local-ban-insert-pair-in-string)
    (sp-remove-local-allow-insert-pair-in-string)

    (sp-add-local-ban-insert-pair-in-code)
    (sp-add-local-allow-insert-pair-in-code)
    (sp-remove-local-ban-insert-pair-in-code)
    (sp-remove-local-allow-insert-pair-in-code)

The names are self-explanatory enough.

To change behaviours of the autopairing, see `M-x customize-group smartparens` for available options.

Wrapping
===========

*(This feature is only partially implemented. Currently, wrapping with tags is not supported. Permission system is not supported.)*

If you select a region and start typing any of the pairs, the active region will be wrapped with the pair. For multi-character pairs, a special insertion mode is entered, where point jumps to the beginning of the region. If you insert a complete pair, the region is wrapped and point returns to the original position.

If you insert a character that can't possibly complete a pair, the wrapping is cancelled, the point returns to the original position and the typed text is inserted.

If you use `delete-selection-mode`, you **MUST** disable it and enable an emulation by running `sp-turn-on-delete-selection-mode`. This behaves in the exact same way as original `delete-selection-mode`, indeed, it simply calls the `delete-selection-pre-hook` when appropriate. However, it intercepts it and handle the wrapping if needed.

At any time in the insertion mode you can use `C-g` to cancel the insertion. In this case, both the opening and closing pairs are removed and the point returns to the original position. The region is not deleted even if `sp-turn-on-delete-selection-mode` is active.

Automatic escaping
============

Warning: this feature needs `font-lock-mode` enabled (but seriously, who doesn't use it).

If `sp-autoescape-string-quote` is enabled, all quotes (by this string delimiters are understood) are escaped if the point is within a string. The quote pair, if auto inserted, is escaped as well.

If you want automatic deletion of escaped quote, you need to add it as a new pair definition with `sp-add-pair`. For example, both `"` and `\"` need to be defined as pairs (these two are included by default as they are by far the most common).

Some example situations:

* `"a|sd"`, user hits `"`, result: `"a\"|sd"`
* `"some | word"`, user hits `"`, result: `"some \"|\" word"`
* `"some | word"`, user types `\"`, result: `"some \"|\" word"` (the quote is not escaped again)

It's best if you try this feature during actual editing to see if you like it or not. Please post suggestions. Also, there are some corner cases where the behaviour might not be expected, but they are so rare that fixing them is not a priority right now. However, if you find any suspicious behaviour do not hesitate to report it.

Example configuration
=============

This is actually my current config for this package. Since I'm only using `emacs-lisp-mode` and `markdown-mode` now, it's somewhat brief for the moment :)

    (smartparens-global-mode t)

    ;; pending deletion. Replace active region with input. This is
    ;; virtually `delete-selection-mode' emulation.
    (sp-turn-on-delete-selection-mode)

    ;;; add new pairs
    (sp-add-pair "*" "*")

    ;;; global
    (sp-add-ban-insert-pair-in-string "'")

    ;;; emacs-lisp-mode
    (sp-add-local-ban-insert-pair "'" 'emacs-lisp-mode)
    (sp-add-local-ban-insert-pair-in-code "`" 'emacs-lisp-mode)

    ;; you can also use the (sp-with) macro. It will automatically add the
    ;; mode to the end of each call. How cool is that!
    (sp-with 'markdown-mode
             (sp-add-local-ban-insert-pair "`")
             (sp-add-local-ban-insert-pair "'")
             ;; this also disables '*' in all other modes
             (sp-add-local-allow-insert-pair "*"))
