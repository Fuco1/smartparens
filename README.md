Important announcements
==========

* Very important option `sp-navigate-consider-symbols` has been added. Setting this to `t` will make spartparens consider symbols and strings as balanced expressions. That is, `something-like-this` will be considered as balanced expression enclosed by pair of whitespace characters. This emulates the original behaviour of `forward-sexp` and it is recommended you set this option to `t` for best user experience.

* From commit 118 forward, strings (as defined by mode syntax-table) are treated as other paired expressions. This means you can use `sp-up-sexp`, slurping and barfing on strings the same way you would use it with any other expression recognized by `sp-get-sexp`.

Table of content
==========

1. [smartparens](#smartparens)
2. [Installation](#installation)
3. [Compatibility issues](#compatibility-issues)
4. [Pair management](#pair-management)
    1. [Mode-dependent custom pairs](#mode-dependent-custom-pairs)
5. [Auto pairing](#auto-pairing)
    1. [Auto pairing in strings/code](#auto-pairing-in-stringscode)
    2. [User-defined predicates for auto pairing](#user-defined-predicates-for-auto-pairing)
6. [Wrapping](#wrapping)
    1. [Wrapping with tags](#wrapping-with-tags)
7. [Automatic escaping](#automatic-escaping)
8. [Navigation and S-exp manipulation](#navigation-and-s-exp-manipulation)
9. [Show smartparens mode](#show-smartparens-mode)
10. [Example configuration](#example-configuration)

smartparens
==========

Modern minor mode for Emacs that deals with parens pairs and tries to be smart about it. This package combines functionality of packages like [autopair](https://github.com/capitaomorte/autopair), [textmate](http://code.google.com/p/emacs-textmate/), [wrap-region](https://github.com/rejeep/wrap-region), partially [paredit](http://emacswiki.org/emacs/ParEdit) and others. It adds support for many more features, some including:

* support for pairs of any length (currently up to 10 characters), for example `"\\\\(" "\\\\)"` for automatic insertion of quoted parens in elisp regexp. These are fully user definable and customizable. Pairs can be same or different for opening and closing part.
* inteligent handling of closing pair. If user types `(`, `(|)` is inserted. If he then types `word)` the result is `(word)|` not `(word)|)`. This behaviour is cancelled if user moves backwards during editing or move point outside of the pair.
* automatic deletion of whole pairs. With pair `("\{" "\}")` (LaTeX literal brackets), `\{|\}` and backspace will remove both of the pairs. `\{\}|` and backspace will remove the whole closing pair. `\{|` and backspace will remove the whole opening pair.
* when followed by the same opening pair or word, do not insert the whole pair. That is: `|()` followed by `(` will produce `(|()` instead of `(|)()`. Similarly, `|word` followed by `(` will produce `(|word`.
* wraps region in defined pairs or defined tag pairs for "tag-modes" (xml/html...).
    * Different tags are supported, for example, languages that would use `{tag}` instead of `<tag>` or different opening pair and closing pair syntax, for example opening with `(tag` and closing with `)` (a.k.a. s-expression) or LaTeX `\begin{} \end{}` pair. Everything is user definable as usual :)
* automatically escape strings if wrapped with another string. `this "string"` turns to `"this \"string\""` automaticaly.
* automatically escape typed quotes inside a string
* [Jumping around](#navigation-and-s-exp-manipulation) the pairs (extending forward-sexp to custom user pairs)
* Functions to manipulate s-expressions, delete, wrap and unwrap, extend and contract...

This is just a list of basic "selling points", but many features have further customisations or finer details of control (not to mention there is far more features than these mentioned). For a complete list, read the readme. The autopair/textmate/wrap-region packages are virtually superseded, and most of the core functionality of paredit is replicated as well.

**All features** are fully customizable via `M-x customize-group smartparens`. You can turn every behaviour on or off for best user experience (yay buzzwords).

**NEW:** I've made a [youtube presentation](http://www.youtube.com/watch?v=ykjRUr7FgoI&list=PLP6Xwp2WTft7rAMgVPOTI2OE_PQlKGPy7&feature=plpp_play_all). It's in 2 parts because youtube didn't allow me to upload it in one video. Switch to 480p! Note that this presentation only talks about features up to commit ~40, so many of the newer features are not covered (those will hopefully be covered in future video).

Installation
==========

Install smartparens by placing `smartparens.el` in `/path/to/elisp`, a directory of your choice, and adding to your .emacs file:

    (add-to-list 'load-path "/path/to/elisp")

Then, the basic setup is as follows:

    (require 'smartparens)
    (smartparens-global-mode 1)

You can also install this as a package with `M-x package-install smartparens`. This package is available in `melpa` repository.

If you've installed this as a package, you don't need to require it, as there is an autoload on `smartparens-global-mode`.

You can disable smartparens in specific global modes by customizing `sp-ignore-mode-list`. Of course, you can also only turn it on in specific modes via the hook mechanisms.

This package *depends* on [dash](https://github.com/magnars/dash.el). If you've installed smartparens via `package-install`, it should resolve dependencies automatically (dash is on melpa and marmalade). If not, you'd need to install it manually. See the installation information on their homepage.

You **musn't** bind anything to the trigger keys -- those that are part of any pair or tag -- and they have to be kept pointing to `self-insert-command`. Smartparens automatically bind these in its own keymap, so do not re-bind them.

See the last section for an [example configuration](#example-configuration). Please have a look at it as it contains a working example and explains many features "visually".

Compatibility issues
==========

#### `sp-delete-selection-mode`, after 30. nov. 2012, commit 70 ####

The functionality provided by `sp-delete-selection-mode` is now automatic. That means, if you use `cua-mode` with `cua-delete-selection` or `delete-selection-mode`, smartparens detect this automatically and adjust the wrapping operations to handle these modes. You do not need to run any special smartparens command (like `sp-turn-on-delete-selection-mode`) nor to disable the original selection modes.

If the original selection modes are disabled, smartparens respect that setting instead.

Both `sp-turn-on/off-delete-selection-mode` functions are now **removed**. Variable `sp-delete-selection-mode` is removed as well.

Therefore, please remove all the calls to these functions from your configuration and replace them with either `(setq delete-selection-mode t)` or `(setq cua-delete-selection t)` (cua sets this to `t` by default).

#### auto-complete ####

There are some problems with auto-complete mode when cancelling the pop-up selection using some closing pair trigger. More investigation is needed now. If you are able to figure out what is going on, please reply. See [Issue #2](https://github.com/Fuco1/smartparens/issues/2)

#### org-cua-dwim ####

There is a bug (or a feature?) in `org-cua-dwim` that handles active selections in a strange way, incompatible with smartparens. This advice should fix the most obvious problems:

    (defadvice cua-delete-region (around smartparens-org-dwim-fix-active-region activate)
      "Fix the smartparens/org-cua-dwim interaction."
      (if (interactive-p)
          (if (region-active-p)
              ad-do-it
            (if (eq major-mode 'org-mode)
                (org-delete-backward-char (or (ad-get-arg 0) 1))
              (delete-backward-char (or (ad-get-arg 0) 1))))
        ad-do-it))

For more information please read the [Issue #16](https://github.com/Fuco1/smartparens/issues/16) on the tracker.

Pair management
==========

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

    ("\\\\(" . "\\\\)") ;; emacs regexp parens
    ("\\{"   . "\\}")
    ("\\("   . "\\)")
    ("\\\""  . "\\\"")
    ("/*"    . "*/")
    ("\""    . "\"")
    ("'"     . "'")
    ("("     . ")")
    ("["     . "]")
    ("{"     . "}")
    ("`"     . "'") ;; tap twice for tex double quotes

You can remove pairs by calling `sp-remove-pair`. This will also automatically delete any assigned permissions!

    (sp-remove-pair "\{")
    (sp-remove-pair "'")

Mode-dependent custom pairs
----------

Sometimes, a globally defined pair is not appropriate for certain major modes. You can redefine globally defined pairs to have different definition in specific major modes. For example, globally defined pair \`' is used in `emacs-lisp-mode` for links in comments and in `LaTeX-mode` for quotes. However, in `markdown-mode`, a pair \`\` is used instead to insert inline code. Therefore, it is desired to redefine this global pair to this new value.

That is accompilshed by using this function:

    (sp-add-local-pair "`" "`" 'markdown-mode) ;; adds `` as a local pair in markdown-mode

Note that this should *only* be used to overload the global definitions. If you want to add a new pair that should only activate in a specific mode, define it as global and set the permissions to only auto pair it in the specific mode. See the next section for more information on permissions.

Local pairs can be removed by caling `sp-remove-local-pair` to restore the original global definition:

    (sp-remove-local-pair "`" 'markdown-mode)

**Important**: this function only removes the pairs you have previously added using `sp-add-local-pair`, if you ever want to revert the change. It does not remove a global pair in the specified mode. If you want to disable some pair in specific modes, see the next section and set the permissions accordingly.

Auto pairing
==========

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

For additional changes to behaviour of autopairing, see `M-x customize-group smartparens` for available options.

Auto pairing in strings/code
----------

In addition to these restrictions, you can also disable all or specific pairs only inside comments and strings (strings from now on) or only in code (everything except strings). For example, the `'  '` pair is really annoying in strings, since it's used as apostrophe in english and other languages. Likewise, \`' is annoying inside lisp code (backtick is used in macros), but is used in emacs lisp documentation.

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

User-defined predicates for auto pairing
----------

In addition to all the permissions and options that are bundled with spartparens, users can add their own predicates to further fine-tune the behaviour of auto insertion. Smartparens provide a "hook" named `sp-autoinsert-inhibit-functions` which is tested after all the built-in tests. All the functions you add to this hook should be of two arguments which smartparens will supply: first is the current opening pair, second is boolean value specifying if point is inside string or comment or not.

The return value should be `t` if you wish to inhibit the insertion and `nil` otherwise. If *any* of the supplied functions return `t`, insertion is cancelled.

The following example function disable auto insertion of ' pair if it is preceeded by character from word constituent or symbol constituent symbol class, but not if the character is 'r' or 'u' and the point is in code. In that case we want to kick in a "special" behaviour of `python` raw or unicode strings and quote the following text.

    (defun my-test-if-preceeding-is-word (open-pair in-string)
      (when (string= open-pair "'")
        (save-excursion
          (backward-char 1)
          (cond
           ((and (not in-string)
                 (memq (preceding-char) '(?r ?u)))
            nil)
           (t (looking-back "\\sw\\|\\s_"))))))

You can supply any number of similar functions.

Wrapping
==========

*(This feature is only partially implemented. Permission system is not supported yet. This means auto wrapping works everywhere if it is turned on. However, this isn't such a big deal as auto-insertion of pairs.)*

If you select a region and start typing any of the pairs, the active region will be wrapped with the pair. For multi-character pairs, a special insertion mode is entered, where point jumps to the beginning of the region. If you insert a complete pair, the region is wrapped and point returns to the original position.

If you insert a character that can't possibly complete a pair, the wrapping is cancelled, the point returns to the original position and the typed text is inserted. If you use `delete-selection-mode` or `cua-delete-selection`, the content of the region is removed first.

At any time in the insertion mode you can use `C-g` to cancel the insertion. In this case, both the opening and closing pairs are removed and the point returns to the original position. The region is not deleted even if some "delete-selection" mode is active.

**New (r99):** You can use functions `sp-select-next-thing` and `sp-select-previous-thing` to activate a region around next or previous expression (or expressions with prefix argument). This is very handy in combination with wrapping to wrap pairs around existing expressions or symbols. For example, to turn

    this-is-some-function (complex-function arg)

into

    (this-is-some-function (complex-function arg))

all you need to do is `C-2 C-] (` (when `sp-select-next-thing` is bound to `C-]`). For more info, see the section about [navigation](#navigation-and-s-exp-manipulation).

*Note: By default, `sp-select-next-thing` only operate on balanced expressions enclosed with pairs. If you want to also consider symbols, set `sp-navigate-consider-symbols` to `t`.*

#### Repeated wrapping

**New (r93):** After wraping a region and immediately after inserting another *basic* pair (that is, defined by `sp-add-pair`), it is often desired to apply this pair as another wrap around the just wrapped region. Imagine, in LaTeX mode, wrapping "word" with quotes to produce `&#96;word'`. Now, hitting another backtick should produce double-quoted word `&#96;&#96;word''`. The same can apply to `markdown-mode` and \* character to mark italics/bold text.

You can now set variable `sp-wrap-repeat-last` to "No repeat", "Repeat only if same", "Re-wrap region with any pair". Read the built-in description for more info.

Note that this behaviour is only active if you *type in* the pair immediately after the wrapping without invoking any additional command, such as backspace or navigation command. However, since this is mostly intended for single character pairs, this does not impose any real limitation.

Wrapping with tags
----------

Wrapping with more structured tags is also supported. For example, in `html-mode` you might want to automatically wrap a region `some code` and change it into `<span class="code">some code</span>`. For this purpose, you can define tag pairs. These allow you to enter special tag wrapping insertion mode, where you can enter arbitrary text. It also automatically mirror the opening tag text into the closing tag. Furthermore, the closing tag can be automatically transformed with any function to a different string. For example, the opening tag's content `span class="code"` can be transformed to just `span`.

The tag wrapping pairs have higher priority than regular tags, that is, if it is possible to start tag-wrapping, the regular wrap mode is exited and the tag insertion mode is entered *even if* there is possible continuation of the currently inserted opening wrap pair. For example, if tag insertion trigger is `<` and there is a regular pair `<< >>`, this is ignored and the tag insertion mode is entered immediately after `<` is inserted.

Tags are defined by following function:

    ;; this pair is already present by default. `sp-match-sgml-tags' cuts
    ;; off everything after the first space: "span class='x'" -> "span".
    (sp-add-tag-pair "<" "<_>" "</_>" 'sp-match-sgml-tags '(sgml-mode html-mode))

    ;; this pair is already present by default.
    (sp-add-tag-pair "\\b" "\\begin{_}" "\\end{_}" 'identity '(tex-mode latex-mode))

where the arguments are:

1. Tag trigger
2. Opening tag format
3. Closing tag format
4. Transformation function for closing tag. You can use built-in function `identity` to return the tag unchanged
5. Modes where this is allowed. Tag pairs can't be defined globally. The rationale is that they are highly specialized and the idea of specific tags for specific modes make more sense.

The character `_` in the format strings is replaced with the inserted text and mirrored to the closing pair. Before inserting text in the closing pair, content of the opening pair is transformed with transformation function. Only one `_` per pair is allowed. The closing tag does not have to contain `_` (then no text is inserted there). If the opening pair doesn't have `_` either, the tag is simply inserted as text and tag insertion mode is not entered. This can be used to form "shortcuts" for commonly used wrappings, such as `(sp-add-tag-pair "2" "**" "**" nil 'markdown-mode)` to mark selected text as bold.

If the transformation function is `nil`, it automatically defaults to `identity`. Otherwise, it should be a one-argument function that accept the content of the opening pair and as output gives the content of the closing pair.

You can add different tags for the same trigger in different modes. The mode sets must not overlap, otherwise random one is picked (the behaviour is undefined).

Tags can be removed with `sp-remove-tag-pair` function, which takes as arguments the trigger and the mode where you want to remove it.

    (sp-remove-tag-pair "<" 'sgml-mode) ;; modes can also be list of modes

When in tag insertion mode, special key-bindings are active. These are:

* `C-a`, `C-e` jumps to the beginning/end of the tag section.
* `C-g` terminate the tag insertion mode.

Tag insertion mode is also terminated if you leave the area of the opening tag pair overlay, for example with search function or `previous-line` command.

Automatic escaping
==========

If `sp-autoescape-string-quote` is enabled, all quotes (by this string delimiters are understood) are escaped if the point is within a string. The quote pair, if auto inserted, is escaped as well.

If you want automatic deletion of escaped quote, you need to add it as a new pair definition with `sp-add-pair`. For example, both `"` and `\"` need to be defined as pairs (these two are included by default as they are by far the most common).

Some example situations:

* `"a|sd"`, user hits `"`, result: `"a\"|sd"`
* `"some | word"`, user hits `"`, result: `"some \"|\" word"`
* `"some | word"`, user types `\"`, result: `"some \"|\" word"` (the quote is not escaped again)

#### Escaping nested quotes inside empty strings

You can specify a list of modes where the quotes aren't escaped if the string is empty. This is useful in modes such as `python-mode`, where `"""|"""` is used to input multi-line strings. Therefore, pressing `"` three times would result in `"""|"""` instead of `"\"\"|\"\""`. Use `M-x customize-variable sp-autoescape-string-quote-if-empty` to add the modes.

#### Escaping quotes when wrapping with a quote pair

If you select a region where both ends are inside a string and wrap it with quotes, the quotes are automatically escaped (the `><` characters mark beginning and end of the region respectively):

    "here is some >|long< string"   -> "here is some \"|long\" string"
    "youcan>quote<|insidewordstoo"  -> "youcan\"quote\"|insidewordstoo"

If you select a region where both ends are *not* inside a string and wrap it with quotes, all unescaped quotes inside the region are automatically escaped.

    >This is "some" text<|              -> "This is \"some\" text"|
    >"also" works "with" more words<|   -> "\"also\" works \"with\" more words"|

#### Escaping quotes before closing pair

**New (r84):** Sometimes is it desirable not to automatically escape the string quote. One such situation is when you are looking at the closing pair and at the same time are inside a string. If you insert a closing pair, 99% of the time you want to close the string and not continue past the closing pair. Consider this situation:

    {"this", "is", "array", "of", "words|}

If you hit `"` here, the expected result is

    {"this", "is", "array", "of", "words"|}

instead of

    {"this", "is", "array", "of", "words\"|\"}

This behaviour is disabled by default to keep backward compatibility. If you want to enable it, use `M-x customize-variable sp-autoinsert-quote-if-followed-by-closing-pair` and set it to `nil`.


It's best if you try these features during actual editing to see if you like it or not. Please post suggestions and heuristics for making this even more pleasant (for example heuristics that guess if I want to close the un-closed string, such as `"Do i want to insert quote here or escape|`). If you find any suspicious behaviour do not hesitate to report it.

Navigation and S-exp manipulation
==========

Smartparens provides multitude of functions to navigate and manipulate the balanced expressions (s-expressions, sexps).

In the function lists, after each function in comment is the "recommended" binding (i.e. the binding author is using), however, no function is bound by default. You should put these into `sp-keymap` so they would only be enabled in `smartparens-mode`. You can use:

    (define-key sp-keymap (kbd "your-key") 'function)

to do the local binding. Note that this has to occur *after* `smartparens-mode` is loaded, otherwise the `sp-keymap` variable will be void. See the [example configuration](#example-configuration) at the end of this readme for the working code to set the bindings.

Each function where it makes sense comes with forward and backward version, where the backward version does the same thing but in reverse direction.

All of the provided functions can accept a prefix argument in which case they do the thing that many times or operate on arg-th expression. The "not backward" versions also accept negative argument, in which case they behave just as the "backward" versions (in fact, backward versions just call normal ones with negative arguments). These functions work as would be expected by most users, however, if you are unsure check the built-in description with `C-h f name-of-function`.

When it makes sense, the function return the expression on which it operated most recently as a return value with format of `sp-get-sexp`, that is 4-tuple `(beg-of-expr end-of-expr opening-pair closing-pair)`. For example `(sp-next-sexp 2)` would return the information about 2nd next expression. This, in combination with `(save-excursion)` macro can be used to quickly query for information about sexps in your own functions.

#### Navigation functions

List of navigation functions:

    sp-forward-sexp (&optional arg)         ;; C-M-f
    sp-backward-sexp (&optional arg)        ;; C-M-b
    sp-down-sexp (&optional arg)            ;; C-M-d
    sp-backward-down-sexp (&optional arg)   ;; C-M-a
    sp-up-sexp (&optional arg)              ;; C-M-e
    sp-backward-up-sexp (&optional arg)     ;; C-M-u
    sp-next-sexp (&optional arg)            ;; C-M-n
    sp-previous-sexp (&optional arg)        ;; C-M-p

These functions work pretty much exactly the same as the emacs-built in versions without `sp-` prefix, but operate on all user defined strictly balanced expressions. Strictly balanced means that `|( [ ) ]` will jump to `( [ |) ]`, not `( [ ) |]` as the default forward-sexp would.

These functions never signal the "Unbalanced parentheses" scan error and by default jump to the beginning or end of next/previous sexp, which is reasonable behaviour. If there is some special behaviour, it is documented.

**New (r96):** If you want to also operate on symbols that are not wrapped, such as `(defun >name-of-fun< (arg) nil)` (leq/geq mark the symbol boundary), set `sp-navigate-consider-symbols` to `t`. Emacs built-in functions `forward-sexp` and `backward-sexp` recognize these as "expressions". If you set this option to `t`, all functions where it makes sense (that is, not unwrapping functions etc.) will consider symbols a balanced expressions. *Strings* enclosed with "" are also considerd as being one symbol.

Lastly, the navigation with expressions where opening and closing pair is the same is troublesome, as it is impossible to detect the beginning and end without maintaining a count in the whole buffer (e.g. what font-lock-mode does with strings). **Therefore, at the moment, these are not recognized as balanced expressions**. If you have an idea for a good heuristic or a method to fix this, please file an issue with the suggestion.

Here's a quick summary for each navigation function:

* `sp-forward-sexp` - Jump *after* the next balanced expression. If inside one and there is no forward exp., jump after its closing pair.
* `sp-backward-sexp` - Jump *before* the previous balanced expression. If inside one and there is no previous exp., jump before its opening pair.
* `sp-down-sexp` - Jump *after* the opening pair of next balanced expression. This effectively descends one level down in the "expression hierarchy". If inside one, jump *after* its opening pair. This can be used to quickly navigate to the beginning of current balanced expression.
* `sp-backward-down-sexp` - Jump *before* the closing pair of previous balanced expression. If inside one, jump *before* its closing pair. This can be used to quickly navigate to the end of current balanced expression.
* `sp-up-sexp` - Jump up one level from the current balanced expression. This means skipping all the enclosed expressions within *this* and then jumping *after* the closing pair. For example `(if (= a b) | (some call) (some other call))` -> `(if ...)|`.
* `sp-backward-up-sexp` - Jump up backwards one level from the current balanced expressions. This means skipping all the enclosed expressions within *this* backwards and then jumping *before* the opening pair.
* `sp-next-sexp` - Jump to the *beginning* of following balanced expression. If there is no following expression on the current level, jump one level up, effectively doing `sp-backward-up-sexp`.
* `sp-previous-sexp` - Jump to the *end* of the previous balanced expression. If there is no previous expression on the current level, jupm one level up, effectively doing `sp-up-sexp`.

#### Manipulation functions

List of manipulation functions:

    sp-kill-sexp (&optional arg)                        ;; C-M-k
    sp-backward-kill-sexp (&optional arg)               ;; C-- C-M-k

    sp-unwrap-sexp (&optional arg)                      ;; M-<delete>
    sp-backward-unwrap-sexp (&optional arg)             ;; M-<backspace>

    sp-splice-sexp (&optional arg)                      ;; M-D
    sp-splice-sexp-killing-forward ()                   ;; C-M-<delete>
    sp-splice-sexp-killing-backward ()                  ;; C-M-<backspace>
    sp-splice-sexp-killing-around (&optional arg)       ;; C-S-<backspace>

    sp-split-sexp ()                                    ;; none

    sp-forward-slurp-sexp (&optional arg)               ;; C-<right>
    sp-forward-barf-sexp (&optional arg)                ;; C-<left>
    sp-backward-slurp-sexp (&optional arg)              ;; C-M-<left>
    sp-backward-barf-sexp (&optional arg)               ;; C-M-<right>

    sp-select-next-thing (&optional arg)                ;; C-M-]
    sp-select-previous-thing (&optional arg)            ;; C-[

    sp-select-next-thing-exchange (&optional arg)       ;; C-]
    sp-select-previous-thing-exchange (&optional arg)   ;; C-- C-]

Some functions, especially slurp/barf functions are inspired by [paredit](http://emacswiki.org/emacs/ParEdit) package and work roughly the same. However, they can accept optional argument to slurp/barf that many times.

Here's a quick summary for each manipulation function:

* `sp-kill-sexp` - Kill the *next* balanced expression. If point is inside one and there's no following expression, kill the enclosing expression instead.
* `sp-backward-kill-sexp` - Kill the *previous* balanced expression.
* `sp-unwrap-sexp` - Remove the wrapping pair from the *following* expression. Following expression is one returned by `sp-forward-sexp`.
* `sp-backward-unwrap-sexp` - Remove the wrapping pair from the *previous* expression. Previous expression is one returned by `sp-backward-sexp`.
* `sp-splice-sexp` - Remove the wrapping pair from *this* expression. With arg, do this on Nth enclosing expression as if first navigated with `sp-up-sexp`.
* `sp-splice-sexp-killing-forward` -  Remove the wrapping pair from *this* expression and kill everything between `(point)` and end of this expression.
* `sp-splice-sexp-killing-backward` -  Remove the wrapping pair from *this* expression and kill everything between the beginning of this expression and `(point)`.
* `sp-splice-sexp-killing-around` - Remove the wrapping pair from *this* expression and kill everything inside save for ARG next expressions.
* `sp-split-sexp` - Split the current list using the enclosing delimiters.
* `sp-forward-slurp-sexp` - Extend the current list by one balanced expression or symbol by moving the *closing* delimiter.
* `sp-forward-barf-sexp` - Contract the current list by one balanced expression or symbol by moving the *closing* delimiter.
* `sp-backward-slurp-sexp` - Extend the current list by one balanced expression or symbol by moving the *opening* delimiter.
* `sp-backward-barf-sexp` - Contract the current list by one balanced expression or symbol by moving the *opening* delimiter.
* `sp-select-next-thing` - Select next balanced expression as returned by `sp-forward-sexp`. Examples of usage: Can be cleverly used with wrapping features, for example if you want to wrap next expression in additional pair of parens. It can also be used to select expressions followed by `M-w` (copy but not kill to ring).
* `sp-select-previous-thing` - Select previous balanced expression as returned by `sp-backward-sexp`.
* `sp-select-next-thing-exchange` - Same as `sp-select-next-thing` but execute `exchange-point-and-mark` afterwards.
* `sp-select-previous-thing-exchange` - Same as `sp-select-previous-thing` but execute `exchange-point-and-mark` afterwards.

Show smartparens mode
==========

`show-smartparens-mode` provides functionality similar to `show-paren-mode`, but works for all the user-defined pairs (that have different opening and closing pair, that is, all those which are detected by `sp-get-sexp`).

It is available as a globalized minor mode and is allowed in all modes where `smartparens-mode` is allowed, that is any mode which is not on the `sp-ignore-mode-list`.

You can turn it on with `(show-smartparens-global-mode t)`. If you want to only turn it on in specific modes, use after-load hooks for these modes and call `(show-smartparens-mode)`.

Note that the pair-search is somewhat slower than `show-paren-mode`, which uses C libraries to do the parsing. If you have a very long buffer and a mis-matched pair, this can sometimes result in a noticable lag (1 second or more) when you place cursor in front/behind this pair.

Example configuration
==========

This is my current config for this package. Some things (`C-<left_bracket>` overwrite of `ESC` sequence) depend on other files, but most of it is readily usable.

    (smartparens-global-mode t)

    ;; highlights matching pairs
    (show-smartparens-global-mode t)

    ;;; key binds
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    ;;; add new pairs
    (sp-add-pair "*" "*")
    (sp-add-pair "$" "$")
    (sp-add-pair "<" ">") ;; in html only!

    ;;; global
    (sp-add-ban-insert-pair-in-string "'")

    ;; you can also use the `sp-with-tag' macro. It will automatically add
    ;; the tag to each function. Use this only with functions where the
    ;; first argument is the opening pair! Here, we want to disable ' pair
    ;; in a bunch of text modes
    (sp-with-tag "'"
                 (sp-add-local-ban-insert-pair 'markdown-mode)
                 (sp-add-local-ban-insert-pair 'tex-mode)
                 (sp-add-local-ban-insert-pair 'latex-mode)
                 (sp-add-local-ban-insert-pair 'text-mode)
                 (sp-add-local-ban-insert-pair 'log-edit-mode)
                 (sp-add-local-ban-insert-pair 'org-mode))

    ;; now, we could've also done just this:
    ;; (sp-add-local-ban-insert-pair "'"
    ;;                               '(markdown-mode
    ;;                                 ...))
    ;; but I wanted to show you how to use the sp-with-tag macro :)

    ;;; emacs-lisp-mode(s)
    (sp-with '(emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode)
             (sp-add-local-ban-insert-pair "'")
             (sp-add-local-ban-insert-pair-in-code "`"))

    ;;; markdown-mode
    ;; you can also use the `sp-with' macro. It will automatically add the
    ;; mode to the end of each call. How cool is that!
    (sp-with '(markdown-mode rst-mode)
             (sp-add-local-pair "`" "`")
             ;; this also disables '*' in all other modes
             (sp-add-local-allow-insert-pair "*")
             (sp-add-tag-pair "2" "**" "**" nil))

    ;;; tex-mode latex-mode
    (sp-with '(tex-mode latex-mode) ;; yes, this works with lists too!
             (sp-add-local-allow-insert-pair "$")
             (sp-add-tag-pair "i" "\"<" "\">" nil))

    ;;; python-mode
    (sp-with 'python-mode
             (sp-add-local-ban-insert-pair "`"))

    ;;; html-mode
    (sp-with '(html-mode sgml-mode)
             (sp-add-local-allow-insert-pair "<"))
