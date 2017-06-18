.. _permissions:

Permissions
===========

All the permissions settings are handled through these two functions:

- ``sp-pair`` for setting global pair properties,
- ``sp-local-pair`` for setting local pair properties.

Each pair has opening and closing delimiter and a number of additional
*simple* or *list* properties.  Simple properties are simple scalars
like a string or a number. List properties are lists of functions,
predicates or constants and determine various behaviours of the
pair. You can specify all the properties in one call.  The distinction between simple and list properties is important when it comes to property inheritance.

There are additional properties you can set that are not handled by this
system. See ``M-x customize-group smartparens`` for available options.
This mostly includes enabling and disabling various heuristics to handle
special cases.

..
   **TODO** Smartparens uses quite flexible system of permissions to
   determine what actions should take place and in what contexts. Add some
   more info here.

   Maybe come up with a better, more uniform, system? **TODO-END**

List property inheritance
--------------------

.. note:: If this section is not clear, consider looking at the :ref:`Actions<actions>` or :ref:`Filters<filters>` sections first to familiarize yourself with the concept of list properties.

When specifying a list of properties for a local pair, you can use
special forms to inherit the global values and add or remove some of
them locally. To do this, you specify the list with the special
structure:

-  if the list is a normal list, the global definition is ignored and
   this list is used locally. This also means that specifying ``nil``
   will simply remove all the values from the list.
-  if the first item in the list is keyword ``:add`` all the items in
   the list will be **added** to the global value
-  if the first item in the list is keyword ``:rem`` all the items in
   the list will be **removed** from the global value
-  if the list is of the form ``((:add ...) (:rem ...))`` the items in
   the "``:add`` list" are added and then the items in the "``:rem``
   list" are removed from the global value.

Therefore, if you first add a global filter and then wish to remove it
locally in a specific major mode, you can do it like this:

.. code-block:: emacs-lisp

    ;; add a global filter
    (sp-pair "'" nil :unless '(sp-point-after-word-p))

    ;; then remove it in c-mode
    (sp-local-pair 'c-mode "'" nil :unless '(:rem sp-point-after-word-p))

    ;; in case there is only one filter the same result would be achieved with
    (sp-local-pair 'c-mode "'" nil :unless nil)


By default all the properties are inherited from the global definition
if one exist.

.. _actions:

Actions
---------

Each pair has with it associated a list of actions that it can perform.
The supported actions are:

-  **insert** - Autoinsert the closing pair when opening pair is typed.
-  **wrap** - Wrap an active region with this pair if its opening delimiter
   is typed while region is active.
-  **autoskip** - If the point is before the closing delimiter and you start
   typing it, smartparens will skip over it in order to not create
   unbalanced expression. However, this is also controlled by various
   other settings, see ``M-x customize-group RET smartparens RET``, and
   search for ``autoskip``. Setting ``autoskip`` action will only tell
   smartparens it is *possible* (not necessary) to do the skipping.
   Therefore, to completely disable autoskip for a pair, you should
   remove this action.
-  **navigate** - Enable this pair for navigation/highlight and strictness
   checks.
- **escape** - Allow autoescaping of this delimiter in string contexts.

All of these actions are enabled for each pair by default. If you want
to only use pair for a subset of actions, you can specify the explicit
actions set by using the keyword argument (just argument from now on)
``:actions``:

.. code-block:: emacs-lisp

    (sp-pair "'" "'" :actions '(wrap))          ;; only use '' pair for wrapping
    (sp-pair "%" "%" :actions '(insert))        ;; only use %% pair for auto insertion, never for wrapping
    (sp-pair "(" ")" :actions '(wrap insert))   ;; use () pair for both actions.

To disable some action only in a specific major mode you add the local
property for the pair. This is done using the function
``sp-local-pair``. To completely disable a pair in a specific mode, you
can use ``nil`` as the list of actions, then no actions will be
performed.

.. code-block:: emacs-lisp

    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)           ;; no '' pair in emacs-lisp-mode
    (sp-local-pair 'markdown-mode "`" nil :actions '(insert))       ;; only use ` for auto insertion in markdown-mode
    (sp-local-pair 'latex-mode "\\"" nil :actions '(:rem insert))   ;; do not use \" for insert action but use it for any other action

.. note:: You have to specify *some* value for the 3rd argument (closing delimiter) in ``sp-local-pair``. If you specify nil, the old value is preserved. If you specify a string, this will :ref:`locally override<local-pair-definitions>` the definiton of the closing pair.

.. warning:: You must specify the action lists together with all other properties in the same call with which you define the pair.  Calling ``sp-local-pair`` twice will ignore the previous call and reapply the current properties on top of the global definitions.

.. _filters:

Filters
------

Each pair has with it associated two lists of predicates that decide if
the action should be performed or not. These are the *when* list and
*unless* list.

If the *when* list is not empty, at least one predicate on this list has
to return non-nil in order for the action to be performed.

If the *unless* list is not empty, all of the predicates must return
nil in order for the action to be performed.

You can use both lists, in which case the formula is:

.. code-block:: emacs-lisp

    (and (test-predicates when-list)
         (not (test-predicates unless-list)))

This expression has to return true in order for the action to be
performed.

To specify the filter lists for the pair, you use the arguments
``:when`` and ``:unless``:

.. code-block:: emacs-lisp

    ;; The '' pair will autopair UNLESS the point is right after a word,
    ;; in which case you want to insert a single apostrophe.
    (sp-pair "'" nil :unless '(sp-point-after-word-p))

    ;; You can also define local filters. Only pair the `' pair inside
    ;; emacs-lisp-mode WHEN point is inside a string. In other modes, the
    ;; global definition is used.
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))

The built-in predicates are:

-  ``sp-in-string-p`` - non-nil if point is inside a string
- ``sp-in-docstring-p`` - non-nil if point is inside an elisp docstring
-  ``sp-in-code-p`` - non-nil if point is inside code
-  ``sp-in-comment-p`` - non-nil if point is inside comment
-  ``sp-in-math-p`` - non-nil if point is inside a LaTeX math block
-  ``sp-point-in-empty-line-p`` - non-nil if point is on an empty line (line
   filled only with whitespace)

Following predicates are only tested for the ``insert`` action:

-  ``sp-point-before-eol-p`` - non-nil if point is before whitespace
   followed by newline
-  ``sp-point-after-bol-p`` - non-nil if point is after beginning of line
   followed by optional whitespace
-  ``sp-point-at-bol-p`` - non-nil if point is exactly at the beginning of
   line
-  ``sp-point-before-symbol-p`` - non-nil if point is before symbol
-  ``sp-point-before-word-p`` - non-nil if point is before word, where word
   is either ``w`` or ``_`` syntax class
-  ``sp-point-after-word-p`` - non nil if point is after word
-  ``sp-point-before-same-p`` - non-nil if point is before an opening pair
   same as the currently inserted one

You can supply any number of your own predicates. These predicates
should accept three arguments:

-  id of the pair, which is the opening delimiter.
-  action - See :ref:`actions`.
-  context - currently ``'string``, ``'code`` or ``'comment``. Note that the
   string/code distinction only makes sense in programming modes and
   modes that define what a "string" is.

Here's a definition of the built-in predicate ``sp-point-after-word-p``
for your inspiration:

.. code-block:: emacs-lisp

    (defun sp-point-after-word-p (id action context)
      "Return t if point is after a word, nil otherwise.  This
    predicate is only tested on \"insert\" action."
      (when (eq action 'insert)
        (save-excursion
          (backward-char 1)
          (looking-back "\\sw\\|\\s_"))))

Delayed insertion
--------------

When using pairs not made of punctuation but of words, such as Ruby's
``def`` and ``end`` pair, we usually don't want to expand ``def`` in
``indefinable``. To solve this issue, we can setup a "delayed
insertion". This will tell smartparens to wait and not insert the
closing pair right away, but see what the next action might
be. Usually we want to pair when the next action is a ``SPC`` or
``RET`` otherwise we don't.

Setting up delayed insertion is very simple. Instead of the usual
predicate you add a *list* of *triggers* to the ``:when`` filter.
These triggers will be tested after the next command is run (next
command means next interactive function in Emacs).

While you can add this special form to a list of regular tests, if it
is present in the ``:when`` argument it will always take precedence
and the insertion will always be delayed.

Here's a shortened example from ``smartparens-ruby.el``:

.. code-block:: emacs-lisp

    (sp-local-pair 'ruby-mode "def" "end"
                   :when '(("SPC" "RET" "<evil-ret>"))
                   )

This will set up a delayed insertion and insert the closing pair only
if the *next* action was invoked by ``SPC``, ``RET`` or ``<evil-ret>``
key.  In addition to key codes, you can supply any number of names of
the commands (such as ``'newline``) or regular ``:when`` predicates
which will be tested after the next command finished.

The predicates on ``:unless`` list are still considered and if they fail
the delayed insertion is not set up.

Make sure to also read the built-in function documentation for
``sp-pair`` inside Emacs by invoking ``C-h f sp-pair RET``.

Pre and post action hooks
-----------------

Each pair has with it associated two lists of functions that are
executed before and after an action is performed with this pair. These
lists are:

-  pre-handlers - functions that run *before* the action is executed.
-  post-handlers - functions that run *after* the action is executed.

These lists are specified by using argument ``:pre-handlers`` and
``:post-handlers`` respectivelly.

These actions are supported:

- ``insert`` - run in ``sp-insert-pair`` before/after closing delimiter is inserted
- ``wrap`` - run after region is wrapped with a pair

Additionally, special actions for various navigation or sexp
manipulation commands are added to allow users to do some additional
formatting.  These actions trigger both for ``:pre-handlers`` and
``:post-handlers``

- ``slurp-forward`` - run in forward-slurp before/after the closing delimiter is moved
-  ``slurp-backward`` - same, but for backward slurp
-  ``barf-forward`` - run in forward-barf before the closing delimiter is moved
-  ``barf-backward`` - same, but for backward barf.
- ``split-sexp`` - for ``sp-split-sexp``

These actions only run in ``:post-handlers``:

- ``beginning-of-sexp`` - run after ``sp-beginning-of-sexp``
- ``end-of-sexp`` - run after ``sp-end-of-sexp``
- ``indent-adjust-sexp`` - run after ``sp-indent-adjust-sexp``
- ``dedent-adjust-sexp`` - run after ``sp-dedent-adjust-sexp``
- ``skip-closing-pair`` - run after ``sp-skip-closing-pair``
- ``rewrap-sexp`` - run after ``sp-rewrap-sexp``

You should always test for the type of action in your hooks and only
run the code when apropriate. One can either create one callback with a
"dispatch table" that then calls the relevant code, or supply many
smaller functions that each test the action separately.

Each handler should be either a lambda, a symbol referring to a named
function or an :ref:`insertion
specification<insertion-specification>`.  If a function, it should
take these threes arguments (same as for the :ref:`filter<filters>`
predicates):

- ``id`` - the id of the pair,
- ``action`` - the action that triggered the handler,
- ``context`` - context in which the action was triggered.

You can also provide a special form ``(function conditions...)`` as a
handler. The condition can be either a name of command or a string
describing an event (in the format of ``single-key-description``). If
the last command or the event matches any on the conditions, the hook
will be executed. This means these hooks are run not after the
insertion, but after the *next* command is executed. **This handler is
only executed after a command following an insertion action.**

With these handlers (or hooks, using standard Emacs terminology) you can
perform various actions after the autoinsertions or wrappings. For
example, a simple function might be used to automatically add newlines
and position the point inside a ``{}`` block in ``c-mode``:

.. code-block:: emacs-lisp

    (defun my-open-block-c-mode (id action context)
      (when (eq action 'insert)
        (newline)
        (newline)
        (indent-according-to-mode)
        (previous-line)
        (indent-according-to-mode)))

To add this function to the ``{}`` pair post-handlers:

.. code-block:: emacs-lisp

    ;; we use :add to keep any global handlers. If you want to replace
    ;; them, simply specify the "bare list" as an argument:
    ;; '(my-open-block-c-mode)
    (sp-local-pair 'c-mode "{" nil :post-handlers '(:add my-open-block-c-mode))

Another useful hook might be to add a space after a pair if it is
directly followed by a word or another pair. An example for the ``()``
in emacs lisp:

.. code-block:: emacs-lisp

    (sp-local-pair 'emacs-lisp-mode "(" nil :post-handlers '(:add my-add-space-after-sexp-insertion))

    (defun my-add-space-after-sexp-insertion (id action _context)
      (when (eq action 'insert)
        (save-excursion
          (forward-char (length (plist-get (sp-get-pair id) :close)))
          (when (or (eq (char-syntax (following-char)) ?w)
                    (looking-at (sp--get-opening-regexp)))
            (insert " ")))))

Finally, an example of a special delayed action form. This will run the
``my-create-newline-and-enter-sexp`` function after you've inserted
``{}`` pair and immediately after hit ``RET``.

.. code-block:: emacs-lisp

    (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

    (defun my-create-newline-and-enter-sexp (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))

Of course, you can perform tasks of any complexity in these hooks, but
it's a good idea to keep them as simple as possible so the "editing
flow" won't be disrupted (e.g.: connecting to wikipedia and fetching 100
pages in a post-handler is *not* a good idea :)).

.. _insertion-specification:

Insertion specification
---------------

Because it is very common to insert text inside a pair after this is
inserted, smartparens provides a simple specification "language" you can
use to make this process easier. Let us first define the language in a
semi-formal way, then we will list some examples.

-  character ``|`` means "save-excursion", that is, all actions after
   this character will be carried out and then the point returns here.
-  sequence ``||`` means the same as above, but after all the
   instructions are executed, ``indent-according-to-mode`` is called
   here as well.
-  a square bracket block ``[...]`` inserts a special directive/command.
   Right now, these are supported:

   -  ``i`` - call ``indent-according-to-mode`` at this point.
   -  ``d#`` - call ``delete-char`` with the specified number (``#``
      stands for an integer) as argument.

-  to insert special characters ``|`` or ``[`` put a backslash ``\``
   before it. You don't need to escape ``]``.
-  all other non-special chatacters are inserted literally.

You can use this in ``:pre-handlers`` and ``:post-handlers`` instead of
a symbol specifying a function---both in the immediate and delayed
hooks.

The specification string is actually translated into a small lisp
program which is then evaluated with point inside the newly inserted
pair. Here we provide couple examples, for a more comprehensive list you
can look into the test suite
`here <https://github.com/Fuco1/smartparens/blob/master/tests/smartparens-test.el>`__
(look for test ``sp-test-insertion-specification-parser``).

.. code-block:: emacs-lisp

    "ab" => (progn (insert "ab"))
    "a|b" =>
    (progn
      (insert "a")
      (save-excursion
        (insert "b")))
    "||\n[i]" =>   ;; this is useful as a delayed RET action for {} pair in C-like languages
    (progn         ;; cf. my-create-newline-and-enter-sexp above
      (save-excursion
        (insert "\n")
        (indent-according-to-mode))
      (indent-according-to-mode))
    "* ||\n[i]" =>  ;; pretty-formats /**/ style comments after RET
    (progn
      (insert "* ")
      (save-excursion
        (insert "\n")
        (indent-according-to-mode))
      (indent-according-to-mode))
    ;; like so
    ;; /*|*/ =>
    ;; /*
    ;;  * |
    ;;  */

Here are some examples replicating some of the above handlers that use
function callbacks.

.. code-block:: emacs-lisp

    (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
    ;; =>
    (sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

    ;; insert space, remember position, insert space
    (sp-local-pair 'emacs-lisp-mode "(" nil :post-handlers '(:add " | "))
    ;; so that typing `(' results into `( | )', where | is the point.

Hooks for wrapping actions
--------------

Due to the nature of the wrapping action the pre-handlers are **NOT**
executed for this action. If you wish to query for information about
the last wrap in the post handler you can use the
``sp-last-wrapped-region`` variable to do so.

If you change the positions of the opening and closing delimiters (for
example by opening new lines or inserting text) you should also update
the ``sp-last-wrapped-region`` variable to reflect these changes,
otherwise some functions might not work correctly (repeated wrapping
and last wrap deletion for example). See the documentation for this
variable for details.
