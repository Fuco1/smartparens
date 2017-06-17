.. _pair-management:

Pair management
===============

Adding pairs
------------

.. el:function:: sp-pair open close

To define a new pair use the ``sp-pair`` function. Here is an example of the most basic use:

.. code-block:: emacs-lisp

   (sp-pair "\{" "\}") ;; latex literal brackets (included by default)
   (sp-pair "<#" "#>")
   (sp-pair "$" "$")   ;; latex inline math mode. Pairs can have same opening and closing string

Pairs defined this way are by default used on all :ref:`actions`. However, you can disable certain pairs for auto insertion and only have them for wrapping, only use them for navigation or use any other combination of actions. This is achieved by setting the pair's actions to allow or disable certain operations in certain contexts.

The ``sp-pair`` function accepts a family of *keyword* arguments which can further specify the behaviour.  The keyword arguments can be arbitrarily combined in any order, the only requirement is that the first two positional arguments are always ``open`` and ``close`` for the pair.

.. function:: (sp-pair :wrap binding)

You can add a binding for a "wrapping" action. Smartparens automatically binds a command that wraps the next expression with this pair to the supplied binding. The bound command accepts the same prefix arguments as ``sp-select-next-thing``. In addition, if a region is already active, it wraps this region.

.. note:: This is useful in combination with evil visual selection mode, since with regular emacs, smartparens wraps the active regions automatically when you press the delimiter.

.. warning:: No syntax check is performed on the active region.  This might change in the future.

To add the binding use the ``:wrap`` keyword:

.. code-block:: emacs-lisp

   (sp-pair "(" ")" :wrap "C-(")
   ;; |foobar
   ;; hit C-(
   ;; becomes (|foobar)

.. function:: (sp-pair :insert binding :trigger trigger)

You can also add a binding for "insert" action. This is done the exact same way as for wrapping, but the keyword is ``:insert``.  Pressing this simply inserts the pair in the buffer. This is useful if you want to insert the pair with a modifier hotkey or a chord. To simply provide a shorter (expandable) trigger, you can specify a ``:trigger`` keyword.

.. code-block:: emacs-lisp

   (sp-local-pair 'LaTeX-mode "\\left(" "\\right)" :insert "C-b l" :trigger "\\l(")


This will make smartparens insert ``\left(|\right)`` when you type ``\l(`` or hit ``C-b l`` (where ``|`` is the point). Typing out the entire opening delimiter ``\left(`` will also work.

.. note:: Many such commands for LaTeX are provided in configuration file ``smartparens-latex.el``. Check it out!

.. note:: You don't have to use both ``:trigger`` and ``insert``; one or the other (or both) are fine.

It is generally better to add these bindings only to certain major modes where you wish you use this functionality instead of binding them globally to avoid hotkey clashes. See the section about :ref:`local pair definitions<local-pair-definitions>`.

Removing pairs
--------------

You can remove pairs by calling ``sp-pair`` using the optional key argument ``:actions`` with value ``:rem``. This will also automatically delete any assigned :ref:`permissions`! This command is mostly only useful for debugging or removing built-in pairs.

.. code-block:: emacs-lisp

   ;; the second argument is the closing delimiter, so you need to skip it with nil
   (sp-pair "\{" nil :actions :rem)
   (sp-pair "'" nil :actions :rem)

Default pairs
-------------

Since some pairs are so common that virtually every user would use them, smartparens comes with a list of global default pairs. At the moment, this list includes:

.. code-block:: emacs-lisp

   ("\\\\(" . "\\\\)") ;; emacs regexp parens
   ("\\{"   . "\\}")   ;; latex literal braces in math mode
   ("\\("   . "\\)")   ;; capture parens in regexp in various languages
   ("\\\""  . "\\\"")  ;; escaped quotes in strings
   ("\""    . "\"")    ;; string double quotes
   ("'"     . "'")     ;; string single quotes/character quotes
   ("("     . ")")     ;; parens (yay lisp)
   ("["     . "]")     ;; brackets
   ("{"     . "}")     ;; braces (a.k.a. curly brackets)
   ("`"     . "`")     ;; latex strings. tap twice for latex double quotes

.. _local-pair-definitions:

Local pair definitions
----------------------

Sometimes, a globally defined pair is not appropriate for certain major modes. You can redefine globally defined pairs to have different definition in specific major modes. For example, globally defined pair `````` is used in ``markdown-mode`` to insert inline code. However, ``emacs-lisp-mode`` uses ```'`` for links in comments and in ``LaTeX-mode`` this pair is used for quotes. Since they share the opening sequence (the "trigger"), it's impossible to have both defined globally at the same time. Therefore, it is desired to redefine this global pair to this new value locally.

That is accomplished by using ``sp-local-pair`` function:

.. code-block:: emacs-lisp

   (sp-local-pair 'emacs-lisp-mode "`" "'") ;; adds `' as a local pair in emacs-lisp-mode


If a global pair with the same trigger does not exist, the pair is defined locally and will only be used in the specified mode. Therefore, you do not need to define a pair globally and then overload it locally. The local definition is sufficient.

Instead of one mode, you can also specify a list to handle multiple modes at the same time (for example ``'(emacs-lisp-mode LaTeX-mode)``).

If you specify a parent major mode all the derived modes will automatically inherit all the definitions.  If you want to add a pair to all programming modes you can define it for ``prog-mode``.  If you want to add a pair to all text modes you can define it for ``text-mode``.

You can use the following snippet to get all the parent major modes of the ``major-mode`` in the current buffer

.. code-block:: emacs-lisp

    (let ((parents (list major-mode)))
      (while (get (car parents) 'derived-mode-parent)
        (push (get (car parents) 'derived-mode-parent) parents))
      parents)


Local pairs can be removed by calling ``sp-local-pair`` with optional keyword argument ``:actions`` with value ``:rem``:

.. code-block:: emacs-lisp

   (sp-local-pair 'LaTeX-mode "`" nil :actions :rem)


.. warning:: This only removes the pairs you have previously added using ``sp-local-pair``. It does not remove/disable a global pair in the specified mode. If you want to disable some pair in specific modes, set its permissions accordingly.

.. el:macro:: sp-with-modes mode-or-modes &rest forms

When configuring a mode it is often the case that we modify multiple pairs at the same time.  The macro ``sp-with-modes`` automatically supplies the ``mode-or-modes`` as first argument to all later forms (it can be a single symbol or a list of symbols for the multiple major modes).

.. code-block:: emacs-lisp

   (sp-with-modes 'emacs-lisp-mode
     ;; disable ', it's the quote character!
     (sp-local-pair "'" nil :actions nil)
     ;; also only use the pseudo-quote inside strings where it
     ;; serves as hyperlink.
     (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)

Named pair definitions (buffer-local)
----------------------

In addition to using the major mode or a parent mode you can also use an arbitrary symbol as the name of the configuration.  This way you can build sets of pairs independent of the major mode hierarchies and you can apply them locally to buffers as you see fit.

The syntax for defining custom named definitions is the same as with ``sp-local-pair`` only except a major mode as the first argument you pass your desired name.

You can for example define a set of escaped pairs to be used cross-major-mode

.. code-block:: emacs-lisp

    (sp-local-pair 'escaped-pairs "\\<" "\\>")
    (sp-local-pair 'escaped-pairs "\\`" "\\'")

You can then apply this definition buffer-locally to the current buffer with

.. code-block:: emacs-lisp

    (sp-update-local-pairs 'escaped-pairs)

This will merge this named configuration into the current buffer's ``sp-local-pairs`` definitions.

Alternatively you can also use an *anonymous* configuration.  The configuration is a plist of arguments with the same meaning as those of ``sp-local-pair`` with the additional requirement of adding ``:open`` and ``:close`` keywords for the opening and closing delimiters.  You can also pass a list of such plists to apply all of them at once.

.. warning:: Make sure to specify at least one action in the ``:actions`` key otherwise the pair will be removed by the virtue of having no actions.  When using ``sp-local-pair`` many of the keyword arguments get sensible defaults so you don't have to specify them; this is **not** the case when using an anonymous configuration directly.  When possible, use mode configrations or named configurations.

This can be used for example for local configuration with major-mode hooks:

.. code-block:: emacs-lisp

   (defun my-php-mode-init ()
     (sp-update-local-pairs '(:open "#"
                              :close "#"
                              :actions (insert))))
   (add-hook 'php-mode-hook 'my-php-mode-init)

This will add the ``# #`` pair to ``php-mode`` buffers via the major mode hook.
