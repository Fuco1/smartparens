.. Smartparens documentation master file, created by
   sphinx-quickstart on Sat Sep 10 22:33:55 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Smartparens's documentation!
=======================================

Smartparens is minor mode for Emacs that *deals with parens pairs and
tries to be smart about it*. It started as a unification effort to
combine functionality of several existing packages in a single,
compatible and extensible way to deal with parentheses, delimiters, tags
and the like. With the basic features found in other packages it also
brings many improvements as well as completely new features. Here's a
highlight of some features, for a complete list and detailed
documentation look in Getting Started below.

-  support for pairs of any length, for example ``"\{" "\}"`` pair used
   in LaTeX to typeset literal braces in math mode. These are fully
   :ref:`user definable and customizable<pair-management>`. Pairs can
   have same or different strings for opening and closing part.
-  intelligent handling of closing pairs. If user types ``(``, ``(|)``
   is inserted. If then the user types ``word)`` the result is
   ``(word)|`` not ``(word)|)``.
-  automatic deletion of complete pairs. With pair ``("\{" "\}")``,
   ``\{|\}`` and backspace will remove both delimiters. ``\{\}|`` and
   backspace will remove the closing pair, and result in ``\{|``.
   Hitting backspace again will remove the opening pair. You can also
   set it to skip over the pairs to keep the structure balanced instead
   by enabling ``smartparens-strict-mode`` (a la paredit).
-  wraps active region in defined pairs or special structured tag pairs
   for "tag-modes" (xml/html...). Different tags are supported, for
   example, languages that would use ``{tag}`` instead of ``<tag>`` or
   LaTeX's ``\begin{} \end{}`` pair. *Everything* is user definable as
   usual.
-  :ref:`Jumping around<working-with-expressions>` the pairs (extending
   forward-sexp and similar functions to custom user pairs)
-  Functions to :ref:`manipulate<working-with-expressions>`
   s-expressions, delete, wrap and unwrap, extend and contract..

Almost all features are fully customizable via
``M-x customize-group smartparens``. You can turn many behaviours on or
off to fit your workflow.

Getting Started
===============

The process of installing smartparens is as simple as calling
``package-install``. However, if you want to learn in detail what
packages smartparens depend on and what settings it modifies in order
to function, read the :ref:`installation` manual.

The :ref:`quick tour<quick-tour>` lists the most basic use cases,
while here follows a list of features smartparens provide, complete
with detailed explanation on how to fine-tune it to your very own
taste.

-  :ref:`pair-management`. How do add and remove
   pairs, how to overload global pairs locally.
-  :ref:`Permissions<permissions>`. How to set permissions for each
   pair, to restrict in which modes and which contexts it is permitted
   to automatically insert the closing pair or wrap the active region.
-  :ref:`Action hooks<action-hooks>`.  How to set up hooks to perform
   custom operations before or after smartparens actions.
-  :ref:`Wrapping<wrapping>`. Automatically wrap active regions with
   pairs or structured tags (for example html tags). Learn how to add
   or remove tags and how you can customize wrapping to fit your
   needs.
-  :ref:`Automatic escaping<automatic-escaping>`. Were you ever annoyed
   by quote escaping in strings? Well, no more! Learn how you can let
   smartparens do the boring for you.
-  :ref:`Navigation<working-with-expressions>`. Navigating balanced
   expressions is a powerful way to move around in buffers and edit
   code. Learn how you can quickly jump back and forth around the paired
   expressions.
-  :ref:`Expression manipulation<working-with-expressions>`. Extend,
   contract, split, splice, wrap, unwrap and much more! How to quickly
   transform your elisp (or any other!) code with powerful
   manipulation functions.
-  :ref:`Hybrid S-expressions<hybrid-expressions>`. Sometimes,
   S-expressions just aren't going to cut it. Learn how to edit (not
   only) "line based" languages (C, Java, C#,...) in a way that helps
   you to keep the delimiters balanced.
-  :ref:`Show smartparens mode<show-smartparens-mode>`. Do you want to
   see where a pair starts and where it ends? Turn on the
   ``show-smartparens-mode`` and let smartparens highlight the pairs.
-  :ref:`User interface<user-interface>`. Smartparens provide a few
   user-interface features, like highlighting currently "active"
   region between pair delimiters and during wrapping. If the defaults
   doesn't fit your color scheme, read this section and learn how to
   customize it.
-  :ref:`example-configuration`. Author's current working smartparens
   configuration. See how the knowledge you've just obtained works in
   practice.
-  :ref:`Default configuration<default-configuration>`. Smartparens
   ships with powerful default configuration. Chances are that you
   won't even need to configure anything at all! Look at the internals
   and see how is smartparens configured by default.

After you familiarize yourself with smartparens, make sure to read
:ref:`tips and tricks<tips-and-tricks>` to get that extra 20% of coolness out of it ;)

Migrating from paredit
----------------------

Among other features, smartparens also replicate a lot of paredit
functionality and extend it to arbitrary pairs and modes. It also
provides many improvements like adding numeric or raw prefix arguments
to modify the behaviour of these commands. Read the
:ref:`paredit-and-smartparens` article for comparsion of paredit and
smartparens features.

Are you an elisp developer?
===========================

If you are, check out these articles about the internals of spartparens!
There are many interesting functions you can use in your own code that
deal with pairs and navigation.

-  :ref:`how-to-use-structural-functions-in-my-own-code`. Create powerful
   functions that operate on pair delimited structures.
-  :ref:`hooks-and-advices-smartparens-modify`, this is mostly for the
   situations where you would modify pre/post command hooks and want to
   learn how smartparens modify them.

And of course, if you write some cool plugin or extension for
smartparens, or have an idea how to do it, contribute!

Contributing
============

We love contributions. It is most awesome if they come with tests. Read
about our :ref:`testing<testing>` infrastructure.

Contents
========

.. toctree::
   :maxdepth: 2

   automatic-escaping
   pair-management
   permissions
   example-configuration


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
