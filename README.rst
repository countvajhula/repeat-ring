.. image:: https://github.com/countvajhula/repeat-ring/actions/workflows/test.yml/badge.svg
    :target: https://github.com/countvajhula/repeat-ring/actions

.. image:: https://coveralls.io/repos/github/countvajhula/repeat-ring/badge.svg?branch=master
    :target: https://coveralls.io/github/countvajhula/repeat-ring?branch=master

.. image:: https://melpa.org/packages/repeat-ring-badge.svg
    :alt: MELPA
    :target: https://melpa.org/#/repeat-ring

.. image:: https://stable.melpa.org/packages/repeat-ring-badge.svg
    :alt: MELPA Stable
    :target: https://stable.melpa.org/#/repeat-ring

repeat-ring
===========
A structured and general way to record and replay your activity within Emacs, inspired by both Vim and Emacs keyboard macros.

Repeat ring provides:

1. A repeat *history* of configurable size, storing recent activity of *any length* on handy rings that you could specialize to *different contexts*.

2. Recording of rich activity based on your *intent*, not just on what you *typed* (e.g., it is usable in completion settings, unlike ordinary keyboard macros).

3. *Implicitly* record activity based on patterns you specify (in addition to explicitly, if you like).

Tutorial
--------

We'll learn about Repeat Ring by example.

First, let's create a repeat ring.

.. code-block:: elisp

  (defvar my-global-repeat-ring
    (repeat-ring-make "my-global-repeat-ring"))

And let's bind some keys to repeat-ring commands that we will soon use.

.. code-block:: elisp

  (global-set-key (kbd "C-c .")
                  (lambda ()
                    (interactive)
                    (repeat-ring-repeat my-global-repeat-ring)))
  (global-set-key (kbd "C-c C-.")
                  (lambda ()
                    (interactive)
                    (repeat-ring-repeat-pop my-global-repeat-ring)))
  (global-set-key (kbd "C-c M-.")
                  (lambda ()
                    (interactive)
                    (repeat-ring-repeat-recent my-global-repeat-ring)))

Repeat rings store and replay *mantras*, which are a generalized form of keyboard macro. In particular, any keyboard macro is also a mantra. Let's add one to the ring.

.. code-block:: elisp

  (repeat-ring-store my-global-repeat-ring "hello")

This stores the literal key sequence "hello", acting just like a standard keyboard macro. Typically, for repeat purposes, you'd like such macros to be recorded *implicitly* without action on your part — and we'll see how to do that soon — but for now we are only interested in understanding how everything works, so we'll work with the rings manually. Now that we have an entry in the repeat ring, place your cursor anywhere in a scratch buffer and hit ``C-c .``.

It should have inserted the text "hello" into the buffer.

But the thing about keyboard macros is that they are purely syntax without semantics. The same macro may mean one thing when you entered it the first time, but a different thing when you execute it again at a later point.

For instance, if you happen to be an Evil user, then you know that "h" means "go backwards one character" while in Normal state, but in insert state, it means "insert the character h" (Evil is the most convenient example here to illustrate the point — if you aren't an Evil user, please read on, and you should still get the idea!). Simply replaying the keyboard macro while in Normal state would *not* have the effect of inserting "hello" into the buffer, even though that's what you may have intended when you stored the macro.

So instead, let's store a different kind of mantra, an explicit buffer insertion, on the repeat ring.

.. code-block:: elisp

  (repeat-ring-store my-global-repeat-ring '(insertion "hello"))

Now, ``C-c .`` inserts "hello" into the buffer, no matter what the keys in "hello" may happen to mean in your particular keybinding (e.g. Evil) configuration.

This is useful in cases where we are interested in storing the *effect* of our keyboard activity and not the keyboard activity itself, and where these aren't the same. For instance, in modern completion UIs, selecting a candidate from a completion menu may entail the key sequence "he\<enter\>", if you happen to select "hello" from the completion menu by hitting Enter. Now, replaying this as a *key sequence* would not insert "hello" into the buffer but "he\\n"!

Mantras are able to express such meaningful activities independently of any keybindings, and can even express arbitrary actions in the form of lambdas, making them a very general way to emulate activity within Emacs.

.. code-block:: elisp

  (repeat-ring-store my-global-repeat-ring
                     (lambda (&rest _args)
                       (message "blah!")))

Of course, typically, for the purposes of repetition, you'd want the repeat ring to store your activity *implicitly*. As a simple example of this, we can write a basic version of Emacs's built-in repeat command by subscribing to, and storing for repetition, all key sequences you enter while using Emacs:

.. code-block:: elisp

  (repeat-ring-subscribe my-global-repeat-ring
                         mantra-key-sequences-topic)

Now, each time you enter any key sequence that results in a command being executed, it will store that on the repeat ring for repetition.

By defining custom mantra parsers, you can create rings that record anything from text changes to window configuration shifts to interactions with a specific package. For instance, the Symex package uses repeat-ring and mantra together to repeat complex structural editing operations performed while in Symex mode.

Comparison with Alternatives
----------------------------

Built-in repeat
~~~~~~~~~~~~~~~

Emacs includes a built-in way to repeat any command, via ``C-x z``. This can be handy, but if you want to repeat an *older* command that wasn't the most recent one, you're out of luck. Additionally, this feature only allows you to repeat *individual commands*, not longer sequences of activity.

Repeat ring allows you to repeat any recent action, and these can be of any length, not just individual commands.

Keyboard macro ring
~~~~~~~~~~~~~~~~~~~

Emacs also includes a built-in keyboard macro ring. This allows you to start recording a macro using ``C-x (`` and end recording with ``C-x )``. Now, the entire sequence — which could be of any length — may be repeated at any time. Not only that, but these recorded sequences are maintained on a ring so that you can repeat any of them, not just the most recently recorded one. Still, this suffers from a few shortcomings: (1) it only records key sequences, which on their own aren't always enough to capture what you mean, (2) there is only one ring, so you cannot tailor your commands to specific contexts, and (3) the recording must be made by you, manually. It does not *implicitly* record your activity for repetition, the way the basic repeat command does.

Repeat ring supports any number of rings, each of which may be populated implicitly based on patterns of activity that you define. This allows you to have context-sensitive rings. Additionally, the stored sequences are richer than just key sequences and can capture the meaning of the changes you made.

Evil "dot"
~~~~~~~~~~

Finally, Vim has the famous "dot" operator, usable in Emacs via ``evil-repeat``. This command not only stores commands for repetition implicitly, it also stores a navigable history of them, and also stores them in a way that's richer than just key sequences. Yet, even this has drawbacks: (1) it's specialized to Evil and isn't a general tool available to Emacs users, no matter their editing preferences, (2) there's only one history and it isn't context sensitive.

Repeat-ring is more expressive than Evil's dot, and it's usable in any Emacs setting, not just for Evil. In particular, it should be straightforward to implement evil-repeat itself using repeat-ring.

Installation
------------

Repeat-ring is not on a package archive such as `MELPA <https://melpa.org/>`_ yet, but you can install it using `Straight.el <https://github.com/radian-software/straight.el>`_ (or `Elpaca <https://github.com/progfolio/elpaca>`_) by putting this somewhere in your :code:`.emacs.d`:

.. code-block:: elisp

  (use-package repeat-ring
    :straight
    (repeat-ring
      :type git
      :host github
      :repo "countvajhula/repeat-ring"))

Overview
--------

Repeat-ring uses the `Mantra <https://github.com/countvajhula/mantra>`_ parser to allow you to define "regex"-like patterns on your keyboard activity, where each such pattern has a corresponding ring. Every time such a pattern is encountered in your activity, it is stored in that ring (avoiding successive duplicates). You can then repeat as well as navigate these recorded key sequences on each of these distinct rings. The activity stored on each ring is governed exclusively by the patterns and conditions you indicate for that ring, and do not interfere with one another.

It's like Emacs's keyboard macros in that there is a history of macros maintained on a ring, and it's like Vim's dot operator in that it records such macros *implicitly* based on patterns that you configure. In addition, it supports any number of rings and arbitrary conditions for recording activity, enabling a rich diversity of context-sensitive repeatable actions.

Further Reading
---------------

This package generalizes Vim's dot operator and is based on the perspective developed in `A Vimlike Fluency <https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/>`_, especially:

- `Living the High Life <https://countvajhula.com/2021/02/02/vim-tip-of-the-day-living-the-high-life/>`_
- `Saying More (Macros) <https://countvajhula.com/2021/02/08/vim-tip-of-the-day-saying-more-macros/>`_

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
