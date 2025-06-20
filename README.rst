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
A structured and configurable way to keep track of and repeat activity, including key sequences, inspired by both Vim and Emacs keyboard macros.

Repeat-ring uses the Mantra parser to allow you to define "regex"-like patterns on your keyboard activity, where each such pattern has a corresponding ring. Every time such a pattern is encountered in your activity, it is stored in that ring (avoiding successive duplicates). You can then repeat as well as navigate these recorded key sequences on each of these distinct rings. The activity stored on each ring is governed exclusively by the patterns and conditions you indicate for that ring, and do not interfere with one another.

It's like Emacs's keyboard macros in that there is a history of macros maintained on a ring, and it's like Vim's dot operator in that it records such macros *implicitly* based on patterns that you configure. In addition, it supports any number of rings and arbitrary conditions for recording activity, enabling a rich diversity of context-sensitive repeatable macros.

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

How It Works
------------

(The following is mostly a description of how Mantra works. Repeat-ring is a small package that subscribes to Mantra parsers to implicitly store activity on stateful ("virtual") rings, and providing a handful of useful commands for repetition.)

Upon each key sequence being entered by you on the `Emacs command loop <https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Overview.html>`_, a listener (on the pre-command and post-command hooks) does basic "lexing" on this input to ensure that it isn't an empty or otherwise invalid sequence. Then, it notifies every configured repeat ring of the key sequence. Each ring checks its own condition to start recording the sequence. If the condition is met, or if recording is already in progress, then the key sequence is accumulated in a local state variable until either a stop condition is met (in which case the full composite sequence is stored in the ring), or an abort condition is met (in which case the state is cleared and is not stored).

The repeat rings themselves are fixed-size ring data structures, so that they contain a history of length N, with the oldest key sequences being overwritten as new ones are recorded. In addition, they support stateful "rotation" (via the virtual-ring package), allowing you to cycle through recent commands instead of just the latest one.

Key sequences on each repeat ring may be "repeated" by evaluating them as *mantras*, which are a generalized form of keyboard macro that can in principle express any kind of user activity.

Further Reading
---------------

This package generalizes Vim's dot operator and is based on the perspective developed in `A Vimlike Fluency <https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/>`_, especially:

- `Living the High Life <https://countvajhula.com/2021/02/02/vim-tip-of-the-day-living-the-high-life/>`_
- `Saying More (Macros) <https://countvajhula.com/2021/02/08/vim-tip-of-the-day-saying-more-macros/>`_

Non-Ownership
-------------

The freely released, copyright-free work in this repository represents an investment in a better way of doing things called attribution-based economics. Attribution-based economics is based on the simple idea that we gain more by giving more, not by holding on to things that, truly, we could only create because we, in our turn, received from others. As it turns out, an economic system based on attribution -- where those who give more are more empowered -- is significantly more efficient than capitalism while also being stable and fair (unlike capitalism, on both counts), giving it transformative power to elevate the human condition and address the problems that face us today along with a host of others that have been intractable since the beginning. You can help make this a reality by releasing your work in the same way -- freely into the public domain in the simple hope of providing value. Learn more about attribution-based economics at `drym.org <https://drym.org>`_, tell your friends, do your part.
