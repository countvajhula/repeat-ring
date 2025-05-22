;;; repeat-ring.el --- Structured and configurable repetition -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/repeat-ring
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (dynaring "0.3"))

;; This file is NOT a part of Gnu Emacs.

;; This work is "part of the world."  You are free to do whatever you
;; like with it and it isn't owned by anybody, not even the
;; creators.  Attribution would be appreciated and is a valuable
;; contribution in itself, but it is not strictly necessary nor
;; required.  If you'd like to learn more about this way of doing
;; things and how it could lead to a peaceful, efficient, and creative
;; world, and how you can help, visit https://drym.org.
;;
;; This paradigm transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.

;;; Commentary:

;; Structured and configurable repetition.

;;; Code:

(require 'ring)
(require 'dynaring)
(require 'pubsub)

(defconst repeat-ring-default-size 20)

(defvar repeat-ring-active-rings
  (dynaring-make)
  "Repeat rings that are actively recording commands.

This is a dynamically sized ring.")

(defun repeat-ring-make (action &optional size)
  "Make a repeat ring named NAME of size SIZE.

A repeat ring is an ordinary fixed-size ring that populates key
sequences parsed on the topic NAME."
  (let* ((size (or size repeat-ring-default-size))
         (ring (make-ring size)))
    (vector ring action 0)))

(defconst repeat-ring--index-ring 0
  "The index of the underlying ring in a repeat ring.")

(defconst repeat-ring--index-action 1
  "The index of the action in a repeat ring.")

(defconst repeat-ring--index-head 2
  "The index of the virtual head in a repeat ring.")

(defun repeat-ring-ring-ring (rring)
  "Get the underlying ring in RRING."
  (seq-elt rring repeat-ring--index-ring))

(defun repeat-ring-ring-action (rring)
  "Get the action of RRING."
  (seq-elt rring repeat-ring--index-action))

(defun repeat-ring-ring-head (rring)
  "Get the virtual head of RRING."
  (seq-elt rring repeat-ring--index-head))

(defun repeat-ring-ring-set-head (rring new-head)
  "Set head on RRING to NEW-HEAD."
  (aset rring repeat-ring--index-head new-head))

(defun repeat-ring-ring-reset-head (rring)
  "Reset head on RRING to 0 (most recent addition)."
  (repeat-ring-ring-set-head rring 0))

(defvar repeat-ring-recent-keys
  (repeat-ring-make #'execute-kbd-macro)
  "A ring to store all recent key sequences.")

(defun repeat-ring-subscribe (rring topic)
  "Subscribe RRING to TOPIC."
  (pubsub-subscribe topic
                    (apply-partially #'repeat-ring-store rring)))

(defun repeat-ring-initialize ()
  "Initialize repeat ring.

This adds key sequence publishing to Emacs's `pre-command-hook` so
that active repeat rings can be notified of them, and adds an initial
basic repeat ring that stores all key sequences."
  ;; add the default repeat ring that stores all key sequences
  (dynaring-insert repeat-ring-active-rings
                   repeat-ring-recent-keys)
  (repeat-ring-subscribe repeat-ring-recent-keys
                         "mantra-all-key-sequences"))

(defun repeat-ring-last-command (rring)
  "The last command stored on the repeat ring RRING."
  (let ((ring (repeat-ring-ring-ring rring)))
    (ring-ref ring 0)))

;; TODO: review naming, with current and last could be confusing
(defun repeat-ring-current-command (rring)
  "The command stored on the repeat ring RRING at the current virtual head."
  (let ((ring (repeat-ring-ring-ring rring))
        (head (repeat-ring-ring-head rring)))
    (ring-ref ring head)))

(defun repeat-ring-rotate-forwards (rring)
  "Rotate RRING forwards."
  (interactive)
  (let ((head (repeat-ring-ring-head rring)))
    (repeat-ring-ring-set-head rring (1- head))))

(defun repeat-ring-rotate-backwards (rring)
  "Rotate RRING backwards."
  (interactive)
  (let ((head (repeat-ring-ring-head rring)))
    (repeat-ring-ring-set-head rring (1+ head))))

(defgroup repeat-ring nil
  "A ring of recent commands for repetition."
  :group 'editing)

(defcustom repeat-ring-noop "C-u 0 h"
  "A key sequence that has no effect.

This is dependent on your Emacs config, so while C-u 0 <command>
should work in most cases, if you, for instance, happen to be an Evil
user and have rebound C-u to something else, then you should use that
other prefix here instead of C-u. Any key sequence that has _no effect
at all_ can be used here.

This key sequence is prefaced to repeated key sequences so that they
are identifiable as repeated sequences and are not stored in the ring
as fresh sequences (otherwise, it would be necessary to introduce
global state to signal that we are in the middle of repetition, which
is fragile and requires much effort and care that is made superfluous
by employing this \"no-op\" trick."
  :type 'string
  :group 'repeat-ring)

(setq repeat-ring-noop "M-u 0 h")  ; testing - move to init config

(defvar repeat-ring--noop
  (string-to-vector
   (kbd repeat-ring-noop))
  "The vector representation of the noop sequence used internally.")

(defun repeat-ring--preface-with-noop (key-seq)
  "Preface KEY-SEQ with a noop.

This is useful to identify that we are in the process of repeating an
action rather than entering a new key sequence, to avoid storing such
repetitions as fresh entries on the repeat ring. Doing it this way
avoids introducing a global state variable to carry this information,
allowing us to encode this state information into the key sequence
data itself and thereby communicate it to the function responsible for
storing new sequences."
  (vconcat repeat-ring--noop key-seq))

(defun repeat-ring-repeat ()
  "Repeat the last command on the most recently used repeat ring."
  (interactive)
  (let ((rring (dynaring-value repeat-ring-active-rings)))
    (funcall (repeat-ring-ring-action rring)
             (repeat-ring--preface-with-noop
              (repeat-ring-current-command rring)))))

(defun repeat-ring-repeat-for-ring (rring)
  "Repeat the last command on the repeat ring RRING."
  (interactive)
  (funcall (repeat-ring-ring-action rring)
           (repeat-ring--preface-with-noop
            (repeat-ring-current-command rring))))

(defun repeat-ring-contents (rring)
  "Contents of repeat ring RRING."
  (ring-elements
   (repeat-ring-ring-ring rring)))

(defun repeat-ring--vector-prefix-p (pfx vec)
  "Is PFX a prefix of VEC?"
  (if (seq-empty-p pfx)
      t
    (if (seq-empty-p vec)
        nil
      (and (equal (aref pfx 0)
                  (aref vec 0))
           (repeat-ring--vector-prefix-p (seq-subseq pfx 1) ; note: inefficient for vectors
                                         (seq-subseq vec 1))))))

(defun repeat-ring-store (rring key-seq)
  "Store KEY-SEQ as an entry in RRING.

Resets the virtual head to the most recently stored element,
i.e., to KEY-SEQ."
  (let* ((ring (repeat-ring-ring-ring rring))
         (ring-empty (ring-empty-p ring))
         (last-stored-key-seq (unless ring-empty
                                (repeat-ring-last-command rring)))
         (successive-duplicate (equal key-seq
                                      last-stored-key-seq))
         (repetition (repeat-ring--vector-prefix-p repeat-ring--noop
                                                   last-stored-key-seq)))
    (when (or ring-empty
              ;; don't record successive duplicates
              (not successive-duplicate))
      (ring-insert ring key-seq))
    ;; reset the head in any case, unless we're executing
    ;; a *repetition*, in which case, preserve the head.
    ;; Note that we still do record the repetition as a fresh
    ;; entry, as it is, in fact, the most recently executed
    ;; macro.
    (unless repetition
      (repeat-ring-ring-reset-head rring))))


(provide 'repeat-ring)
;;; repeat-ring.el ends here
