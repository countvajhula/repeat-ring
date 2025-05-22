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
    (vector ring action 0 nil)))

(defconst repeat-ring--index-ring 0
  "The index of the underlying ring in a repeat ring.")

(defconst repeat-ring--index-action 1
  "The index of the action in a repeat ring.")

(defconst repeat-ring--index-head 2
  "The index of the virtual head in a repeat ring.")

(defconst repeat-ring--index-repeating 3
  "The index of the item being repeated (if any) in a repeat ring.")

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

(defun repeat-ring-head-rotated-p (rring)
  "Whether RRING's virtual head is rotated from its true head."
  (not (= (repeat-ring-ring-head rring)
          0)))

(defun repeat-ring-repeating (rring)
  "Get the item being repeated in RRING."
  (seq-elt rring repeat-ring--index-repeating))

(defun repeat-ring-set-repeating (rring repeating)
  "Flag that an item is being repeated in RRING."
  (aset rring repeat-ring--index-repeating repeating))

(defun repeat-ring-clear-repeating (rring)
  "Clear the repeating flag in RRING."
  (aset rring repeat-ring--index-repeating nil))

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

(defun repeat-ring-repeat ()
  "Repeat the last command on the most recently used repeat ring."
  (interactive)
  (let* ((rring (dynaring-value repeat-ring-active-rings))
         (to-repeat (repeat-ring-current-command rring)))
    (repeat-ring-set-repeating rring to-repeat)
    (funcall (repeat-ring-ring-action rring)
             to-repeat)))

(defun repeat-ring-repeat-for-ring (rring)
  "Repeat the last command on the repeat ring RRING."
  (interactive)
  (let ((to-repeat (repeat-ring-current-command rring)))
    (repeat-ring-set-repeating rring to-repeat)
    (funcall (repeat-ring-ring-action rring)
             to-repeat)))

(defun repeat-ring-remove-last (rring)
  "Remove the last (most recent) entry from RRING.

Adjust head if necessary."
  (ring-remove (repeat-ring-ring-ring rring)
               0))

(defun repeat-ring-repeat-pop (rring)
  "Cycle to the previous entry in the repeat ring RRING.

This undoes the previous repetition, removes the record of the
repetition in the ring, and executes the previous entry."
  (undo-only 1)
  (when (repeat-ring-head-rotated-p rring)
    (repeat-ring-remove-last rring))
  (repeat-ring-rotate-backwards rring)
  (repeat-ring-repeat-for-ring rring))

(defun repeat-ring-contents (rring)
  "Contents of repeat ring RRING."
  (ring-elements
   (repeat-ring-ring-ring rring)))

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
         (repetition (repeat-ring-repeating rring)))
    (when (or ring-empty
              ;; don't record successive duplicates
              (not successive-duplicate))
      (ring-insert ring key-seq))
    ;; reset the head in any case, unless we're executing
    ;; a *repetition*, in which case, preserve the head.
    ;; Note that we still do record the repetition as a fresh
    ;; entry, as it is, in fact, the most recently executed
    ;; macro.
    (if repetition
        (repeat-ring-clear-repeating rring)
      (repeat-ring-ring-reset-head rring))))


(provide 'repeat-ring)
;;; repeat-ring.el ends here
