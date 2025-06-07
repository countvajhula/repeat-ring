;;; repeat-ring.el --- Structured and configurable repetition -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/repeat-ring
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (virtual-ring "0.0") (pubsub "0.0"))

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

(require 'virtual-ring)
(require 'pubsub)
(require 'mantra)

(defconst repeat-ring-default-size 20)

(defun repeat-ring-make (name &optional size)
  "Make a repeat ring named NAME of size SIZE.

A repeat ring is a fixed-size ring. It maintains peculiar state to
distinguish fresh commands from repeated commands, and also leverages
the state of the underlying virtual ring to track which command is
being repeated.

A repeat ring may optionally be automatically populated with parsed
key sequences through a pub/sub mechanism using the `pubsub' package."
  (let* ((size (or size repeat-ring-default-size))
         (ring (virtual-ring-make size)))
    (vector name ring nil)))

(defconst repeat-ring--index-name 0
  "The index of the name of the repeat ring.

This name is used in any subscriptions to mantra parsers via pubsub,
and may be used to unsubscribe the ring from these key sequences.")

(defconst repeat-ring--index-ring 1
  "The index of the underlying ring in a repeat ring.")

(defconst repeat-ring--index-repeating 2
  "The index of the item being repeated (if any) in a repeat ring.")

(defun repeat-ring-name (rring)
  "Get the name of the repeat ring RRING."
  (seq-elt rring repeat-ring--index-name))

(defun repeat-ring-ring (rring)
  "Get the underlying ring in RRING."
  (seq-elt rring repeat-ring--index-ring))

(defun repeat-ring-repeating (rring)
  "Get the item being repeated in RRING."
  (seq-elt rring repeat-ring--index-repeating))

(defun repeat-ring-set-repeating (rring repeating)
  "Flag that an item is being repeated in RRING.

This sets the REPEATING field, whose non-nil value implies repetition
is in progress."
  (aset rring repeat-ring--index-repeating repeating))

(defun repeat-ring-clear-repeating (rring)
  "Clear the repeating flag in RRING."
  (aset rring repeat-ring--index-repeating nil))

(defun repeat-ring-subscribe (rring topic)
  "Subscribe RRING to TOPIC.

The ring will be subscribed to TOPIC using its name as the subscriber
name.  This allows the ring name to be used to subsequently unsubscribe
from TOPIC, if desired."
  (pubsub-subscribe topic
                    (repeat-ring-name rring)
                    (apply-partially #'repeat-ring-store rring)))

(defun repeat-ring-unsubscribe (rring topic)
  "Unsubscribe RRING from TOPIC."
  (pubsub-unsubscribe topic
                      (repeat-ring-name rring)))

(defun repeat-ring--repeat (rring mantra)
  "Repeat MANTRA on RRING.

This sets the `repeating' field to MANTRA, which encodes this state
in the ring so that the resulting fresh key sequence is appropriately
handled when we attempt to store it."
  (repeat-ring-set-repeating rring mantra)
  (mantra-eval mantra))

(defun repeat-ring-repeat (rring)
  "Repeat the last command on the repeat ring RRING."
  (let ((to-repeat (virtual-ring-current-entry
                    (repeat-ring-ring rring))))
    (repeat-ring--repeat rring to-repeat)))

(defun repeat-ring-repeat-pop (rring)
  "Cycle to the previous entry in the repeat ring RRING.

This undoes the previous repetition, removes the record of the
repetition in the ring, and executes the previous entry."
  (let ((ring (repeat-ring-ring rring)))
    (undo-only 1)
    (when (virtual-ring-head-rotated-p ring)
      (virtual-ring-remove-last ring))
    (virtual-ring-rotate-backwards ring)
    (repeat-ring-repeat rring)))

(defun repeat-ring-repeat-recent (rring)
  "Select a recent command on RRING and repeat it."
  (let* ((mantras (virtual-ring-contents
                    (repeat-ring-ring rring)))
         (key-descriptions (mapcar #'key-description
                                   mantras))
         (to-repeat-str (completing-read "Repeat: "
                                         key-descriptions))
         (index (cl-position to-repeat-str key-descriptions :test #'equal))
         (to-repeat (nth index mantras)))
    (repeat-ring--repeat rring to-repeat)))

(defun repeat-ring-store (rring mantra)
  "Store MANTRA as an entry in RRING.

Resets the virtual head to the most recently stored element,
i.e., to MANTRA."
  (let* ((ring (repeat-ring-ring rring))
         (ring-empty (virtual-ring-empty-p ring))
         (last-stored-mantra (unless ring-empty
                                (virtual-ring-last-entry ring)))
         (successive-duplicate (equal mantra
                                      last-stored-mantra))
         (repetition (repeat-ring-repeating rring)))
    (when (or ring-empty
              ;; don't record successive duplicates
              (not successive-duplicate))
      ;; reset the head in any case, unless we're executing
      ;; a *repetition*, in which case, preserve the head.
      ;; Note that we still do record the repetition as a fresh
      ;; entry, as it is, in fact, the most recently executed
      ;; macro.
      (virtual-ring-store ring
                          mantra
                          repetition))
    (when repetition
      (repeat-ring-clear-repeating rring))))


(provide 'repeat-ring)
;;; repeat-ring.el ends here
