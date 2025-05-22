;;; repeat-ring.el --- Structured and configurable repetition -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/repeat-ring
;; Version: 0.0
;; Package-Requires: ((emacs "25.1") (dynaring "0.3") (virtual-ring "0.0") (pubsub "0.0") (mantra "0.0"))

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
(require 'dynaring)
(require 'pubsub)

(defconst repeat-ring-default-size 20)

(defvar repeat-ring-active-rings
  (dynaring-make)
  "Repeat rings that are actively recording commands.

This is a dynamically sized ring.")

(defun repeat-ring-make (&optional size)
  "Make a repeat ring named NAME of size SIZE.

A repeat ring is an ordinary fixed-size ring that populates key
sequences parsed on the topic NAME."
  (let* ((size (or size repeat-ring-default-size))
         (ring (virtual-ring-make size)))
    (vector ring nil)))

(defconst repeat-ring--index-ring 0
  "The index of the underlying ring in a repeat ring.")

(defconst repeat-ring--index-repeating 1
  "The index of the item being repeated (if any) in a repeat ring.")

(defun repeat-ring-ring (rring)
  "Get the underlying ring in RRING."
  (seq-elt rring repeat-ring--index-ring))

(defun repeat-ring-repeating (rring)
  "Get the item being repeated in RRING."
  (seq-elt rring repeat-ring--index-repeating))

(defun repeat-ring-set-repeating (rring repeating)
  "Flag that an item is being repeated in RRING."
  (aset rring repeat-ring--index-repeating repeating))

(defun repeat-ring-clear-repeating (rring)
  "Clear the repeating flag in RRING."
  (aset rring repeat-ring--index-repeating nil))

(defun repeat-ring-subscribe (rring &optional topic)
  "Subscribe RRING to TOPIC.

If TOPIC isn't specified, RRING will not be subscribed to keyboard
events and it will be up to you to populate it with repeatable
commands any way you see fit. If TOPIC is `all', then the ring will be
subscribed to all complete key sequences.

Also add RRING to the global dynamic ring of repeat rings."
  (dynaring-insert repeat-ring-active-rings
                   rring)
  (let ((topic (if (eq 'all topic)
                   ;; parser that publishes all complete key sequences
                   "mantra-all-key-sequences"
                 topic)))
    (when topic
      (pubsub-subscribe topic
                        (apply-partially #'repeat-ring-store rring)))))

(defun repeat-ring-unsubscribe (rring)
  "Remove RRING from the ring of active rings.

This doesn't unsubscribe the ring from pub/sub, as identifying the
corresponding subscriber callback isn't generally possible unless we
change the approach used in the `pubpub' package to introduce an
indirection via a symbolic identifier that would then pull up the
callback, instead of calling the callback directly."
  (dynaring-delete repeat-ring-active-rings rring))

(defun repeat-ring--repeat (rring key-seq)
  "Repeat KEY-SEQ.

Restore RRING as the head of `repeat-ring-active-rings', the dynamic
ring of repeat rings where head is the most recently used one."
  (repeat-ring-set-repeating rring key-seq)
  (execute-kbd-macro key-seq)
  (dynaring-break-insert repeat-ring-active-rings rring))

(defun repeat-ring-repeat-for-ring (rring)
  "Repeat the last command on the repeat ring RRING."
  (let ((to-repeat (virtual-ring-current-entry
                    (repeat-ring-ring rring))))
    (repeat-ring--repeat rring to-repeat)))

(defun repeat-ring-current-ring ()
  "The most recently used repeat ring."
  (dynaring-value repeat-ring-active-rings))

(defun repeat-ring-repeat ()
  "Repeat the last command on the most recently used repeat ring."
  (interactive)
  (let ((rring (repeat-ring-current-ring)))
    (repeat-ring-repeat-for-ring rring)))

(defun repeat-ring-repeat-pop-for-ring (rring)
  "Cycle to the previous entry in the repeat ring RRING.

This undoes the previous repetition, removes the record of the
repetition in the ring, and executes the previous entry."
  (let ((ring (repeat-ring-ring rring)))
    (undo-only 1)
    (when (virtual-ring-head-rotated-p ring)
      (virtual-ring-remove-last ring))
    (virtual-ring-rotate-backwards ring)
    (repeat-ring-repeat-for-ring rring)))

(defun repeat-ring-repeat-pop ()
  "Cycle to the previous entry in the most recently used repeat ring.

This undoes the previous repetition, removes the record of the
repetition in the ring, and executes the previous entry."
  (interactive)
  (let ((rring (repeat-ring-current-ring)))
    (repeat-ring-repeat-pop-for-ring rring)))

(defun repeat-ring-repeat-recent-for-ring (rring)
  "Select a recent command on RRING and repeat it."
  (let* ((key-seqs (virtual-ring-contents
                    (repeat-ring-ring rring)))
         (key-descriptions (mapcar #'key-description
                                   key-seqs))
         (to-repeat-str (completing-read "Repeat: "
                                         key-descriptions))
         (index (cl-position to-repeat-str key-descriptions :test #'equal))
         (to-repeat (nth index key-seqs)))
    (repeat-ring--repeat rring to-repeat)))

(defun repeat-ring-repeat-recent ()
  "Select a recent command on the most recently used repeat ring and repeat it."
  (interactive)
  (let ((rring (repeat-ring-current-ring)))
    (repeat-ring-repeat-recent-for-ring rring)))

(defun repeat-ring-store (rring key-seq)
  "Store KEY-SEQ as an entry in RRING.

Resets the virtual head to the most recently stored element,
i.e., to KEY-SEQ."
  (let* ((ring (repeat-ring-ring rring))
         (ring-empty (virtual-ring-empty-p ring))
         (last-stored-key-seq (unless ring-empty
                                (virtual-ring-last-entry ring)))
         (successive-duplicate (equal key-seq
                                      last-stored-key-seq))
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
                          key-seq
                          repetition))
    (when repetition
      (repeat-ring-clear-repeating rring))))


(provide 'repeat-ring)
;;; repeat-ring.el ends here
