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

;; TODO: should the ring not be coupled to pubsub via the name?
(defun repeat-ring-make (name &optional size)
  "Make a repeat ring named NAME of size SIZE.

A repeat ring is an ordinary fixed-size ring that populates key
sequences parsed on the topic NAME."
  (let* ((size (or size repeat-ring-default-size))
         (ring (make-ring size)))
    (vector name ring)))

(defconst repeat-ring--index-name 0
  "The index of the name of the repeat ring.")

(defconst repeat-ring--index-ring 1
  "The index of the underlying ring in a repeat ring.")

(defun repeat-ring-ring-name (rring)
  "Get the name of the repeat-ring RRING."
  (seq-elt rring repeat-ring--index-name))

(defun repeat-ring-ring-ring (rring)
  "Get the underlying ring in RRING."
  (seq-elt rring repeat-ring--index-ring))

(defvar repeat-ring-recent-keys
  (repeat-ring-make "basic")
  "A ring to store all recent key sequences.")

(defun repeat-ring-initialize ()
  "Initialize repeat ring.

This adds key sequence publishing to Emacs's `pre-command-hook` so
that active repeat rings can be notified of them, and adds an initial
basic repeat ring that stores all key sequences."
  ;; add the default repeat ring that stores all key sequences
  (dynaring-insert repeat-ring-active-rings
                   repeat-ring-recent-keys)
  (let ((add-to-default-ring (lambda (key-seq)
                               (repeat-ring-store repeat-ring-recent-keys
                                                  key-seq))))
    (pubsub-subscribe (repeat-ring-ring-name repeat-ring-recent-keys)
                      add-to-default-ring)))

(defun repeat-ring-last-command (rring)
  "The last command stored on the repeat ring RRING."
  (let ((ring (repeat-ring-ring-ring rring)))
    (ring-ref ring 0)))

(defun repeat-ring-repeat ()
  "Repeat the last command on the most recently used repeat ring."
  (interactive)
  (execute-kbd-macro
   (repeat-ring-last-command
    (dynaring-value repeat-ring-active-rings))))

(defun repeat-ring-repeat-for-ring (rring)
  "Repeat the last command on the repeat ring RRING."
  (interactive)
  (execute-kbd-macro
   (repeat-ring-last-command rring)))

(defun repeat-ring-rotate-ring-forwards ()
  "Rotate the ring of repeat rings forwards."
  (interactive)
  (dynaring-rotate-left repeat-ring-active-rings))

(defun repeat-ring-rotate-ring-backwards ()
  "Rotate the ring of repeat rings backwards."
  (interactive)
  (dynaring-rotate-right repeat-ring-active-rings))

(defun repeat-ring-contents (rring)
  "Contents of repeat ring RRING."
  (ring-elements
   (repeat-ring-ring-ring rring)))

(defun repeat-ring-store (rring key-seq)
  "Store KEY-SEQ as an entry in RRING."
  (let ((ring (repeat-ring-ring-ring rring)))
    (when (or (ring-empty-p ring)
              (not (equal key-seq (repeat-ring-last-command rring))))
      ;; don't record successive duplicates
      (ring-insert ring key-seq))))


(provide 'repeat-ring)
;;; repeat-ring.el ends here
