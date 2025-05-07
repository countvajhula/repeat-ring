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

(defconst repeat-ring-size 20)

(defvar repeat-ring-active-rings
  (dynaring-make)
  "Repeat rings that are actively recording commands.

This is a dynamically sized ring.")

(defun repeat-ring-ring-ring (rring)
  "Get the underlying ring in RRING."
  (seq-elt rring 0))

(defun repeat-ring-ring-criteria (rring)
  "Get storage criteria for RRING."
  (seq-elt rring 1))

(defun repeat-ring-make (criteria)
  "Make a repeat ring with CRITERIA.

A repeat ring is an ordinary fixed-size ring together with criteria
for storing key sequences in it."
  (let ((ring (make-ring repeat-ring-size)))
    (vector ring
            criteria)))

(defun repeat-ring-subscribe (rring)
  "Add the repeat ring RRING as a subscriber to key events."
  (dynaring-insert repeat-ring-active-rings
                   rring))

(defun repeat-ring-unsubscribe (rring)
  "Unsubscribe the repeat ring RRING from key events."
  (dynaring-delete repeat-ring-active-rings
                   rring))

(defvar repeat-ring-recent-keys
  (repeat-ring-make (lambda (_key-seq) t))
  "A ring to store all recent key sequences.")

(defun repeat-ring-initialize ()
  "Initialize repeat ring.

This adds key sequence publishing to Emacs's `post-command-hook` so
that active repeat rings can be notified of them, and adds an initial
basic repeat ring that stores all key sequences."
  (add-hook 'post-command-hook #'repeat-ring-notify)
  (repeat-ring-subscribe repeat-ring-recent-keys))

(defun repeat-ring-last-command (rring)
  "The last command stored on the repeat ring RRING."
  (let ((ring (repeat-ring-ring-ring rring)))
    (ring-ref ring 0)))

(defun repeat-ring-check-criteria (rring key-seq)
  "Check whether KEY-SEQ matches the criteria for storage in RRING."
  (let ((criteria (repeat-ring-ring-criteria rring)))
    (funcall criteria key-seq)))

(defun repeat-ring-store (rring key-seq)
  "Store KEY-SEQ in RRING if it meets the criteria."
  (let ((ring (repeat-ring-ring-ring rring)))
    (if (ring-empty-p ring)
        (when (repeat-ring-check-criteria rring key-seq)
          (ring-insert ring key-seq))
      (unless (equal key-seq (repeat-ring-last-command rring))
        ;; don't record successive duplicates
        (when (repeat-ring-check-criteria rring key-seq)
          (ring-insert ring key-seq))))))

(defun repeat-ring-notify ()
  "Notify all repeat rings of each key sequence entered.

Each repeat ring has *conditions* under which a key sequence will be
considered for storage, and *patterns* that such sequences must match
in order to be stored."
  (let ((this-key-sequence (this-command-keys-vector)))
    (when (and this-key-sequence (not (seq-empty-p this-key-sequence)))
      (dolist (rring (dynaring-values repeat-ring-active-rings))
        (repeat-ring-store rring this-key-sequence)))))

(defun repeat-ring-repeat ()
  "Repeat the last command on the most recently used repeat ring."
  (interactive)
  (execute-kbd-macro
   (repeat-ring-last-command
    (dynaring-value repeat-ring-active-rings))))

(defun repeat-ring-rotate-ring-forwards ()
  "Rotate the ring of repeat rings forwards."
  (interactive)
  (dynaring-rotate-left repeat-ring-active-rings))

(defun repeat-ring-rotate-ring-forwards ()
  "Rotate the ring of repeat rings forwards."
  (interactive)
  (dynaring-rotate-right repeat-ring-active-rings))

(defun repeat-ring-contents (rring)
  "Contents of repeat ring RRING."
  (ring-elements
   (repeat-ring-ring-ring rring)))


(provide 'repeat-ring)
;;; repeat-ring.el ends here
