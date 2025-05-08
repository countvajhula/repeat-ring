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

(defconst repeat-ring--index-ring 0
  "The index of the underlying ring in a repeat ring.")

(defconst repeat-ring--index-record-condition 1
  "The index of the condition for recording in a repeat ring.")

(defconst repeat-ring--index-store-condition 2
  "The index of the condition for storage in a repeat ring.")

(defconst repeat-ring--index-state 3
  "The index of the state variable in a repeat ring.")

(defun repeat-ring-ring-ring (rring)
  "Get the underlying ring in RRING."
  (seq-elt rring repeat-ring--index-ring))

(defun repeat-ring-ring-record-condition (rring)
  "Get condition to start recording for RRING."
  (seq-elt rring repeat-ring--index-record-condition))

(defun repeat-ring-ring-store-condition (rring)
  "Get condition to store a recording in RRING."
  (seq-elt rring repeat-ring--index-store-condition))

(defun repeat-ring-ring-state (rring)
  "Get accumulated state in RRING."
  (seq-elt rring repeat-ring--index-state))

(defun repeat-ring-ring-set-state (rring new-state)
  "Set state on RRING to NEW-STATE."
  (aset rring repeat-ring--index-state new-state))

(defun repeat-ring-ring-append-state (rring new-state)
  "Append NEW-STATE to RRING's existing state."
  (repeat-ring-ring-set-state rring
                              (vconcat (repeat-ring-ring-state rring)
                                       new-state)))

(defun repeat-ring-ring-clear-state (rring)
  "Clear the RRING's state."
  (repeat-ring-ring-set-state rring (vector)))

(defun repeat-ring-make (record-condition store-condition)
  "Make a repeat ring.

A repeat ring is an ordinary fixed-size ring together with criteria
for storing key sequences in it in the form of a RECORD-CONDITION to
determine the start of a repeatable key sequence, and a STORE-CONDITION
to determine the end.

The RECORD-CONDITION is checked in `pre-command-hook' and STORE-CONDITION
is checked in `post-command-hook'. Once RECORD-CONDITION is satisfied,
the key sequences are accumulated in STATE as a composed key sequence
(vector). When STORE-CONDITION is satisfied (which might happen during
invocation of the same command, or it might not), the entire sequence
in STATE is stored in the ring as a single key sequence vector."
  (let ((ring (make-ring repeat-ring-size)))
    (vector ring
            record-condition
            store-condition
            (vector))))

(defun repeat-ring-subscribe (rring)
  "Add the repeat ring RRING as a subscriber to key events."
  (dynaring-insert repeat-ring-active-rings
                   rring))

(defun repeat-ring-unsubscribe (rring)
  "Unsubscribe the repeat ring RRING from key events."
  (dynaring-delete repeat-ring-active-rings
                   rring))

(defvar repeat-ring-recent-keys
  (repeat-ring-make (lambda (_key-seq) t)
                    (lambda (_key-seq) t))
  "A ring to store all recent key sequences.")

(defun repeat-ring-initialize ()
  "Initialize repeat ring.

This adds key sequence publishing to Emacs's `pre-command-hook` so
that active repeat rings can be notified of them, and adds an initial
basic repeat ring that stores all key sequences."
  (add-hook 'pre-command-hook #'repeat-ring-notify-start)
  (add-hook 'post-command-hook #'repeat-ring-notify-end)
  (repeat-ring-subscribe repeat-ring-recent-keys))

(defun repeat-ring-last-command (rring)
  "The last command stored on the repeat ring RRING."
  (let ((ring (repeat-ring-ring-ring rring)))
    (ring-ref ring 0)))

(defun repeat-ring-recording-in-progress-p (rring)
  "Whether RRING is already recording, i.e., accumulating state."
  (let ((state (repeat-ring-ring-state rring)))
    (not (seq-empty-p state))))

(defun repeat-ring-check-record-condition (rring key-seq)
  "Check whether KEY-SEQ matches the criteria for storage in RRING."
  (let ((criteria (repeat-ring-ring-record-condition rring)))
    (or (repeat-ring-recording-in-progress-p rring)
        (funcall criteria key-seq))))

(defun repeat-ring-check-store-condition (rring key-seq)
  "Check whether KEY-SEQ matches the criteria for storage in RRING."
  (let ((criteria (repeat-ring-ring-store-condition rring)))
    (and (repeat-ring-recording-in-progress-p rring)
         ;; TODO: should also pass state?
         ;; note, state already includes key-seq
         (funcall criteria key-seq))))

(defun repeat-ring-record (rring key-seq)
  "Record KEY-SEQ by appending it to RRING's current state.

Only if either the record-condition on KEY-SEQ is met, or if recording is
already in progress."
  (when (repeat-ring-check-record-condition rring key-seq)
    (repeat-ring-ring-append-state rring key-seq)))

(defun repeat-ring-store (rring)
  "Store the current state as an entry in RRING.

This also clears the state."
  (let ((ring (repeat-ring-ring-ring rring))
        (state (repeat-ring-ring-state rring)))
    (when (seq-empty-p state)
      (error "Can't store empty key sequence!"))
    (when (or (ring-empty-p ring)
              (not (equal state (repeat-ring-last-command rring))))
      ;; don't record successive duplicates
      (ring-insert ring state))
    ;; clear state in any case
    (repeat-ring-ring-clear-state rring)))

(defun repeat-ring-end-recording (rring key-seq)
  "Store KEY-SEQ in RRING if it meets the criteria."
  ;; state already includes key-seq by this point,
  ;; as this is being called in post-command
  (when (repeat-ring-check-store-condition rring key-seq)
    (repeat-ring-store rring)))

(defun repeat-ring-notify-start ()
  "Notify all repeat rings of a newly entered key sequence.

Each repeat ring has *conditions* under which a key sequence will be
considered for storage, and *patterns* that such sequences must match
in order to be stored."
  (let ((this-key-sequence (this-command-keys-vector)))
    (when (and this-key-sequence (not (seq-empty-p this-key-sequence)))
      (dolist (rring (dynaring-values repeat-ring-active-rings))
        (repeat-ring-record rring this-key-sequence)))))

(defun repeat-ring-notify-end ()
  "Notify all repeat rings after the conclusion of a command for a key sequence.

Each repeat ring has *conditions* under which a key sequence will be
considered for storage, and *patterns* that such sequences must match
in order to be stored."
  (let ((this-key-sequence (this-command-keys-vector)))
    (when (and this-key-sequence (not (seq-empty-p this-key-sequence)))
      (dolist (rring (dynaring-values repeat-ring-active-rings))
        (repeat-ring-end-recording rring this-key-sequence)))))

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


(provide 'repeat-ring)
;;; repeat-ring.el ends here
