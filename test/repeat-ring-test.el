;; Note: we want to retain dynamic binding for these tests because the
;; ERT "fixtures" rely on it.

;; To run the tests from within Emacs, you must `eval-buffer` this test
;; buffer first. Then, run tests using `ert-run-tests-interactively`.
;; But, to avoid having to evaluate the changes (which may affect the live
;; environment), it may be preferable to `make test` at the shell, instead.

;; Notes:
;; - If you see "lisp nesting exceeds max-lisp-eval-depth"
;;   while running these tests, it could be that you have a duplicate
;;   "body" invocation within one of the nested fixtures. Since these
;;   are dynamically bound, every fixture needs to have a distinct
;;   name for the body argument.
;; - If you see errors like "(void-function t)", "(void-function nil)"
;;   and "invalid function nil . 0"
;;   then you probably are using a fixture without wrapping the body
;;   in a lambda

;; Add source paths to load path so the tests can find the source files
;; Adapted from:
;; https://github.com/Lindydancer/cmake-font-lock/blob/47687b6ccd0e244691fb5907aaba609e5a42d787/test/cmake-font-lock-test-setup.el#L20-L27
(defvar repeat-ring-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".."))
  (add-to-list 'load-path
               (concat repeat-ring-test-setup-directory dir)))

;;

(require 'repeat-ring)

;;
;; Fixtures
;;


;; fixture recipe from:
;; https://www.gnu.org/software/emacs/manual/html_node/ert/Fixtures-and-Test-Suites.html


(defmacro with-fixture (fixture &rest test)
  "Run TEST using FIXTURE."
  (declare (indent 1))
  `(,fixture
    (lambda ()
      ,@test)))

(defun fixture-all-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make (lambda (_key-seq) t)
                                             (lambda (_key-seq) t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

(defun fixture-none-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make (lambda (_key-seq) nil)
                                             (lambda (_key-seq) nil)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

(defvar fixture-single-key [108]
  "A sequence representing a single key press (the letter l).")

(defvar fixture-multi-key [3 102]
  "A sequence representing a multi key event (C-c f).")

(defvar fixture-key-sequence [108 3 102]
  "A sequence representing a composed key sequence (l C-c f).")

(defun fixture-1-ring (body-1)
  (with-fixture fixture-all-ring
    (repeat-ring-ring-set-state rring fixture-single-key)
    (repeat-ring-store rring)
    (funcall body-1)))

(defun fixture-ring-with-state (body-2)
  (with-fixture fixture-all-ring
    (repeat-ring-ring-set-state rring fixture-single-key)
    (funcall body-2)))

(defmacro with-pub-sub (&rest test)
  (declare (indent 0))
  `(lambda ()
     (unwind-protect
         (progn (repeat-ring-subscribe rring)
                ,@test)
       (repeat-ring-unsubscribe rring))))

;;
;; Tests
;;

(ert-deftest repeat-ring-test ()
  ;; null constructor
  (should (vectorp (repeat-ring-make (lambda (_key-seq) t)
                                     (lambda (_key-seq) t))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-all-ring
    (should (ring-p (repeat-ring-ring-ring rring))))

  ;; repeat-ring-ring-record-condition
  (with-fixture fixture-all-ring
    (should (functionp (repeat-ring-ring-record-condition rring))))

  ;; repeat-ring-ring-store-condition
  (with-fixture fixture-all-ring
    (should (functionp (repeat-ring-ring-store-condition rring))))

  (with-fixture fixture-all-ring
    (should (vectorp (repeat-ring-ring-state rring)))))

(ert-deftest repeat-ring-state-test ()
  (with-fixture fixture-all-ring
    (should (seq-empty-p (repeat-ring-ring-state rring))))
  (with-fixture fixture-ring-with-state
    (should-not (seq-empty-p (repeat-ring-ring-state rring))))
  (with-fixture fixture-ring-with-state
    (repeat-ring-ring-append-state rring [1 2 3])
    (should (equal (vconcat fixture-single-key [1 2 3])
                   (repeat-ring-ring-state rring))))
  (with-fixture fixture-ring-with-state
    (repeat-ring-ring-clear-state rring)
    (should (seq-empty-p (repeat-ring-ring-state rring)))))

(ert-deftest repeat-ring-pub-sub-test ()
  (should (with-fixture fixture-all-ring
            (with-pub-sub
              (dynaring-contains-p repeat-ring-active-rings
                                   rring)))))

(ert-deftest repeat-ring-last-command-test ()
  (should (with-fixture fixture-1-ring
            (equal (repeat-ring-last-command rring)
                   fixture-single-key))))

(ert-deftest repeat-ring-recording-in-progress-test ()
  (with-fixture fixture-1-ring
    (should-not (repeat-ring-recording-in-progress-p rring)))
  (with-fixture fixture-ring-with-state
    (should (repeat-ring-recording-in-progress-p rring))))


(ert-deftest repeat-ring-check-record-condition-test ()
  (with-fixture fixture-all-ring
    (should
     (repeat-ring-check-record-condition rring
                                         fixture-single-key)))
  (with-fixture fixture-none-ring
    (should-not
     (repeat-ring-check-record-condition rring
                                         fixture-single-key))))

(ert-deftest repeat-ring-check-store-condition-test ()
  (with-fixture fixture-none-ring
    (should-not
     (repeat-ring-check-store-condition rring
                                        fixture-single-key)))
  (with-fixture fixture-all-ring
    (should-not
     (repeat-ring-check-store-condition rring
                                        fixture-single-key)))
  (with-fixture fixture-ring-with-state
    (should
     (repeat-ring-check-store-condition rring
                                        fixture-single-key))))

(ert-deftest repeat-ring-record-test ()
  (with-fixture fixture-none-ring
    (repeat-ring-record rring
                        fixture-single-key)
    (should-not (repeat-ring-recording-in-progress-p rring)))
  (with-fixture fixture-all-ring
    ;; recording not already in progress but criteria for recording pass
    (repeat-ring-record rring
                        fixture-single-key)
    (should (repeat-ring-recording-in-progress-p rring)))
  (with-fixture fixture-ring-with-state
    ;; recording already in progress AND criteria for recording pass
    (repeat-ring-record rring
                        fixture-single-key)
    (should (repeat-ring-recording-in-progress-p rring)))
  (with-fixture fixture-none-ring
    ;; criteria to start recording fail BUT recording already in progress
    (repeat-ring-ring-set-state rring fixture-single-key)
    (repeat-ring-record rring
                        fixture-single-key)
    (should (repeat-ring-recording-in-progress-p rring))))

(ert-deftest repeat-ring-store-test ()
  (with-fixture fixture-ring-with-state
    ;; stores key sequence
    (repeat-ring-store rring)
    (should (equal fixture-single-key
                   (repeat-ring-last-command rring))))
  (with-fixture fixture-ring-with-state
    ;; clears state
    (repeat-ring-store rring)
    (should-not (repeat-ring-recording-in-progress-p rring)))
  (with-fixture fixture-all-ring
    (should-error (repeat-ring-store rring)))
  (with-fixture fixture-1-ring
    ;; should store if distinct from previous
    (repeat-ring-ring-set-state rring fixture-multi-key)
    (repeat-ring-store rring)
    (should (equal fixture-multi-key
                   (repeat-ring-last-command rring)))
    (should (= 2
               (ring-length
                (repeat-ring-ring-ring rring)))))
  (with-fixture fixture-1-ring
    ;; should not store successive duplicates
    (repeat-ring-ring-set-state rring fixture-single-key)
    (repeat-ring-store rring)
    (should (= 1
               (ring-length
                (repeat-ring-ring-ring rring))))))

(ert-deftest repeat-ring-end-recording-test ()
  (with-fixture fixture-ring-with-state
    (repeat-ring-end-recording rring fixture-single-key)
    (should (equal fixture-single-key
                   (repeat-ring-last-command rring)))))

(ert-deftest repeat-ring-contents-test ()
  (should-not
   (with-fixture fixture-all-ring
     (repeat-ring-contents rring)))
  (should-not
   (with-fixture fixture-ring-with-state
     (repeat-ring-contents rring)))
  (should
   (with-fixture fixture-1-ring
     (repeat-ring-contents rring))))
