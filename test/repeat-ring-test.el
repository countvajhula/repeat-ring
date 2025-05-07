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
        (progn (setq rring (repeat-ring-make (lambda (_key-seq) t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

(defun fixture-none-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make (lambda (_key-seq) nil)))
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
    (repeat-ring-store rring fixture-single-key)
    (funcall body-1)))

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
  (should (vectorp (repeat-ring-make (lambda (_key-seq) t))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-all-ring
    (should (ring-p (repeat-ring-ring-ring rring))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-all-ring
    (should (functionp (repeat-ring-ring-criteria rring)))))

(ert-deftest repeat-ring-pub-sub-test ()
  (should (with-fixture fixture-all-ring
            (with-pub-sub
              (dynaring-contains-p repeat-ring-active-rings
                                   rring)))))

(ert-deftest repeat-ring-last-command-test ()
  (should (with-fixture fixture-1-ring
            (equal (repeat-ring-last-command rring)
                   fixture-single-key))))

(ert-deftest repeat-ring-check-criteria-test ()
  (should (with-fixture fixture-all-ring
            (repeat-ring-check-criteria rring
                                        fixture-single-key)))
  (should-not (with-fixture fixture-none-ring
                (repeat-ring-check-criteria rring
                                            fixture-single-key))))

(ert-deftest repeat-ring-store-test ()
  (should
   (with-fixture fixture-all-ring
     (repeat-ring-store rring
                        fixture-single-key)))
  (should-not
   (with-fixture fixture-none-ring
     (repeat-ring-store rring
                        fixture-single-key)))
  (should
   ;; should store if distinct from previous
   (with-fixture fixture-1-ring
     (repeat-ring-store rring
                        fixture-multi-key)))
  (should-not
   ;; should not store successive duplicates
   (with-fixture fixture-1-ring
     (repeat-ring-store rring
                        fixture-single-key))))

(ert-deftest repeat-ring-contents-test ()
  (should-not
   (with-fixture fixture-all-ring
     (repeat-ring-contents rring)))
  (should
   (with-fixture fixture-1-ring
     (repeat-ring-contents rring))))
