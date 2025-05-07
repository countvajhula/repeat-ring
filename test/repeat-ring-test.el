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
        (progn (setq rring (repeat-ring-make (lambda () t)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

(defun fixture-none-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make (lambda () nil)))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

;;
;; Tests
;;

(ert-deftest repeat-ring-test ()
  ;; null constructor
  (should (vectorp (repeat-ring-make (lambda () t))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-all-ring
    (should (ring-p (repeat-ring-ring-ring rring))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-all-ring
    (should (functionp (repeat-ring-ring-criteria rring)))))
