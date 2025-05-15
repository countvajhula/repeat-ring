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
(require 'pubsub)

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

(defconst fixture-test-ring-name "test")

(defconst fixture-test-element "abcd")

(defun fixture-0-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make fixture-test-ring-name))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

(defun fixture-1-ring (body-1)
  (with-fixture fixture-0-ring
    (repeat-ring-store rring fixture-test-element)
    (funcall body-1)))

(defmacro with-pub-sub (&rest test)
  (declare (indent 0))
  `(lambda ()
     (unwind-protect
         (progn (pubsub-subscribe (repeat-ring-ring-name rring)
                                  (apply-partially #'repeat-ring-store
                                                   rring))
                ,@test)
       (pop (gethash (repeat-ring-ring-name rring)
                     pubsub-board)))))

;;
;; Tests
;;

(ert-deftest repeat-ring-test ()
  ;; null constructor
  (should (vectorp (repeat-ring-make fixture-test-ring-name)))
  (should (vectorp (repeat-ring-make fixture-test-ring-name 10)))
  (should (= 10 (ring-size
                 (repeat-ring-ring-ring
                  (repeat-ring-make fixture-test-ring-name
                                    10)))))

  ;; repeat-ring-ring-name
  (with-fixture fixture-0-ring
    (should (equal fixture-test-ring-name (repeat-ring-ring-name rring))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-0-ring
    (should (ring-p (repeat-ring-ring-ring rring)))))

(ert-deftest repeat-ring-pub-sub-test ()
  (with-fixture fixture-0-ring
    (with-pub-sub
     (pubsub-publish (repeat-ring-ring-name rring) fixture-test-element)
     (should (ring-member (repeat-ring-ring-ring rring)
                          fixture-test-element)))))

(ert-deftest repeat-ring-last-command-test ()
  (with-fixture fixture-1-ring
    (should (equal (repeat-ring-last-command rring)
                   fixture-test-element))))

(ert-deftest repeat-ring-store-test ()
  (with-fixture fixture-0-ring
    ;; stores any key sequence if ring is empty
    (repeat-ring-store rring fixture-test-element)
    (should (equal fixture-test-element
                   (repeat-ring-last-command rring))))
  (with-fixture fixture-1-ring
    ;; should store if distinct from previous
    (let ((new-element "new-element"))
      (repeat-ring-store rring new-element)
      (should (equal new-element
                     (repeat-ring-last-command rring)))
      (should (= 2
                 (ring-length
                  (repeat-ring-ring-ring rring))))))
  (with-fixture fixture-1-ring
    ;; should not store successive duplicates
    (repeat-ring-store rring fixture-test-element)
    (should (= 1
               (ring-length
                (repeat-ring-ring-ring rring))))))

(ert-deftest repeat-ring-contents-test ()
  (with-fixture fixture-0-ring
    (should-not (repeat-ring-contents rring)))
  (with-fixture fixture-1-ring
    (should (repeat-ring-contents rring))))
