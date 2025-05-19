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
(defconst fixture-test-element-2 "efgh")
(defconst fixture-test-element-3 "ijkl")

(defun fixture-0-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make fixture-test-ring-name #'identity))
               (funcall body))
      ;; perhaps aid garbage collection
      (setq rring nil))))

(defun fixture-1-ring (body-1)
  (with-fixture fixture-0-ring
    (repeat-ring-store rring fixture-test-element)
    (funcall body-1)))

(defun fixture-2-ring (body-2)
  (with-fixture fixture-1-ring
    (repeat-ring-store rring fixture-test-element-2)
    (funcall body-2)))

(defun fixture-3-ring (body-3)
  (with-fixture fixture-2-ring
    (repeat-ring-store rring fixture-test-element-3)
    (funcall body-3)))

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
  (should (vectorp (repeat-ring-make fixture-test-ring-name #'identity)))
  (should (vectorp (repeat-ring-make fixture-test-ring-name #'identity 10)))
  (should (= 10 (ring-size
                 (repeat-ring-ring-ring
                  (repeat-ring-make fixture-test-ring-name
                                    #'identity
                                    10)))))
  (should (= 0 (repeat-ring-ring-head
                (repeat-ring-make fixture-test-ring-name #'identity))))

  ;; repeat-ring-ring-name
  (with-fixture fixture-0-ring
    (should (equal fixture-test-ring-name (repeat-ring-ring-name rring))))

  ;; repeat-ring-ring-ring
  (with-fixture fixture-0-ring
    (should (ring-p (repeat-ring-ring-ring rring))))

  ;; repeat-ring-ring-head
  (with-fixture fixture-0-ring
    (should (= 0 (repeat-ring-ring-head rring))))

  ;; repeat-ring-ring-set-head
  (with-fixture fixture-0-ring
    (repeat-ring-ring-set-head rring 1)
    (should (= 1 (repeat-ring-ring-head rring))))

  ;; repeat-ring-ring-reset-head
  (with-fixture fixture-0-ring
    (repeat-ring-ring-set-head rring 1)
    (repeat-ring-ring-reset-head rring)
    (should (= 0 (repeat-ring-ring-head rring)))))

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

(ert-deftest repeat-ring-current-command-test ()
  (with-fixture fixture-3-ring
    (repeat-ring-ring-set-head rring 1)
    (should (equal (repeat-ring-current-command rring)
                   fixture-test-element-2))))

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
                (repeat-ring-ring-ring rring)))))
  (with-fixture fixture-3-ring
    ;; should reset head upon storing new element
    (repeat-ring-ring-set-head rring 1)
    (repeat-ring-store rring fixture-test-element)
    (should (= 0 (repeat-ring-ring-head rring))))
  (with-fixture fixture-3-ring
    ;; should reset head upon storing new element
    ;; even if the element isn't actually stored
    (repeat-ring-ring-set-head rring 1)
    (repeat-ring-store rring fixture-test-element-3)
    (should (= 0 (repeat-ring-ring-head rring))))
  (with-fixture fixture-3-ring
    ;; should store elements in order of recency
    (repeat-ring-store rring fixture-test-element)
    (should (equal (list fixture-test-element ; most recently added
                         fixture-test-element-3
                         fixture-test-element-2
                         fixture-test-element)
                   (repeat-ring-contents rring))))
  (with-fixture fixture-3-ring
    ;; should store new element at underlying ring head
    ;; (i.e., in order of most recently stored)
    ;; even if surface ring "virtual" head is different
    (repeat-ring-ring-set-head rring 1)
    (repeat-ring-store rring fixture-test-element)
    (should (equal (list fixture-test-element ; most recently added
                         fixture-test-element-3
                         fixture-test-element-2
                         fixture-test-element)
                   (repeat-ring-contents rring)))))

(ert-deftest repeat-ring-contents-test ()
  (with-fixture fixture-0-ring
    (should-not (repeat-ring-contents rring)))
  (with-fixture fixture-1-ring
    (should (repeat-ring-contents rring))))

(ert-deftest repeat-ring-rotate-test ()
  (with-fixture fixture-3-ring
    (repeat-ring-rotate-forwards rring)
    (should (equal fixture-test-element
                   (repeat-ring-current-command rring))))
  (with-fixture fixture-3-ring
    (repeat-ring-rotate-backwards rring)
    (should (equal fixture-test-element-2
                   (repeat-ring-current-command rring)))))
