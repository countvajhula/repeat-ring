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

(defconst fixture-ring-name "test")

(defconst fixture-ring-2-name "test2")

(defconst fixture-test-element "abcd")
(defconst fixture-test-element-2 "efgh")
(defconst fixture-test-element-3 "ijkl")

(defconst fixture-test-topic-name "dummy-topic")

(defun fixture-0-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make fixture-ring-name))
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

;;
;; Tests
;;

(ert-deftest repeat-ring-test ()
  ;; null constructor
  (should (vectorp (repeat-ring-make fixture-ring-name)))
  (should (vectorp (repeat-ring-make fixture-ring-name 10)))
  (should (= 10 (ring-size
                 (virtual-ring-ring
                  (repeat-ring-ring
                   (repeat-ring-make fixture-ring-name 10))))))

  ;; repeat-ring-ring
  (with-fixture fixture-0-ring
    (should (ring-p
             (virtual-ring-ring
              (repeat-ring-ring rring)))))

  ;; repeat-ring-repeating
  (with-fixture fixture-0-ring
    (should-not (repeat-ring-repeating rring)))

  ;; repeat-ring-set-repeating
  (with-fixture fixture-0-ring
    (repeat-ring-set-repeating rring fixture-test-element)
    (should (equal fixture-test-element
                   (repeat-ring-repeating rring))))

  ;; repeat-ring-clear-repeating
  (with-fixture fixture-0-ring
    (repeat-ring-set-repeating rring fixture-test-element)
    (repeat-ring-clear-repeating rring)
    (should-not (repeat-ring-repeating rring))))

(ert-deftest repeat-ring-subscribe-test ()
  (with-fixture fixture-0-ring
    (unwind-protect
        (progn (repeat-ring-subscribe rring fixture-test-topic-name)
               (pubsub-publish fixture-test-topic-name fixture-test-element)
               (should (ring-member
                        (virtual-ring-ring
                         (repeat-ring-ring rring))
                        fixture-test-element)))
      (repeat-ring-unsubscribe rring fixture-test-topic-name)
      (pop (gethash fixture-test-topic-name
                    pubsub-board)))))

(ert-deftest repeat-ring-repeat ()
  (with-fixture fixture-0-ring
    (repeat-ring-store rring [])
    (repeat-ring-repeat rring)
    (should-not (repeat-ring-repeating rring))))

(ert-deftest repeat-ring-repeat-pop ()
  ;; TODO: if already virtually rotated, then removes true head
  ;;       and not otherwise
  ;; TODO: rotates backwards, so head should be incremented
  ;; (with-fixture fixture-0-ring
  ;;   (repeat-ring-store rring [])
  ;;   (insert "hi")
  ;;   (repeat-ring-repeat-pop rring) ; currently: user error, no undo information found
  ;;   (should-not (repeat-ring-repeating rring)))
  )

(ert-deftest repeat-ring-store-test ()
  (with-fixture fixture-0-ring
    ;; stores any key sequence if ring is empty
    (repeat-ring-store rring fixture-test-element)
    (should (equal fixture-test-element
                   (virtual-ring-last-entry
                    (repeat-ring-ring rring)))))
  (with-fixture fixture-1-ring
    ;; should store if distinct from previous
    (let ((new-element "new-element"))
      (repeat-ring-store rring new-element)
      (should (equal new-element
                     (virtual-ring-last-entry
                      (repeat-ring-ring rring))))
      (should (= 2
                 (ring-length
                  (virtual-ring-ring
                   (repeat-ring-ring rring)))))))
  (with-fixture fixture-1-ring
    ;; should not store successive duplicates
    (repeat-ring-store rring fixture-test-element)
    (should (= 1
               (ring-length
                (virtual-ring-ring
                 (repeat-ring-ring rring))))))
  (with-fixture fixture-3-ring
    ;; should reset head upon storing new element
    (virtual-ring-set-head (repeat-ring-ring rring) 1)
    (repeat-ring-store rring fixture-test-element)
    (should (= 0 (virtual-ring-head (repeat-ring-ring rring)))))
  (with-fixture fixture-3-ring
    ;; should preserve head if repeating
    (virtual-ring-set-head (repeat-ring-ring rring) 1)
    (repeat-ring-set-repeating rring fixture-test-element)
    (repeat-ring-store rring fixture-test-element)
    (should (= 1 (virtual-ring-head (repeat-ring-ring rring)))))
  (with-fixture fixture-3-ring
    ;; should record fresh repetition if repeating
    (virtual-ring-set-head (repeat-ring-ring rring) 1)
    (repeat-ring-set-repeating rring fixture-test-element)
    (repeat-ring-store rring fixture-test-element)
    (should (equal fixture-test-element
                   (ring-ref
                    (virtual-ring-ring
                     (repeat-ring-ring rring))
                    0))))
  (with-fixture fixture-3-ring
    ;; should store elements in order of recency
    (repeat-ring-store rring fixture-test-element)
    (should (equal (list fixture-test-element ; most recently added
                         fixture-test-element-3
                         fixture-test-element-2
                         fixture-test-element)
                   (virtual-ring-contents (repeat-ring-ring rring)))))
  (with-fixture fixture-3-ring
    ;; should store new element at underlying ring head
    ;; (i.e., in order of most recently stored)
    ;; even if surface ring "virtual" head is different
    (virtual-ring-set-head (repeat-ring-ring rring) 1)
    (repeat-ring-store rring fixture-test-element)
    (should (equal (list fixture-test-element ; most recently added
                         fixture-test-element-3
                         fixture-test-element-2
                         fixture-test-element)
                   (virtual-ring-contents
                    (repeat-ring-ring rring))))))
