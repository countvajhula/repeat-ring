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

(defconst fixture-test-element "abcd")
(defconst fixture-test-element-2 "efgh")
(defconst fixture-test-element-3 "ijkl")

(defconst fixture-test-topic-name "dummy-topic")

(defconst fixture-all-topic-name "mantra-all-key-sequences")

(defun fixture-0-ring (body)
  (let ((rring nil))
    (unwind-protect
        (progn (setq rring (repeat-ring-make))
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
         (progn (repeat-ring-subscribe rring
                                       fixture-test-topic-name)
                ,@test)
       (pop (gethash fixture-test-topic-name
                     pubsub-board)))))
(defun fixture-two-rings (body-4)
  (let ((rring1 nil)
        (rring2 nil))
    (unwind-protect
        (progn (setq rring1 (repeat-ring-make))
               (setq rring2 (repeat-ring-make))
               (repeat-ring-subscribe rring1)
               (repeat-ring-subscribe rring2)
               (repeat-ring-store rring1 fixture-test-element)
               (repeat-ring-store rring2 fixture-test-element)
               (funcall body-4))
      (repeat-ring-unsubscribe rring1)
      (repeat-ring-unsubscribe rring2))))

;;
;; Tests
;;

(ert-deftest repeat-ring-test ()
  ;; null constructor
  (should (vectorp (repeat-ring-make)))
  (should (vectorp (repeat-ring-make 10)))
  (should (= 10 (ring-size
                 (virtual-ring-ring
                  (repeat-ring-ring
                   (repeat-ring-make 10))))))

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


(ert-deftest repeat-ring-pub-sub-test ()
  (with-fixture fixture-0-ring
    (with-pub-sub
     (pubsub-publish fixture-test-topic-name fixture-test-element)
     (should (ring-member
              (virtual-ring-ring
               (repeat-ring-ring rring))
              fixture-test-element)))))
(ert-deftest repeat-ring-current-ring-test ()
  (with-fixture fixture-two-rings
    (repeat-ring-repeat-for-ring rring1)
    (should (equal rring1 (repeat-ring-current-ring))))
  (with-fixture fixture-two-rings
    (repeat-ring-repeat-for-ring rring2)
    (should (equal rring2 (repeat-ring-current-ring)))))

(ert-deftest repeat-ring-subscribe-test ()
  (with-fixture fixture-0-ring
    (unwind-protect
        (progn (repeat-ring-subscribe rring fixture-test-topic-name)
               (pubsub-publish fixture-test-topic-name fixture-test-element)
               (should (ring-member
                        (virtual-ring-ring
                         (repeat-ring-ring rring))
                        fixture-test-element))))
    (pop (gethash fixture-test-topic-name
                  pubsub-board)))
  (with-fixture fixture-0-ring
    (unwind-protect
        (progn (repeat-ring-subscribe rring nil)
               (pubsub-publish fixture-test-topic-name fixture-test-element)
               ;; doesn't really test much since we aren't subscribing to any
               ;; topic here, and using a particular one doesn't validate
               ;; that it isn't subscribed at all
               (should-not (ring-member
                            (virtual-ring-ring
                             (repeat-ring-ring rring))
                            fixture-test-element))))
    (pop (gethash fixture-test-topic-name
                  pubsub-board)))
  ;; "all" subscribes to all key sequences
  (with-fixture fixture-0-ring
    (unwind-protect
        (progn (repeat-ring-subscribe rring 'all)
               (pubsub-publish fixture-all-topic-name fixture-test-element)
               (should (ring-member
                        (virtual-ring-ring
                         (repeat-ring-ring rring))
                        fixture-test-element))))
    (pop (gethash fixture-all-topic-name
                  pubsub-board))))

(ert-deftest repeat-ring-repeat-for-ring ()
  (with-fixture fixture-0-ring
    (repeat-ring-store rring [])
    (repeat-ring-repeat-for-ring rring)
    (should (repeat-ring-repeating rring))))

(ert-deftest repeat-ring-repeat-pop-for-ring ()
  ;; TODO: if already virtually rotated, then removes true head
  ;;       and not otherwise
  ;; TODO: rotates backwards, so head should be incremented
  (with-fixture fixture-0-ring
    (repeat-ring-store rring [])
    (repeat-ring-repeat-for-ring rring)
    (should (repeat-ring-repeating rring))))

(ert-deftest repeat-ring-rotate-test ()
  (with-fixture fixture-3-ring
    (repeat-ring-rotate-forwards rring)
    (should (equal fixture-test-element
                   (virtual-ring-current-entry
                    (repeat-ring-ring rring)))))
  (with-fixture fixture-3-ring
    (repeat-ring-rotate-backwards rring)
    (should (equal fixture-test-element-2
                   (virtual-ring-current-entry
                    (repeat-ring-ring rring))))))

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
                    (repeat-ring-ring rring)))))
  (with-fixture fixture-1-ring
    ;; should clear repeating flag
    (repeat-ring-set-repeating rring fixture-test-element)
    (repeat-ring-store rring fixture-test-element)
    (should-not (repeat-ring-repeating rring))))
