;; Initial Loads
(begin
  ;; Chez-specific code to get better debug info.
  (eval-when (eval load)
    (optimize-level 0)
    (debug-level 3))

  (load "./all.scm")
  (load "./test-check.scm"))

(printf "vicare-tests\n")
(load "./test-vicare.scm")

(printf "test-basics")
(load "./test-basics.scm")

(printf "==-tests\n")
(load "==-tests.scm")

(printf "symbolo-tests\n")
(load "symbolo-tests.scm")

(printf "numbero-tests\n")
(load "numbero-tests.scm")

(printf "symbolo-numbero-tests\n")
(load "symbolo-numbero-tests.scm")

(printf "stringo-tests.scm\n")
(load "stringo-tests.scm")

(printf "disequality-tests\n")
(load "disequality-tests.scm")

(printf "absento-closure-tests\n")
(load "absento-closure-tests.scm")

(printf "absento-tests\n")
(load "absento-tests.scm")

(printf "test-infer\n")
(load "test-infer.scm")

(printf "test-simple-interp\n")
(load "simple-interp.scm")
(load "test-simple-interp.scm")

(printf "test-quines\n")
(load "test-quines.scm")

(printf "test-numbers\n")
(load "numbers.scm")
(load "test-numbers.scm")

(load "full-interp.scm")

(printf "test-sets\n")
(load "test-sets.scm")

(printf "occurs-free\n")
(load "./occurs-free.scm")

(printf "natural-deduction\n")
(load "./natural-deduction.scm")

(printf "scheme-interp-with-set-support\n")
(load "./scheme-interp-with-set-support.scm")

(printf "full-interp-with-set-support\n")
(load "./full-interp-with-set-support.scm")

(printf "tabling-with-sets\n")
(load "./tabling-with-sets.scm")

(printf "seto-listo-tests.scm\n")
(load "./seto-listo-tests.scm")

(printf "test-alist.scm\n")
(load "./test-alist.scm")

;; Test Indicator
(begin
  (if test-failed
      (display "Test Failed!")
      (display "Tests Passed!"))
  (newline))
