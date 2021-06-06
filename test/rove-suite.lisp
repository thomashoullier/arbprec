(defpackage :arbprec/test (:use :cl :arbprec :rove))
(in-package :arbprec/test)

;;; Helper functions.
(defun test-arbprec-fun (valset fun
                         &key (res-to-integer nil) (test #'eq))
  "Test a arbprec fun over a valset. valset is ((num1 res1) (num2 res2) etc.).
   Every (eq res (fun (make-words num))) must be true."
  (ok (notany
       #'null
       (loop
         for (num res) in valset
         collect (funcall test res
                          (if res-to-integer
                              (to-integer (funcall fun (make-words num)))
                              (funcall fun (make-words num))))))
      (format nil "~A: ok" fun)))

(defun test-arbprec-comp (valset fun)
  "Test a arbprec comparison.
   valset is a list with elements (result arg1 arg2 arg3). Arguments are
   fed to #'fun in order."
  (ok (notany
       #'null
       (loop for val in valset
             collect (eq (car val)
                         (apply fun (map 'list #'make-words (cdr val))))))
      (format nil "~A: ok" fun)))

(defun test-arbprec-op (valset fun)
  "Test a arbprec operation.
   valset is a list with elements (result arg1 arg2 arg3)."
  (ok (notany
       #'null
       (loop for val in valset
             collect (= (car val)
                        (to-integer
                         (apply fun (map 'list #'make-words (cdr val)))))))
      (format nil "~A: ok" fun)))

;;; Tests.
(deftest validation
  (testing "make-words"
    (let ((valset (list 0 1 2 100 255 256 342901092899200)))
      (loop for val in valset do
        (make-words val))
      (pass "pass")))
  ;; (testing "Readers and tests"
  ;;          (test-bigint-fun '((0 T) (1 nil) (-1 nil) (32 nil))
  ;;                           #'zero-p)
  ;;          (test-bigint-fun '((45 +1) (0 +1) (-3 -1) (1201290239219 +1))
  ;;                           #'sign-of)
  ;;          (test-bigint-fun '((0 0) (3 2) (-4 3) (255 8) (-256 9))
  ;;                           #'length-in-bits))
  ;; (testing "Simple operations"
  ;;          (test-bigint-fun '((0 0) (1 1) (-1 1) (43 43) (-258 258))
  ;;                           #'abso :res-to-integer T)
  ;;          (test-bigint-fun '((30 -30) (12 -12) (0 0) (-9 9) (256 -256))
  ;;                           #'neg :res-to-integer T))
  ;; (testing "Conversions"
  ;;          (test-bigint-fun '((32 32) (-3 -3) (0 0) (1 1) (256 256))
  ;;                           #'to-integer)
  ;;          (test-bigint-fun '((0 #*0) (1 #*1) (-1 #*1) (2 #*01) (255 #*11111111)
  ;;                             (256 #*000000001))
  ;;                           #'to-bitvector :test #'equal))
  ;; (testing "Comparisons"
  ;;          (test-bigint-comp '((T 1 3) (T 0 1) (nil 0 -1) (T -4 258) (nil -534 -1890)
  ;;                              (T 0 1 2 3) (T -8000 -30 0 40 5000) (nil 0 0)
  ;;                              (nil 89 89) (nil -78 -78))
  ;;                            #'lt)
  ;;          (test-bigint-comp '((T 3 0) (nil 0 3) (nil -20000 1) (T 4500 0)
  ;;                              (T 3 2 1) (T 4000 0 -1 -8000)
  ;;                              (T -92901229 -10291182093))
  ;;                            #'gt)
  ;;          (test-bigint-comp '((T 1 3) (T 0 1) (T 0 0) (nil -8 -90) (T 9000 445000)
  ;;                              (nil 9000229 -123120398) (T 0 0 0 0)
  ;;                              (T -4903 -89 0 1 50))
  ;;                            #'leq)
  ;;          (test-bigint-comp '((T 0 0 0 0) (T 45 3 0 -1 -20202) (nil 590 8029292)
  ;;                              (nil 4 2 1 292920))
  ;;                            #'geq)
  ;;          (test-bigint-comp '((T 0 0 0 0) (T 0 0) (T 1 1) (T 89 89 89)
  ;;                              (T -92910 -92910) (nil -87289 87289)
  ;;                              (nil -1 1) (nil -256 256) (T -255 -255)
  ;;                              (T 255 255) (T 256 256) (T 45 45 45 45 45))
  ;;                            #'b=))
  ;; (testing "Operations"
  ;;          (test-bigint-op '((9 3 6) (0 0 0) (0 0) (0 0 0 0) (1 0 1) (1 0 -4 3 2)
  ;;                            (-80 -79 0 0 -1) (0 -90 90 8 9 -8 -9) (1 1) (0 0)
  ;;                            (-256 -256))
  ;;                          #'add)
  ;;          (test-bigint-op '((0 0) (-8 8) (0 0 0 0) (-9 9 18)
  ;;                            (256 255 -1 0) (-90 -45 45) (-256 -255 1))
  ;;                          #'sub)
  ;;          (test-bigint-op '((0 0) (59 59) (-289 289 -1) (-458 229 -2 1)
  ;;                            (1 -1 -1 -1 -1) (0 -492 0) (1 1 1))
  ;;                          #'mul))
  )
