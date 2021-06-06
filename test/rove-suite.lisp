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
(defparameter *large-num* (- (expt 2 1953) 99))
(defparameter *large-num2* (+ (expt 2 1534) (expt 2 98) 87))
(defparameter *cur-ws* (get-word-size))

(deftest validation
  (testing "make-words"
    (let ((valset (list 0 1 2 100 255 256 342901092899200 *large-num*
                        *large-num2*)))
      (loop for val in valset do
        (make-words val))
      (pass "pass")))
  (testing "word-size"
    (pass "get-word-size: pass")
    (set-word-size (get-word-size)) (pass "set-word-size: pass"))
  (testing "Readers and tests"
    (test-arbprec-fun `((0 T) (1 nil) (32 nil) (,*large-num* nil)
                        (,*large-num2* nil)) #'zero-p)
    (test-arbprec-fun `((0 0) (3 2) (4 3) (255 8) (256 9) (,*large-num* 1953)
                        (,*large-num2* 1535))
                      #'length-in-bits))
  (testing "Words simple operations"
    (let ((valset `((1 1 ,(ash 1 *cur-ws*)) (1 -1 0) (0 8 0))))
      (ok (notany
           #'null
           (loop for (num shift res) in valset
                 collect
                 (= res (to-integer (words-ash (make-words num) shift)))))
          "words-ash: ok"))
    (let ((valset `((#(0 1 3) -1 #(0) #(0 1 3))
                    (#(0 1 0 0 3) 3 #(0 1) #(3))
                    (#(0) 0 #(0) #(0))))
          (tmp1) (tmp2))
      (ok (notany
           #'null
           (loop for (words upto-n words1 words2) in valset
                 do (multiple-value-setq (tmp1 tmp2) (split words upto-n))
                 collect
                 (and (equalp tmp1 words1)
                      (equalp tmp2 words2))))
          "split: ok")))
  (testing "Conversions"
    (test-arbprec-fun `((32 32) (0 0) (1 1) (256 256) (,*large-num* ,*large-num*)
                        (,*large-num2* ,*large-num2*))
                      #'to-integer :test #'=)
    (test-arbprec-fun '((0 #*0) (1 #*1) (-1 #*1) (2 #*01) (255 #*11111111)
                        (256 #*000000001))
                      #'to-bitvector :test #'equal))
  (testing "Comparisons"
    (test-arbprec-comp `((less-than 0 1) (same 0 0) (greater-than 90 0)
                         (less-than 450 1290) (same 120 120)
                         (less-than ,*large-num2* ,*large-num*))
                       #'compare-words))
  (testing "Operations"
    (test-arbprec-op `((9 3 6) (1 0 1) (0 0 0) (19 18 1)
                       (,*large-num* ,(- *large-num* 90) 90))
                     #'add-naive-words)
    (test-arbprec-op `((9 18 9) (0 58 58)
                       (0 ,*large-num* ,*large-num*)
                       (,(- *large-num* *large-num2*)
                        ,*large-num* ,*large-num2*))
                     #'subtract-naive-words)
    (test-arbprec-op `((0 492010 0) (0 0 0) (,*large-num* ,*large-num* 1)
                       (,(* *large-num* *large-num2*)
                        ,*large-num* ,*large-num2*))
                     #'mul-words-naive))
  (testing "Optimized multiplications"
    (test-arbprec-op `((0 492010 0) (0 0 0) (,*large-num* ,*large-num* 1)
                       (,(* *large-num* *large-num2*)
                        ,*large-num* ,*large-num2*))
                     #'mul-words-karatsuba)))
