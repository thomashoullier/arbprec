(defpackage :arbprec
  (:use :cl)
  (:export
   #:make-words
   #:get-word-size
   #:set-word-size
   ;; Readers and tests
   #:zero-p
   #:length-in-bits
   ;; Words simple operations
   #:words-ash
   #:split
   ;; Conversions
   #:to-integer
   #:to-bitvector
   ;; Comparisons
   #:compare-words
   #:greater-than
   #:less-than
   #:same
   ;; Operations
   #:add-naive-words
   #:subtract-naive-words
   #:mul-words-naive
   ;; Optimized multiplications
   #:mul-words-karatsuba))

(in-package :arbprec)

;; the size in bits of the words used for representing bigints
(defvar *word-size* 32) ; Customize depending on whatever is most efficient
;;                        for fixnum operations.
(defvar *max-word-int* (1- (expt 2 *word-size*)))

(defun get-word-size () *word-size*)
(defun set-word-size (word-size)
  "Set a new word size for all arbprec."
  (setf *word-size* word-size
        *max-word-int* (1- (expt 2 *word-size*))))
