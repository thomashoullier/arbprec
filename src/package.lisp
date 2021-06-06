(defpackage :arbprec
  (:use :cl)
  (:export
   #:make-words
   ;; Readers and tests
   #:zero-p
   #:length-in-bits
   ;; Words simple operations
   #:words-ash
   #:split
   #:prune-zero-msb
   ;; Conversions
   #:to-integer
   #:to-bitvector
   ;; Comparisons
   #:compare-words
   ;; Operations
   #:add-naive-words
   #:subtract-naive-words
   #:mul-words-naive
   ;; Optimized multiplications
   #:mul-words-karatsuba))

(in-package :arbprec)
;; TODO: * Interface to get/set the word size.

;; the size in bits of the words used for representing bigints
(defvar *word-size* 32) ; Customize depending on whatever is most efficient
;;                        for fixnum operations.
(defvar *max-word-int* (1- (expt 2 *word-size*)))
