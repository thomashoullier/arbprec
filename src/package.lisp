(defpackage :arbprec
  (:use :cl)
  (:export
   ;; Readers and tests
   #:zero-p
   #:length-in-bits
   ;; Conversions
   #:to-integer
   #:to-bitvector
   ))

(in-package :arbprec)
;; TODO: * Interface to get/set the word size.

;; the size in bits of the words used for representing bigints
(defvar *word-size* 32) ; Customize depending on whatever is most efficient
;;                        for fixnum operations.
(defvar *max-word-int* (1- (expt 2 *word-size*)))
