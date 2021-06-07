;;;; arbprec simple operations, readers and tests.
(in-package :arbprec)

(defun make-words (integer)
  "Create a vector of words from a native integer (can be a bignum)."
  (let ((remainder (abs integer))
        (words (make-array 1 :fill-pointer 0))
        (cur-integer 0)
        (bitgate (1- (expt 2 *word-size*))))
    ;; We apply an AND over the remainder of the integer to convert
    ;; with a gate of all 1's over the current word.
    (loop do
      (setf cur-integer (logand bitgate remainder)
            remainder (ash remainder (- *word-size*)))
      (vector-push-extend cur-integer words)
          while (> remainder 0))
    words))

;;; Readers and tests.
(defun zero-p (words)
  "Does the vector of words represent a zero?"
  ;; A zero should always be a single word of value zero.
  (and (= (length words) 1) (= 0 (aref words 0))))

(defun length-in-bits (words)
  "Length of a vector of words in bits. Up to the last non-zero MSB."
  (when (zero-p words) (return-from length-in-bits 0))
  (let (;; Length in bits of all words except most significant.
        (len-lsb (1+ (* (1- (length words)) *word-size*)))
        (msb-pos *word-size*)
        (msb-word (aref words (1- (length words)))))
    ;; Find the MSB in the last word.
    (loop until (logbitp (decf msb-pos) msb-word))
    (+ len-lsb msb-pos)))
