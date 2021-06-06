;;;; Conversions to and from words.
(in-package :arbprec)

(defun to-integer (words)
  "Convert words to a native integer."
  (let ((cur-base (- *word-size*)))
    (loop for word across words
          sum (* word (expt 2 (incf cur-base *word-size*))))))

(defun to-bitvector (words)
  "Convert words to a bitvector. LSB is first. Up to last non-zero MSB."
  (let ((bitvec))
    (loop for word across words do
      (setf bitvec (concatenate 'bit-vector
                                bitvec
                                (word-to-bitvector word))))
    (prune-last-zeros-bitvector bitvec)))

;;; Helpers
(defun word-to-bitvector (word)
  "Convert a word-sized integer to a bit vector.
   Return the bitvector with LSB first."
  (let ((bitvec (make-array *word-size* :element-type 'bit)))
    (loop for i from 0 below *word-size* do
      (setf (aref bitvec i) (if (logbitp i word) 1 0)))
    bitvec))

(defun prune-last-zeros-bitvector (bitvec)
  "Prune the last zeros in a bitvector. Keep the zero for bigint zero."
  (let ((last-one-pos 0))
    (loop named find-last-one for i from (1- (length bitvec)) downto 0 do
      (when (= (aref bitvec i) 1) (setf last-one-pos i)
            (return-from find-last-one)))
    (subseq bitvec 0 (1+ last-one-pos))))
