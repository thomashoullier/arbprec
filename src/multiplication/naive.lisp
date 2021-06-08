;;;; Naive multiplication method.
(in-package :arbprec)

(defun mul-words-naive (words1 words2)
  "Multiply two arrays of words together. LSB first. Naive O(n^2) method."
  ;; See Knuth TAOCP Section 4.3.1 Algorithm M.
  (let* ((l1 (length words1)) (l2 (length words2))
         (reswords (make-array (+ l1 l2) :fill-pointer (+ l1 l2)
                                         :initial-element 0))
         (tmp 0) (carry 0))
    (loop for word1 across words1 for j from 0 do
      (setf carry 0)
      (loop for word2 across words2 for i from 0 do
        (multiple-value-setq (carry tmp)
          (truncate (+ (* word1 word2) carry (aref reswords (+ i j)))
                    (1+ *max-word-int*)))
        (setf (aref reswords (+ i j)) tmp))
      (setf (aref reswords (+ j l2)) carry))
    (prune-zero-msb reswords)))
