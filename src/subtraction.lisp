(in-package :arbprec)

(defun subtract-naive-words (words1 words2)
  "Subtract two arrays of words (LSB first). words1 - words2, with
   words1 >= words2, it is also assumed that no zero msb are present.
   Naive method with borrow."
  ;; TODO: * Write a test for fixed bug: #(0 2) - #(1)
  (let* ((l1 (length words1))
         (reswords (make-array l1 :fill-pointer 0))
         (borrow 0) (tmp 0) (cur-ind 0))
    ;; Loop over the common bytes.
    (loop for word1 across words1
          for word2 across words2 do
            (multiple-value-setq (borrow tmp)
              (floor (+ word1 (- word2) borrow) (1+ *max-word-int*)))
            (vector-push tmp reswords)
            (incf cur-ind))
    ;; Loop over the remaining bytes in the longest array.
    (loop for i from cur-ind below l1 do
      (multiple-value-setq (borrow tmp)
        (floor (+ (aref words1 i) borrow) (1+ *max-word-int*)))
      (vector-push tmp reswords))
    ;; No borrow remaining since words1 >= words2.
    (prune-zero-msb reswords)))
