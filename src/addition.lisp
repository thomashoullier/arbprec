;;;; Addition operation for words.
(in-package :arbprec)

(defun add-naive-words (words1 words2)
  "Add two arrays of words (LSB first) together. Naive method with carry."
  (let* ((maxlength (max (length words1) (length words2)))
         (longest-num (if (= maxlength (length words1)) words1 words2))
         (reswords (make-array maxlength :fill-pointer 0))
         (carry 0) (tmp 0) (cur-ind 0))
    ;; Loop over the common bytes.
    (loop for word1 across words1
          for word2 across words2 do
            (multiple-value-setq (carry tmp)
              (truncate (+ word1 word2 carry) (1+ *max-word-int*)))
            (vector-push tmp reswords)
            (incf cur-ind))
    ;; Loop over the remaining bytes in the longest array.
    (loop for i from cur-ind below maxlength do
      (multiple-value-setq (carry tmp)
        (truncate (+ (aref longest-num i) carry) (1+ *max-word-int*)))
      (vector-push tmp reswords))
    ;; Apply remaining carry.
    (when (= carry 1) (vector-push-extend 1 reswords))
    reswords))
