(in-package :arbprec)

(defun mul-words-karatsuba (words1 words2 &key (size-thresh 1))
  "Multiply two arrays of words using the Karatsuba method.
   size-thresh controls the size of either words to multiply at which
   the usual multiplication is used. Default is 1 word long."
  (let ((l1 (length words1)) (l2 (length words2)))
    ;; Recursion termination: multiply naively when one of the words
    ;; is small enough.
    (when (or (<= l1 size-thresh) (<= l2 size-thresh))
      (return-from mul-words-karatsuba (mul-words-naive words1 words2)))
    ;; If not, we split, multiply the pieces and add them together.
    (let ((middle (floor (/ (min l1 l2) 2)))
          (low1) (high1) (low2) (high2)
          (z0) (z1) (z2))
      (multiple-value-setq (low1 high1) (split words1 (1- middle)))
      (multiple-value-setq (low2 high2) (split words2 (1- middle)))
      (psetf z0 (mul-words-karatsuba low1 low2)
             z1 (mul-words-karatsuba (add-naive-words low1 high1)
                                     (add-naive-words low2 high2))
             z2 (mul-words-karatsuba high1 high2))
      (reduce #'add-naive-words
              (list (words-ash z2 (* 2 middle))
                    (words-ash (subtract-naive-words
                                z1 (add-naive-words z2 z0))
                               middle)
                    z0)))))
