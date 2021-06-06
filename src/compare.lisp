;;;; Comparison for words.
(in-package :arbprec)

(defun compare-words (words1 words2)
  "Compare two arrays of words. It is assumed the MSB is never zero except
   when the number is zero!
   Return:
   * 'less-than : words1 < words2
   * 'greater-than : words1 > words2
   * 'same : words1 = words2"
  (let ((l1 (length words1))
        (l2 (length words2)))
    (when (< l1 l2) (return-from compare-words 'less-than))
    (when (> l1 l2) (return-from compare-words 'greater-than))
    ;; We simply compare the words starting from MSB and going downwards.
    (let ((word1) (word2))
      (loop for i from (1- l1) downto 0 do
        (psetf word1 (aref words1 i) word2 (aref words2 i))
        (cond ((< word1 word2) (return-from compare-words 'less-than))
              ((> word1 word2) (return-from compare-words 'greater-than)))))
    'same))
