;;;; Top-level layer for operations on words.
(in-package :arbprec)

(defun add (words1 words2)
  "Add words1 and words2."
  (add-naive-words words1 words2))

(defun sub (words1 words2)
  "Subtract words2 from words1. words2 <= words1 required, other
   case is undefined behaviour."
  (subtract-naive-words words1 words2))

(defun mul (words1 words2)
  "Multiply words1 and words2. The method is chosen depending on the
   size of the arguments."
  (mul-words-naive words1 words2))
