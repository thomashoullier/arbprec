;;;; Operations on arrays of words.
(in-package :arbprec)

(defun words-ash (words nwords)
  "Perform a word shift by nwords towards the most-significant end of the
   array. (Positive nwords create bigger numbers.)"
  (cond ((= nwords 0) (copy-seq words))
        ((> nwords 0) (concatenate 'vector
                                   (make-array nwords :initial-element 0)
                                   words))
        (T                             ; negative shifts
         (subseq words (min (- nwords) (length words))))))

(defun split (words upto-n)
  "Split a vector of words into two vectors of words.
   (values words1 words2).
   The first word is the least-significant, has words from 0 up to
   upto-n included."
  (values (subseq words 0 (1+ upto-n)) (subseq words (1+ upto-n))))

(defun prune-zero-msb (words)
  "Remove all msb that are zero in an vector of words. Destructive."
  (loop for msb across (reverse words)
        while (and (= msb 0) (> (length words) 1)) do
          (vector-pop words))
  words)
