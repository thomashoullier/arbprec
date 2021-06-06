;;;; Operations on arrays of words.
(in-package :arbprec)

(defun words-ash (words nwords)
  "Perform a word shift by nwords towards the most-significant end of the
   array. (Positive nwords create bigger numbers.)"
  (cond ((= nwords 0) (copy-seq words))
        ((zero-p words) #(0)) ; zero shifted remains zero
        ((> nwords 0) (concatenate 'vector
                                   (make-array nwords :initial-element 0)
                                   words))
        (T                             ; negative shifts
         (if (>= (abs nwords) (length words))
             #(0) ; zero is the lowest we can go.
             (subseq words (min (- nwords) (length words)))))))

(defun split (words upto-n)
  "Split a vector of words into two vectors of words.
   (values words1 words2).
   The first word is the least-significant, has words from 0 up to
   upto-n included.
   Empty vectors are #(0). Zero msb are pruned."
  (values (prune-zero-msb (replace-empty-vec-with-zero
                           (subseq words 0 (1+ upto-n))))
          (replace-empty-vec-with-zero (subseq words (1+ upto-n)))))

(defun replace-empty-vec-with-zero (vec)
  (if (= (length vec) 0) #(0) vec))

(defun prune-zero-msb (words)
  "Remove all msb that are zero in an vector of words."
  (let ((reswords (make-array (length words) :fill-pointer (length words)
                                             :initial-contents words)))
    (loop for msb across (reverse reswords)
          while (and (= msb 0) (> (length reswords) 1)) do
            (vector-pop reswords))
    reswords))
