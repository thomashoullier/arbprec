# Arbitrary Precision (Core)
`arbprec` is a lower-level API for arbitrary precision arithmetic.
It can be used as the basis for constructing representation and arithmetic
on arbitrary precision unsigned integers (and from there many other things).

We are concerned here with the manipulation of vectors of words (bytes).

## Context
Most of the use cases for arbitrary precision integers in Common Lisp are
covered by `bignum`. I implemented `arbprec` for the sake of exploring the
subject. `arbprec` might become worthwhile over bignums performance-wise once
I implement fast multiplications and tweak the performance.

## Usage
The base objects are vectors of words/bytes from LSB to MSB (0 index to end).
The size in bits of a word/byte in the system can be adjusted just after loading
the system. Adjusting the word size while words are already declared is
undefined.

### Word size parameter
**get-word-size** => *word-size*
**set-word-size** *word-size* => *word-size*

Reader and writer for the *word-size* parameter. *word-size* is expressed
in number of bits. The idea is that it should match the size of fixnums, for
which operations are efficient.

### Make words
**make-words** *integer* => *words*

Create *words*, a vector of words/bytes from a native Common Lisp *integer*.
*integer* can be a *bignum*.

### Readers and tests
**zero-p** *words* => *boolean*

Check whether *words* represents zero. Zero is defined as `#(0)`.

**length-in-bits** *words* => *nbits*

Return the size in bits of *words*.

```common-lisp
(length-in-bits (make-words 0))
;;=> 0
(length-in-bits (make-words 16))
;;=> 5
```

### Simple operations
**words-ash** *words* *shift* => *shifted-words*

`#'ash` equivalent for words. *words* is shifted *shift* (positive or negative)
places towards the MSB (end of the vector). Negative shifts will never go lower
than the zero word. Positive shift applied on zero is still zero.

```common-lisp
(words-ash #(4) 2)
;;=> #(0 0 4)
(words-ash #(0) -1)
;;=> #(0)
```

**split** *words* *upto-n* => *words-lsb*, *words-msb*

Split *words* into two words: *words-lsb* from word 0 up to word
*upto-n* included, *words-msb* the remaining words. Never returns
empty words but zero instead. Prunes the most significant zeros in
the splitted words.

```common-lisp
(split #(1 2 3 4 5) 2)
;;=> #(1 2 3) #(4 5)
(split #(4 3 0 0 8 9) 2)
;;=> #(4 3) #(0 8 9)
(split #(4 3 0 0 8 9) -1)
;;=> #(0) #(4 3 0 0 8 9)
```

### Conversions
**to-integer** *words* => *integer*

Convert *words* to a native Common Lisp *integer*.

**to-bitvector** *words* => *bitvector*

Convert *words* to a *bitvector* representation. The bitvector
always ends with a 1, except for the zero word.

```common-lisp
(to-bitvector (make-words 9))
;;=> #*1001
(to-bitvector (make-words 0))
;;=> #*0
```

### Comparison
**compare-words** *words1* *words2* => *comparison-result*

Compare two words. The result can be `'less-than` (*words1* < *words2*),
`'greater-than` or `'same`.

### Operations
**add-naive-words** *words1* *words2* => *added-words*

Compute the addition of *words1* and *words2*.

**subtract-naive-words** *words1* *words2* => *subtraction*

Compute the subtraction of *words2* from *words1*. The case where
*words2* is greater than *words1* is undefined.

**mul-words-naive** *words1* *words2* => *multiplied-words*

Multiply *words1* and *words2*. Uses the naive `O(n^2)` method suitable
for "small" numbers.

### Optimized multiplication
**mul-words-karatsuba** *words1* *words2* &key *size-thresh*
  => *multiplied-words*

Multiply *words1* and *words2* using the Karatsuba multiplication, which is
asymptotically `O(n^{log2(3)})` (`O(n^1.58)`). The *size-thresh* is a parameter
that sets the length threshold (in number of words) that either of the
multiplied words needs to be in order to trigger a naive multiplication to end
the recursion.

## Dependencies
* `arbprec`: None.
* `arbprec/test`:
  * [rove](https://github.com/fukamachi/rove)

## Tests
Launch tests with:

```common-lisp
(asdf:test-system "arbprec")
```

## See also
* [computable-reals](https://github.com/stylewarning/computable-reals)
* [hypergeometrica](https://github.com/stylewarning/hypergeometrica)

## References
1. Knuth, Donald E. Art of computer programming, volume 2: Seminumerical
   algorithms. Addison-Wesley Professional, 2014. **Section 4.3**
2. https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic
3. J. Arndt, Matters Computational: Ideas, Algorithms, Source Code (Springer
   Science & Business Media, 2010). **Chapter 28**
4. https://en.wikipedia.org/wiki/Karatsuba_algorithm
