(use ../common/common)

(def input (slurp "input"))

(def sequence-parse
  (peg/compile
    ~{
      :main (some (* :digit :s*))
      :digit (/ (<- (some (set "-0123456789"))) ,scan-number)
      }))

(defn get-sequence [line]
  (peg/match sequence-parse line))

# Using this for two reasons:
# 1. We can avoid doing the difference for the whole sequence
# 2. If the same value is obtained twice, we don't do the computation the second time
#    (this shouldn't really happen in this case, but whatever)
(defn memoize-function [function]
  (fiber/new
    (fn [index]
      # Shadowing the parameter to make it a variable.
      (var index index)
      (def memo-values @{})
      (forever
        (when (nil? (memo-values index))
          (set (memo-values index) (function index)))
        (set index (yield (memo-values index)))))))

# If Janet had currying, this would look better.
# We just return a function that, given an index, returns that difference for a sequence.
(defn get-difference-function [sequence]
  (fn [index]
    (def f (resume sequence index))
    (def s (resume sequence (inc index)))
    (unless (nil? s)
      (- s f))))

(defn difference [sequence]
  (memoize-function (get-difference-function sequence)))

(defn get-difference-terms [sequence]
  (def first-term (resume sequence 0))
  (def second-term (resume sequence 1))
  (def diff (difference sequence))
  (if (nil? second-term)
    @[first-term]
    (array/push
      (get-difference-terms diff)
      first-term)))

(defn choose [n k]
  (defn factorial [a] (if (<= a 0) 1 (product (range 1 (inc a)))))
  (/ (factorial n) (factorial k) (factorial (- n k))))

(defn main [& args]
  (var result 0)
  (def lines (split-lines input))
  (each line lines
    # To solve this problem, we flip the sequence.
    (def sequence (reverse (get-sequence line)))
    (def diff-list (reverse (get-difference-terms (memoize-function |(get sequence $)))))
    (def k (length (get-sequence line)))
    (eachp [i a] diff-list
      (+= result (* (choose k i) a))))
    (print result))
