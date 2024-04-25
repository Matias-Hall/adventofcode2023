(use ../common/common)

(def input (slurp "input"))

(def line-peg (peg/compile
    ~{
      :main (any (+ :digit :spelt-digits 1))
      :digit (<- (range "09"))
      :spelt-digits (+
        # `look 0` is used to avoid consuming letters for another digit
        # For example, "oneight" includes the digits 1 and 8,
        # but with a simpler expression, "one" will consume the "e"
        # leaving "ight" which does not work
        # `look 0` matches the text without advancing, then a constant is produced
        (* (look 0 "one") (constant "1") 1)
        (* (look 0 "two") (constant "2") 1)
        (* (look 0 "three") (constant "3") 1)
        (* (look 0 "four") (constant "4") 1)
        (* (look 0 "five") (constant "5") 1)
        (* (look 0 "six") (constant "6") 1)
        (* (look 0 "seven") (constant "7") 1)
        (* (look 0 "eight") (constant "8") 1)
        (* (look 0 "nine") (constant "9") 1))
      }))

(defn extract-digits [line]
  (peg/match line-peg line))

(defn get-calibration-value [digits]
  (scan-number (string (first digits) (last digits))))

(defn main [& args]
  (var sum 0)
  (each line (split-lines input)
    (set sum
         (+ sum
            (-> line
                 (extract-digits)
                 (get-calibration-value)))))
  (print sum))

# Print all the extracted digits, for debugging
# (defn main [& args]
#   (each line (split-lines input)
#     (-> line
#          (extract-digits)
#          (pp))))
