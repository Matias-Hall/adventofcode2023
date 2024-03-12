(def input (slurp "input"))

(defn split-lines [text]
  (peg/match
    ~{
      :main (any (* (<- :line) 1))
      :line (any (if-not "\n" 1))
      }
    text))

(def line-peg (peg/compile
    ~{
      :main (any (+ :digit 1))
      :digit (<- (range "09"))
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
