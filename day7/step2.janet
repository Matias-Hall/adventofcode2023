(def input (slurp "input"))

(defn split-lines [text]
  (peg/match
    ~{
      :main (any (* (<- :line) 1))
      :line (any (if-not "\n" 1))
      }
    text))


(def cards [:J :2 :3 :4 :5 :6 :7 :8 :9 :T :Q :K :A])

(def play-parse
  (peg/compile
    ~{
      :main (* (group (repeat 5 :card)) " " :number)
      :card (/ (<- (set "AKQJT98765432")) ,keyword)
      :number (/ (<- :d+) ,scan-number)
      }))

(defn get-play [line]
  (peg/match play-parse line))

# Is memoization necessary? Who knows
# Is this the best way to memoize? Probably not, I'm sure macros can be used to memoize a function.
(def memo-hand-strength @{})
(defn hand-strength [hand]
  (if (nil? (memo-hand-strength hand))
    (do
      (var counts @[])
      (each card cards
        (array/push counts (count |(= $ card) hand)))
      # Takes the jokers from counts.
      (def jokers (first counts))
      (array/remove counts 0)
      (reverse! (sort counts))
      # It is always best to add jokers to the strongest card type.
      (+= (counts 0) jokers)
      # Returns a hand strength number.
      # 6 is the strongest (five of a kind)
      # 0 is the weakest (high card)
      (def strength
        (cond
          (= 5 (first counts)) 6
          (= 4 (first counts)) 5
          (and
            (= 3 (first counts))
            (= 2 (counts 1))) 4
          (= 3 (first counts)) 3
          (and
            (= 2 (first counts))
            (= 2 (counts 1))) 2
          (= 2 (first counts)) 1
          0))
      (set (memo-hand-strength hand) strength)
      strength)
    (memo-hand-strength hand)))

(defn card< [left right]
  (<
   (find-index |(= left $) cards)
   (find-index |(= right $) cards)))

(defn strength-by-hand [left right]
  (prompt :result
    (for i 0 5
      (unless (= (left i) (right i))
        (return :result (card< (left i) (right i)))))
    (return :result (card< (last left) (last right)))))

# Compares two hands
(defn hand< [left right]
  (def left-strength (hand-strength left))
  (def right-strength (hand-strength right))
  (if (= left-strength right-strength)
    (strength-by-hand left right)
    (< (hand-strength left) (hand-strength right))))


(defn main [& args]
  (def plays-list (map get-play (split-lines input)))
  (def plays @{})
  (each [hand bid] plays-list
    (set (plays hand) bid))
  (def hands (keys plays))
  (var result 0)
  (eachp [i hand] (sort hands hand<)
    (+= result (* (+ 1 i) (plays hand))))
  (print result))
