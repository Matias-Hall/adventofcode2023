# Null-coalescing operator stolen from Kotlin.
(defn ?: [nillable dflt]
  "Evaluates to dflt if nillable is nil. A null-coalescing operator."
  (if (nil? nillable)
    dflt
    nillable))

(defn split-lines [text]
  "Split text on any newlines."
  (peg/match
    ~{
      :main (any (* (<- :line) 1))
      :line (any (if-not "\n" 1))
      }
    text))

