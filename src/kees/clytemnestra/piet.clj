(ns kees.clytemnestra.piet)

;; I don't want to do a bunch of BS rotating stateful pointer sequences
;; Hardcoded CW and CCW cycles are easier...
(def clockwise {:right :down :down :left :left :up :up :right})
(def counterclockwise {:right :up :up :left :left :down :down :right})

;; A hardcoded lookup of java color ints to piet colors
;; is presumably a fine bypass for intermediate RGB values.
(def color-map
  {-1        [:white]
   -16777216 [:black]
   -16192    [0 0] #_[:red     :light]
   -64       [1 0] #_[:yellow  :light]
   -4128832  [2 0] #_[:green   :light]
   -4128769  [3 0] #_[:cyan    :light]
   -4144897  [4 0] #_[:blue    :light]
   -16129    [5 0] #_[:magenta :light]
   -65536    [0 1] #_[:red     :normal]
   -256      [1 1] #_[:yellow  :normal]
   -16711936 [2 1] #_[:green   :normal]
   -16711681 [3 1] #_[:cyan    :normal]
   -16776961 [4 1] #_[:blue    :normal]
   -65281    [5 1] #_[:magenta :normal]
   -4194304  [0 2] #_[:red     :dark]
   -4145152  [1 2] #_[:yellow  :dark]
   -16728064 [2 2] #_[:green   :dark]
   -16727872 [3 2] #_[:cyan    :dark]
   -16777024 [4 2] #_[:blue    :dark]
   -4194112  [5 2] #_[:magenta :dark]})

(defn- cycle-difference
  "Given two numbers within a looping range of length l, find their distance."
  [a b l]
  (let [r (cycle (range l))]
    (->> r
         (drop-while #(not= a %))
         (take-while #(not= b %))
         count)))

(defn find-hue-shift
  "Find Piet's 'distance' between two colors"
  [[a b] [c d]]
  [(cycle-difference a c 6)
   (cycle-difference b d 3)])

;; ========== COMMANDS IN LANGUAGE SPEC ========================================
(defn ppush
  "Push `:value` onto stack"
  [state]
  (update state :stack #(cons (:value state) %)))

(defn ppop
  "Discard top value from stack"
  [state]
  (update state :stack rest))

(defn- parithmetic
  "Construct the 5 included basic arithmetic operations, + - * / %"
  [op]
  (fn [state]
    (if (or (< (count (:stack state)) 2)
            (and (zero? (first (:stack state)))
                 (some #{op} [/ mod])))
      state
      (let [out (apply (comp int op) (reverse (take 2 (:stack state))))]
        (update state :stack #(cons out (nnext %)))))))

(def p+ "Add top two stack values and replace in stack."
  (parithmetic +))
(def p- "Subtract top from second stack value and replace in stack."
  (parithmetic -))
(def p* "Multiply top two stack values and replace in stack."
  (parithmetic *))
(def p÷ "Divide second from first stack value and replace in stack."
  (parithmetic /))
(def p% "Modulous divide second from first stack value and replace in stack."
  (parithmetic mod))

(defn pnot
  "Flip top stack value to 1 if 0 and 0 if otherwise."
  [{:keys [stack] :as state}]
  (if (seq stack)
    (let [n (if (zero? (first stack)) 1 0)]
      (update state :stack #(cons n (rest %))))
    state))

(defn p>
  "Replace top two stack values with 1 if second is GT first, otherwise 0."
  [{:keys [stack] :as state}]
  (if (< 1 (count stack))
    (let [n (if (apply < (take 2 stack)) 1 0)]
      (update state :stack #(cons n (nnext %))))
    state))

(defn pdp
  "Rotate pointer n times CW or CCW as per signed top stack value (then pop)."
  [{:keys [stack] :as state}]
  (if-let [n (first stack)]
    (let [op (if (pos? n) clockwise counterclockwise)]
      (ppop (update-in state [:compass 0] #(nth (iterate op %) (abs n)))))
    state))

(defn pcc
  "Flip codel chooser n times as per top stack value (then pop)."
  [{:keys [stack] :as state}]
  (if-let [n (first stack)]
    (ppop (update-in state [:compass 1] #(nth (iterate - %) (abs n))))
    state))

(defn pdupe
  "Get and push top stack value."
  [{:keys [stack] :as state}]
  (if (first stack)
    (update state :stack #(cons (first %) %))
    state))

(defn- proll*
  "Implementation for rolling the stack once to given depth."
  [depth +?]
  (fn [stack]
    (if +?
      (reduce into []
              [(rest (take depth stack))
               (take 1 stack)
               (drop depth stack)])
      (reduce into []
              [(take 1 (drop (dec depth) stack))
               (take (dec depth) stack)
               (drop depth stack)]))))

(defn proll
  "Piet's weird command"
  [{:keys [stack] :as state}]
  (let [[depth n] stack]
    (if (or (not n)
            (< depth 2)
            (< (- (count stack) 2) depth))
      state
      (update state :stack #(as-> % c
                              (drop 2 c)
                              (iterate (proll* depth (pos? n)) c)
                              (nth c (abs n))
                              (or (seq c) '()))))))

(defn pinn
  "Ask for a signed integer input and push onto the stack."
  [state]
  (let [input (do (print "Number input: ")
                  (flush)
                  (read-line))
        s (re-find #"-?\d+" input)]
    (cond-> state
      s (update :stack #(cons (Integer. s) %)))))

(defn pinc
  "Ask for one unicode character input and push onto the stack."
  [state]
  (let [input (do (print "Character input: ")
                  (flush)
                  (read-line))
        c (first input)]
    (cond-> state
      c (update :stack #(cons (int c) %)))))

(defn poutn
  "Push top value from the stack and add to output string."
  [{:keys [stack] :as state}]
  (if-let [n (first stack)]
    (ppop (update state :output #(cons n %)))
    state))

(defn poutc
  "Push top value from the stack and add as unicode character to output string."
  [{:keys [stack] :as state}]
  (if-let [n (first stack)]
    (if-let [c (when (and (char n) (< 31 n))
                 (char n))]
      (ppop (update state :output #(cons c %)))
      state)
    state))

(def commands
  "Reference table for looking up piet commands"
  [[nil   ppush ppop]
   [p+    p-    p*]
   [p÷    p%    pnot]
   [p>    pdp   pcc]
   [pdupe proll pinn]
   [pinc  poutn poutc]])
