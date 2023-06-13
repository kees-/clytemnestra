(ns kees.clytemnestra.impl
  (:import javax.imageio.ImageIO)
  (:require [babashka.fs :as fs]
            [clojure.set :as set]))

;; Conventions
;; 
;; Canvas: 2D array of a piet program as points of [:color :tone]
;;   This includes white and black! Which don't have tones.
;;   :color is first so that color lookups happen uniformly,
;;   and tone lookups for black and white—which shouldn't happen anyway—are nil.
;; 
;; Compass: combination of DP and CC.
;;   The CC is represented by -1 and 1. Gotcha! That's for a concise flip.
;; 

;; ========== IMAGE PROCESSING =================================================
(def valid-formats ["png" "gif"])

(defn get-image
  "Validate a file exists on disk and is an image, returning file from path."
  [file]
  (let [f (fs/file file)
        ext (fs/extension f)]
    (cond
      (not (fs/exists? f)) (throw (Exception. "File not found"))
      (fs/directory? f) (throw (Exception. "You supplied a directory"))
      (not (some #(= ext %) valid-formats)) (throw (Exception. (format "Invalid file format. Give a file of type %s" valid-formats)))
      :else f)))

;; A hardcoded lookup of java color ints to piet colors
;; is presumably a fine bypass for intermediate RGB values.
(def color-map
  {-1        [:white]
   -16777216 [:black]
   -16192    [:red     :light]
   -64       [:yellow  :light]
   -4128832  [:green   :light]
   -4128769  [:cyan    :light]
   -4144897  [:blue    :light]
   -16129    [:magenta :light]
   -65536    [:red     :normal]
   -256      [:yellow  :normal]
   -16711936 [:green   :normal]
   -16711681 [:cyan    :normal]
   -16776961 [:blue    :normal]
   -65281    [:magenta :normal]
   -4194304  [:red     :dark]
   -4145152  [:yellow  :dark]
   -16728064 [:green   :dark]
   -16727872 [:cyan    :dark]
   -16777024 [:blue    :dark]
   -4194112  [:magenta :dark]})

(defn image->canvas
  [file w h size]
  (let [column (fn [n h]
                 (mapv #(color-map (.getRGB file n (* % size)))
                       (range h)))]
    (mapv #(column (* % size) h)
          (range w))))

(defn make-canvas
  "Return a prepared 2D array of pixel data from a given image and codel size."
  [file size]
  (let [image (ImageIO/read (get-image file))
        w (/ (.getWidth image) size)
        h (/ (.getHeight image) size)]
    (when-not (and (int? w) (int? h))
      (throw (Exception. "Image has bad dimensions: must be evenly divisible by codel size")))
    (image->canvas image w h size)))

;; ========== Canvas parsing ===================================================
(def adjacent {:right [1 0] :down [0 1] :left [-1 0] :up [0 -1]})
(def nextdoor-neighbors (vals adjacent))

(defn nextdoor
  "Return the next door neighbors of a coordinate pair."
  [coord]
  (map #(mapv + coord %) nextdoor-neighbors))

(defn find-block
  "From a coordinate, find the expanse of its block on the canvas."
  [canvas coord]
  (let [color (get-in canvas coord)
        family? #(= color (get-in canvas %))]
    (when-not color
      (throw (Exception. "How did you get here? That pixel is off the canvas.")))
    (loop [family #{coord}]
      (let [all-neighbors (reduce into #{} (map nextdoor family))
            new-neighbors (set/difference all-neighbors family)
            new-family (filter family? new-neighbors)]
        (if-not (seq new-family)
          family
          (recur (into family new-family)))))))

;; ========== CANVAS TRAVERSING ================================================
(def seed
  "The initial state of the interpreter."
  {:pointer [0 0]
   :block #{}
   :value nil
   :stack '()
   :compass [:right -1]
   :bonks 0
   :steps 0
   :output ""
   :terminate? false})

;; I don't want to do a bunch of BS rotating stateful pointer sequences
;; Hardcoded CW and CCW cycles are easier...
(def clockwise {:right :down :down :left :left :up :up :right})
(def counterclockwise {:right :up :up :left :left :down :down :right})

;; There are four possible directional outcomes based on dp and cc
;; Top right:
;;   right-left  -> right-up
;;   up-right    -> up-right
;; Bottom right
;;   right-right -> right-down
;;   down-left   -> down-right
;; Bottom left
;;   down-right  -> down-left
;;   left-left   -> left-down
;; Top left
;;   left-right  -> left-up
;;   up-left     -> up-left
;; 
;; That seems redundent, but see that the absolute CC linear direction
;; converts to a consistent relative cardinal direction.

(def compasses
  {[:right :up] [max min]
   [:down :right] [max max]
   [:down :left] [min max]
   [:left :up] [min min]})

(defn find-corner
  "Given the state of the compass, find a block's 'outermost' coordinate."
  [state]
  (let [{:keys [block compass]} state
        [dp cc] compass]
    (if (= 1 (count block))
      (first block)
      (let [absolute-cc (dp (if (pos? cc) clockwise counterclockwise))
            [x y] (compasses (sort [dp absolute-cc]))
            edge (reduce x (map first block))
            edges (filter #(= edge (first %)) block)]
        [edge (reduce y (map second edges))]))))

(defn next-step
  [state]
  (let [{:keys [compass]} state
        dp (first compass)
        corner (find-corner state)]
    (mapv + corner (dp adjacent))))

(defn next-compass
  "The compass alternated between flipping the CC and rotating the DP."
  [state]
  (if (even? (:bonks state))
    (update-in state [:compass 1] -)
    (update-in state [:compass 0] clockwise)))

(defn instruct
  "Takes the state and returns the next effect.
   This does not update the state; it signals what to do next."
  [canvas state]
  (if (< 7 (:bonks state))
    :terminate
    (case (first (get-in canvas (next-step state)))
      :black :search
      nil :search
      :white :slide
      :continue)))

(defn state-continue
  [canvas state]
  (let [{:keys [compass block]} state
        dp (first compass)
        pointer (find-corner state)
        new-pointer (mapv + pointer (dp adjacent))]
    (-> state
        (assoc :bonks 0
               :pointer new-pointer
               :value (count block)
               :block (find-block canvas new-pointer)))))

(defn state-search
  "Having determined the pointer can't step forward,
   update the state in preparation of searching somewhere else."
  [_ state]
  (-> state
      next-compass
      (update :bonks inc)))

;; White's behavior is unusual.
(defn state-slide
  "The pointer's next step is onto white; go forward until something changes."
  [canvas state]
  (loop [state (update state :pointer #(mapv + (-> state :compass first adjacent) %))
         visited []]
    (let [direction (-> state :compass first adjacent)
          {:keys [pointer]} state
          position [pointer direction]]
      (if (some #(= position %) visited)
        ;; Exit point
        (assoc state :terminate? true)
        (let [path (iterate #(mapv + % direction) pointer)
              end (last (take-while #(= [:white] (get-in canvas %)) path))
              new-pointer (mapv + end direction)
              codel (get-in canvas new-pointer)]
          (if (and (some? codel) (not= [:black] codel))
            ;; Exit point
            (assoc state
                   :pointer new-pointer
                   :block (find-block canvas new-pointer))
            (recur (-> state
                       (update-in [:compass 0] clockwise)
                       (update-in [:compass 1] -)
                       (assoc :pointer end))
                   (conj visited position))))))))

(defn tick-state
  [canvas state effect]
  (update
   (case effect
     :slide (state-slide canvas (assoc state :pointer (find-corner state)))
     :continue (state-continue canvas (assoc state :pointer (find-corner state)))
     :search (state-search canvas state)
     :terminate (assoc state :terminate? true)
     (assoc state :terminate? true))
   :steps inc))

(defn interpreter
  [canvas verbose? limit]
  (when verbose? (println "Hi!"))
  (loop [state (assoc seed :block (find-block canvas (:pointer seed)))]
    (when (< limit (:steps state))
      (throw (Exception. "Number of steps exceeded limit")))
    (when-not (seq (:pointer state))
      (throw (Exception. "The pointer got lost!")))
    (let [instruction (instruct canvas state)]
      (when verbose?
        (println instruction (dissoc state :block)))
      (if (:terminate? state)
        state
        (recur (tick-state canvas state instruction))))))
