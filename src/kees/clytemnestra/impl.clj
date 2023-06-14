(ns kees.clytemnestra.impl
  (:import javax.imageio.ImageIO)
  (:require [babashka.fs :as fs]
            [clojure.set :as set]
            [kees.clytemnestra.piet :as p]))

;; Conventions
;; 
;; Canvas: 2D array of a piet program as points of [m n], [:white], or [:black].
;;    m and n correspond to lookup coords for their location in the color cycle.
;;    This way, arbitrary palettes could be supplied.
;; 
;; Compass: combination of DP and CC.
;;   The CC is represented by -1 and 1. Gotcha! For a concise flip.
;;   -1 left, 1 right
;; 
;; Bonks: Number of times pointer has consecutively hit a wall or edge.
;; Slide: Movement mechanic for traversing whitespace.
;; Continue: Logic for a valid move from one color block to another.
;; Search: Unmoving compass rotation for finding valid block exits.

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

(defn image->canvas
  "Take an image file with dimensions and find color values for every pixel."
  [file w h size]
  (let [column (fn [n h]
                 (mapv #(p/color-map (.getRGB file n (* % size)))
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

;; Thank you clj-piet for greatly improving my block search performance
(defn find-block
  "From a coordinate, find the expanse of its block on the canvas."
  [canvas coord]
  (let [color (get-in canvas coord)
        family? #(= color (get-in canvas %))]
    (when-not color
      (throw (Exception. "How did you get here? That pixel is off the canvas.")))
    (loop [family #{coord}
           neighborhood #{coord}]
      (if-not (seq neighborhood)
        family
        (let [neighbors (->> neighborhood
                             (mapv nextdoor)
                             (reduce into #{})
                             (set/select family?))]
          (recur (set/union family neighborhood)
                 (set/difference neighbors family)))))))

;; ========== CANVAS TRAVERSING ================================================
(def seed
  "The initial state of the interpreter."
  {:pointer [0 0]
   :block #{}
   :color nil
   :value nil
   :stack '()
   :compass [:right -1]
   :bonks 0
   :steps 0
   :output '()
   :terminate? false})

(def compasses
  {[:right :up] [max min]
   [:down :right] [max max]
   [:down :left] [min max]
   [:left :up] [min min]})

;; Interested how I can simplify this at least for readability..
;; DP and CC are treated as interchangeable to determine prevailing direction,
;; but DP is needed to find the order in which that direction is traveled.
(defn find-corner
  "Given the state of the compass, find a block's 'outermost' coordinate."
  [state]
  (let [{:keys [block compass]} state
        [dp cc] compass]
    (if (= 1 (count block))
      (first block)
      (let [absolute-cc (dp (if (pos? cc) p/clockwise p/counterclockwise))
            order (if (some #{dp} [:up :down]) reverse identity)
            [extreme-1 extreme-2] (order (compasses (sort [dp absolute-cc])))
            [axis-1 axis-2] (order [first second])
            edge-1 (reduce extreme-1 (map axis-1 block))
            edges (filter #(= edge-1 (axis-1 %)) block)
            edge-2 (reduce extreme-2 (map axis-2 edges))
            corner (vec (order [edge-1 edge-2]))]
        corner))))

(defn next-step
  "Find the extreme of the currently occupied block and step forward 1 codel.
   Doesn't check for validity!"
  [state]
  (let [{:keys [compass]} state
        dp (first compass)
        corner (find-corner state)]
    (mapv + corner (dp adjacent))))

(defn next-compass
  "Flip the CC or rotate the DP depending on how many times it's done so."
  [state]
  (if (even? (:bonks state))
    (update-in state [:compass 1] -)
    (update-in state [:compass 0] p/clockwise)))

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

(defn state-terminate
  "Signal the program is complete."
  [_ state]
  (assoc state :terminate? true))

(defn inspect-codel
  "Check out what's going on at the current codel's block."
  [canvas {:keys [pointer] :as state}]
  (let [block (find-block canvas pointer)]
    (assoc state
           :color (get-in canvas pointer)
           :block block
           :value (count block))))

(defn state-continue
  "Exit the extreme of the current block into the next."
  [canvas state]
  (let [{:keys [block color]} state
        new-pointer (next-step state)
        new-color (get-in canvas new-pointer)
        command (get-in p/commands (p/find-hue-shift color new-color))]
    (-> state
        (assoc :bonks 0
               :value (count block)
               :pointer new-pointer
               :color new-color
               :block (find-block canvas new-pointer))
        command)))

(defn state-search
  "Having determined the pointer can't step forward,
   prepare to search somewhere else."
  [_ state]
  (-> state
      next-compass
      (update :bonks inc)))

;; White's behavior is unusual.
;; How can I improve this?
(defn state-slide
  "The pointer's next step is onto white; go forward until something changes."
  ([canvas state] (let [state (assoc state
                                     :pointer (next-step state)
                                     :bonks 0)]
                    (state-slide canvas state [])))
  ([canvas state visited]
   (let [direction (-> state :compass first adjacent)
         {:keys [pointer]} state
         position [pointer direction]]
     (if (some #(= position %) visited)
        ;; Exit point
       (state-terminate canvas state)
       (let [path (iterate #(mapv + % direction) pointer)
             end (last (take-while #(= [:white] (get-in canvas %)) path))
             new-pointer (mapv + end direction)
             codel (get-in canvas new-pointer)]
         (if (and (some? codel) (not= [:black] codel))
            ;; Exit point
           (inspect-codel canvas (assoc state :pointer new-pointer))
           (recur canvas
                  (-> state
                      (update-in [:compass 0] p/clockwise)
                      (update-in [:compass 1] -)
                      (assoc :pointer end))
                  (conj visited position))))))))

(defn tick-state
  "Calculate the subsequent state based on what was determined to happen next."
  [canvas state effect]
  (let [instruction (case effect
                      :slide state-slide
                      :continue state-continue
                      :search state-search
                      :terminate state-terminate
                      state-terminate)]
    (instruction canvas (update state :steps inc))))

(defn interpreter
  "Main interpreter machine.. Returns the statemap of a terminated program"
  [canvas verbose? limit]
  (loop [state (inspect-codel canvas seed)]
    (cond
      (and (pos? limit) (< limit (:steps state)))
      (throw (Exception. "Number of steps exceeded limit"))
      (not (seq (:pointer state)))
      (throw (Exception. "The pointer got lost!")))
    (let [instruction (instruct canvas state)]
      (when verbose? (println (dissoc state :block)))
      (if (:terminate? state)
        state
        (recur (tick-state canvas state instruction))))))
