(ns kees.clytemnestra.helpers
  (:require [babashka.process :refer [shell]]
            [clojure.string :as s]))

(def ^:private colors
  [[[255 192 192] [255   0   0] [192   0   0]]
   [[255 255 192] [255 255   0] [192 192   0]]
   [[192 255 192] [0   255   0] [0   192   0]]
   [[192 255 255] [0   255 255] [0   192 192]]
   [[192 192 255] [0   0   255] [0     0 192]]
   [[255 192 255] [255 0   255] [192   0 192]]])

;; ========== PRETTY-PRINTING PROGRAM IMAGE AT COMMAND LINE ====================
(defn- rgb-escape
  "Format escapes and codes for RGB color in the terminal."
  [{:keys [rgbv text]}]
  (let [esc #(reduce str (interpose \; %))
        fg (esc (reduce into [38 2] [rgbv]))
        bg (esc (reduce into [48 2] [rgbv]))
        line [\u001b \[ bg \m \u001b \[ fg \m text \u001b "[0m"]]
    (reduce str line)))

(defn- codel->block-print
  "Turns one color coordinate a full block character with FG and BG color set."
  [coord]
  (let [color (case coord
                [:black] [0 0 0]
                [:white] [255 255 255]
                (get-in colors coord [210 210 210]))]
    (rgb-escape {:rgbv color :text (str (char 0x2588) (char 0x2588))})))

(defn- get-max-width
  "Isn't happy when not run from a terminal but get columns of window."
  []
  (let [dimensions (:out (shell {:out :string :continue :true} "stty" "size"))
        width (if (= "" dimensions) 80 (last (re-seq #"\d+" dimensions)))]
    (dec (/ (Integer. width) 2))))

(defn canvas->term-color
  "Convert the canvas into a color-escaped string for printing."
  [canvas]
  (let [array (mapv #(mapv codel->block-print %) canvas)
        transposed (apply mapv vector array)
        max-width (get-max-width)
        warning "(Program may be cut off due to window size)"
        rows (cond->> (mapv #(reduce str (take max-width %)) transposed)
               (< max-width (count (first transposed))) (cons warning))]
    (str \newline (s/join \newline rows) \newline)))

;; ========== MISC =============================================================
(defn command-name
  "TBD how to notify about the running command but this gets its name."
  [command]
  (second (re-find #".*\$(.+)@.*" (str command))))
