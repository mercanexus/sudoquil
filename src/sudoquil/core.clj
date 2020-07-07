(ns sudoquil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def sudoku [[9 0 0 0 0 0 0 3 0] 
             [0 0 0 0 7 1 0 6 0]
             [0 0 6 8 0 2 5 0 0]
             [0 9 0 0 0 0 0 0 0]
             [0 2 0 6 0 0 9 4 0]
             [0 0 0 0 0 0 0 0 7]
             [0 8 0 0 0 6 0 0 0]
             [0 6 0 0 1 0 8 0 0]
             [0 3 2 0 0 7 0 0 6]])

(defn cell [g v] (let [[r c] v] ((g r) c)))

(defn blanks [g]
  (for [r (range 9) c (range 9) :when (= 0 (cell g [r c]))] [r c]))

(defn row-set [g n] (filter #(> % 0) (g n)))

(defn col-set [g n]
  (filter #(> % 0) (map #(cell g %) (for [r (range 9)] [r n]))))

(defn grid [v]
  (let [[r c] v]
    (cond (and (>= r 0) (< r 3) (>= c 0) (< c 3)) 0
          (and (>= r 0) (< r 3) (>= c 3) (< c 6)) 1
          (and (>= r 0) (< r 3) (>= c 6) (< c 9)) 2
          (and (>= r 3) (< r 6) (>= c 0) (< c 3)) 3
          (and (>= r 3) (< r 6) (>= c 3) (< c 6)) 4
          (and (>= r 3) (< r 6) (>= c 6) (< c 9)) 5
          (and (>= r 6) (< r 9) (>= c 0) (< c 3)) 6
          (and (>= r 6) (< r 9) (>= c 3) (< c 6)) 7 
          (and (>= r 6) (< r 9) (>= c 6) (< c 9)) 8)))

(defn grid-set [g n]
  (filter #(> % 0) (map #(cell g %) (cond (= n 0) (for [r (range 3) c (range 3)] [r c])
                                          (= n 1) (for [r (range 3) c (range 3 6)] [r c])
                                          (= n 2) (for [r (range 3) c (range 6 9)] [r c])
                                          (= n 3) (for [r (range 3 6) c (range 3)] [r c])
                                          (= n 4) (for [r (range 3 6) c (range 3 6)] [r c])
                                          (= n 5) (for [r (range 3 6) c (range 6 9)] [r c])
                                          (= n 6) (for [r (range 6 9) c (range 3)] [r c])
                                          (= n 7) (for [r (range 6 9) c (range 3 6)] [r c])
                                          (= n 8) (for [r (range 6 9) c (range 6 9)] [r c])))))

(defn check-num
  "Return num if number fits; nil otherwise"
  [g v num]
  (let [[r c] v
        blk (grid v)]
    (if (contains? (set (concat (grid-set g blk) (row-set g r) (col-set g c))) num) nil num)))

(defn inc-cell
  "Increment value in cell v till valid then return value; return 0 if there are no valid values."
  [g v]
  (loop [n (cell g v)]
    (if (= n 9) 0
        (let [n' (inc n)]
          (if (check-num g v n') n'
              (recur n'))))))

(defn setup []
  (q/frame-rate 60)
  {:game sudoku
   :blanks (blanks sudoku)
   :i 0})

(defn update-state [state]
  (let [{:keys [game blanks i]} state]
    (if (or (empty? blanks) (= i (count blanks))) state
        (let [n (inc-cell game (nth blanks i))
              g' (assoc-in game (nth blanks i) n)]
          {:game g'
           :blanks blanks
           :i (if (= n 0) (dec i) (inc i))}))))

(defn draw-state [state]
  (q/background 240)
  (q/fill 0)
  (q/text-font (q/create-font "Courier New" 40) 40)
  (doall (map #(q/text (str ((:game state) %1)) 20 %2) (range 9) (range 40 400 40))))

(q/defsketch sudoquil
  :title "Sudoku Solver - Recursive Backtracking Algorithm"
  :size [400 380]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
