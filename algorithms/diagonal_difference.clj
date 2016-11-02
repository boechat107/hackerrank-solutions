(require '[clojure.string :as s])

(let [ndim (Integer/parseInt (read-line))
      input-mat (map (fn [_] (map #(Integer/parseInt %)
                                  (-> (read-line) (s/split #" "))))
                     (range ndim))
      accumulator (fn accumulator
                    ([xs] (accumulator 0 0 0 xs))
                    ([acc lc cc xs]
                     (cond
                       ;; Final step.
                       (>= lc ndim) acc
                       ;; Next line, first column.
                       (>= cc ndim) (recur acc (inc lc) 0 xs)
                       ;; Accumulate for the primary diagonal.
                       (== lc cc) (recur (+ acc (first xs)) lc (inc cc) (rest xs))
                       :else (recur acc lc (inc cc) (rest xs)))))]
  (-> (-
       ;; Primary diagonal.
       (accumulator (flatten input-mat))
       ;; Secondary diagonal.
       (accumulator (flatten (reverse input-mat))))
      Math/abs
      println))
