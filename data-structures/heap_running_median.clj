;; Reference:
;; https://en.wikipedia.org/wiki/Binary_heap
;;
;; "It is possible to modify the heap structure to allow extraction of both the
;; smallest and largest element in {\displaystyle O} O {\displaystyle (\log n)}
;; (\log n) time.[11] To do this, the rows alternate between min heap and max
;; heap. The algorithms are roughly the same, but, in each step, one must
;; consider the alternating rows with alternating comparisons. The performance
;; is roughly the same as a normal single direction heap. This idea can be
;; generalised to a min-max-median heap."

(defn simple-median
  "Simple implementation using 'sort' multiple times."
  [xs]
  (let [drop-num (fn [xs] (let [n (count xs)]
                            (if (< n 3) 0 (-> (count xs)
                                              (/ 2)
                                              Math/ceil
                                              dec))))
        even (even? (count xs))
        mxs (->> (sort xs)
                 (drop (drop-num xs)))]
    (if even
      (/ (+ (first mxs) (second mxs)) 2)
      (first mxs))))

(let [read-int (fn [& _] (Integer/parseInt (read-line)))]
  (->> (read-int)
       range
       (map read-int)
       (reduce (fn [stream x]
                 (let [xs (conj stream x)]
                   (->> (median xs)
                        float
                        (format "%.1f")
                        println)
                   xs))
               nil)))
