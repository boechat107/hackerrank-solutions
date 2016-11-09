(defn simple-median
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
