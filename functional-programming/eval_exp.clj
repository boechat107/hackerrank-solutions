(let [n (Integer/parseInt (read-line))
      data (map (fn [_] (Float/parseFloat (read-line)))
                (range n))
      factorial (fn [x] (reduce * 1 (range 1 (inc x))))]
  (doseq [y (map (fn [x]
                   ;; For each input number, this function returns exp(x).
                   (reduce #(+ %1
                               (/ (Math/pow x %2)
                                  (factorial %2)))
                           1 ; The first term.
                           ;; Only the first 10 terms.
                           (range 1 10)))
                 data)]
    (println y)))
