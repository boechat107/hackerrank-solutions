(require '[clojure.string :as s])

(let [_ (Integer/parseInt (read-line))  ; not used
      ;; Input data sequence.
      data (map #(Integer/parseInt %)
                (s/split (read-line) #" "))
      ;; Recursive function to split the data.
      partitioner (fn partitioner
                    ([xs] (partitioner (first xs) nil nil nil xs))
                    ([p left equal right xs]
                     (let [x (first xs)]
                       (cond
                         ;; We finished the input sequence.
                         (not x) (concat left equal right)
                         (== x p) (recur p left (conj equal x) right (rest xs))
                         (< x p) (recur p (conj left x) equal right (rest xs))
                         :else (recur p left equal (conj right x) (rest xs))
                         ))))]
  (apply print (partitioner data)))
