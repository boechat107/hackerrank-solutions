(require '[clojure.string :as s])

(let [_ (read-line) ; not used
      data (map #(Integer/parseInt %)
                (s/split (read-line) #" "))
      ;; Function that splits a sequence into three parts: left, pivot and right.
      partitioner (fn partitioner
                    ([xs] (partitioner (first xs) nil nil xs))
                    ([p left right xs]
                     (let [x (first xs)]
                       (cond
                         ;; We finished the input sequence. Reverse is used to
                         ;; keep the original order.
                         (not x) [(reverse left) p (reverse right)]
                         (< x p) (recur p (conj left x) right (rest xs))
                         (> x p) (recur p left (conj right x) (rest xs))
                         ;; Because we have unique numbers, p is going to be
                         ;; added later.
                         :else (recur p left right (rest xs))))))
      ;; Function to apply partitioner recursively on the two partitions
      ;; (left/right) and then join the results.
      div-join (fn div-join [xs]
                 (if (> 2 (count xs))
                   ;; Empty or one element seq doesn't need to be sorted.
                   xs
                   (let [[l p r] (partitioner xs)
                         joined (concat (div-join l) (list p) (div-join r))]
                     (apply println joined)
                     joined)))]
  (div-join data))
