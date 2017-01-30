(let [n (Integer/parseInt (read-line))
      ;; Input data.
      data (map (fn [_] (Integer/parseInt (read-line)))
                (range n))
      ;; Returns the parent index of a given index.
      parent-idx (fn [i]
                   (-> (dec i) (/ 2) Math/floor int))
      ;; Insert elements into the min-heap.
      h-insert (fn [heap x]
                 (let [go (fn [h i]
                            (let [p-idx (parent-idx i)
                                  parent (get h p-idx)]
                              (if (or (zero? i) (>= x parent))
                                (assoc h i x)
                                (recur (assoc h, p-idx x, i parent)
                                       p-idx))))]
                   (go heap (count heap))))
      ;; Left and right children of an index.
      l-idx #(inc (* 2 %))
      r-idx #(+ 2 (* 2 %))
      ;; Returns the new heap after the min extraction.
      h-pop (fn [heap]
              (if (== 1 (count heap))
                []
                (let [x (peek heap)
                      go (fn [h i]
                           (let [li (l-idx i), ri (r-idx i)
                                 lx (get h li), rx (get h ri)]
                             (cond
                               ;; We keep the moving element as the parent of
                               ;; this sub-heap.
                               (or (and (nil? lx) (nil? rx))
                                   (and (nil? rx) (<= x lx))
                                   (and (<= x lx) (<= x rx)))
                               (assoc h i x)
                               ;; The element should be replaced by its left
                               ;; child.
                               (or (nil? rx) (<= lx rx))
                               (recur (assoc h, i lx, li x) li)
                               ;; Replaced by its right child.
                               :else
                               (recur (assoc h, i rx, ri x) ri))))]
                  (go (pop heap) 0))))
      ;; Takes n elements from the min-heap, returning a vector.
      h-take (fn [n heap]
               (first
                (reduce (fn [[ret h] _] [(conj ret (first h)) (h-pop h)])
                        [[] heap]
                        (range n))))
      ;; [Int] -> Double
      ;; Returns the median of a heap.
      h-median (fn [heap]
                 (double
                  (if (even? (count heap))
                    (->> (h-take (-> (count heap) (/ 2) inc) heap)
                         reverse
                         (take 2)
                         (reduce + 0)
                         (* 0.5))
                    (last
                     (h-take (-> (count heap) inc (/ 2)) heap)))))]
  (reduce (fn [heap x]
            (let [h (h-insert heap x)]
              (println (h-median h))
              h))
          []
          data))
