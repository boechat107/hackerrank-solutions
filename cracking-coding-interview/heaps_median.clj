(defn parent-idx
  "Returns the parent index of a given index."
  [i]
  (-> (dec i) (/ 2) Math/floor int))

(defn h-insert
  "Insert elements into the heap. The first argument says if the heap is a max
  or a min heap."
  [op heap x]
  (let [go (fn [h i]
             (let [p-idx (parent-idx i)
                   parent (get h p-idx)]
               (if (or (zero? i) (op x parent))
                 (assoc h i x)
                 (recur (assoc h, p-idx x, i parent)
                        p-idx))))]
    (go heap (count heap))))

(def hmin-insert (partial h-insert >=))

(def hmax-insert (partial h-insert <=))

;; Left and right children of an index.
(def l-idx #(inc (* 2 %)))

(def r-idx #(+ 2 (* 2 %)))

(defn h-pop
  "Returns a new heap after getting its head."
  [heap]
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

(defn h-take
  "Takes n elements from the min-heap, returning a vector."
  [n heap]
  (first
   (reduce (fn [[ret h] _] [(conj ret (first h)) (h-pop h)])
           [[] heap]
           (range n))))

(defn h-median
  "[Int] -> Double
  Returns the median of a heap."
  [heap]
  (double
   (if (even? (count heap))
     (let [n (-> (count heap) (/ 2) inc)
           sorted (h-take n heap)]
       (-> (sorted (dec n))
           (+ (sorted (- n 2)))
           (/ 2)))
     (let [n (-> (count heap) inc (/ 2))]
       ((h-take n heap) (dec n))))))

(defn print-medians
  [data]
  (reduce (fn [heap x]
            (let [h (hmin-insert heap x)]
              (println (h-median h))
              h))
          []
          data))

(let [n (Integer/parseInt (read-line))
      ;; Input data.
      data (map (fn [_] (Integer/parseInt (read-line)))
                (range n))]
  (print-medians data))
