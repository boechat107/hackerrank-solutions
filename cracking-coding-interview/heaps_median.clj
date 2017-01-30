;; ## Heaps: Find the Running Median

;; The solution involves the usage of two heaps at the same time, as described
;; in the reference below.

;; Atkinson, Michael D., et al. "Min-max heaps and generalized priority queues."
;; Communications of the ACM 29.10 (1986): 996-1000. APA

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
  "Returns a new heap after removing the head of the given heap."
  [op heap]
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
                       (and (nil? rx) (op x lx))
                       (and (op x lx) (op x rx)))
                   (assoc h i x)
                   ;; The element should be replaced by its left
                   ;; child.
                   (or (nil? rx) (op lx rx))
                   (recur (assoc h, i lx, li x) li)
                   ;; Replaced by its right child.
                   :else
                   (recur (assoc h, i rx, ri x) ri))))]
      (go (pop heap) 0))))

(def hmin-pop (partial h-pop <=))

(def hmax-pop (partial h-pop >=))

(defn balance-heaps
  "[Int], [Int] -> [[Int] [Int]]
  Moves heap heads from one heap to the other if the difference of heap sizes
  are greater than 1."
  [max-heap min-heap]
  (let [max-cnt (count max-heap)
        min-cnt (count min-heap)]
    (cond
      (<= (Math/abs (- max-cnt min-cnt)) 1)
      [max-heap min-heap]
      ;;
      (> max-cnt min-cnt)
      [(hmax-pop max-heap) (hmin-insert min-heap (max-heap 0))]
      ;;
      :else
      [(hmax-insert max-heap (min-heap 0)) (hmin-pop min-heap)])))

(defn median
  "[Int], [Int] -> Number
  Returns the median of the two given heaps."
  [max-heap min-heap]
  (cond
    (== (count max-heap) (count min-heap))
    (-> (max-heap 0)
        (+ (min-heap 0))
        (/ 2))
    ;;
    (> (count max-heap) (count min-heap))
    (max-heap 0)
    ;;
    :else (min-heap 0)))

(defn print-medians
  [data]
  (reduce (fn [[max-heap last-median min-heap] x]
            (let [[max-h min-h] (if (> x last-median)
                                  [max-heap (hmin-insert min-heap x)]
                                  [(hmax-insert max-heap x) min-heap])
                  [max-h min-h] (balance-heaps max-h min-h)
                  median (median max-h min-h)]
              (println (double median))
              [max-h median min-h]))
          [[] 0 []]
          data))

(let [n (Integer/parseInt (read-line))
      ;; Input data.
      data (map (fn [_] (Integer/parseInt (read-line)))
                (range n))]
  (print-medians data))
