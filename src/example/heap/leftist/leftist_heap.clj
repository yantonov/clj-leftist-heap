(ns example.heap.leftist.leftist-heap
  (:use example.heap.leftist.heap))

(deftype LeftistHeap [key value rank left right comparer])

(extend-type LeftistHeap
  HeapWithRank
  (heap-rank [this] (.rank this))

  MergeableHeap
  (heap-empty? [this] (= 0 (.rank this)))

  (heap-get-min [this] [(.key this)
                        (.value this)])

  (heap-merge [this other-heap]
    (cond
      (heap-empty? this) other-heap
      (heap-empty? other-heap) this
      :else
      (letfn [(ensure-leftist-property [key value left-heap right-heap comparer]
                (let [left-rank (heap-rank left-heap)
                      right-rank (heap-rank right-heap)]
                  (if (>= right-rank left-rank)
                    (->LeftistHeap key value (inc left-rank) right-heap left-heap comparer)
                    (->LeftistHeap key value (inc right-rank) left-heap right-heap comparer))))]
        (let [this-p (.key this)
              other-min (heap-get-min other-heap)
              other-p (heap-item-priority other-heap other-min)
              cmp (.comparer this)]
          (if (< (cmp this-p other-p) 0)
            (ensure-leftist-property this-p
                                     (.value this)
                                     (.left this)
                                     (heap-merge (.right this) other-heap)
                                     cmp)
            (ensure-leftist-property other-p
                                     (heap-item-value other-heap other-min)
                                     (.left other-heap)
                                     (heap-merge this (.right other-heap))
                                     cmp))))))

  (heap-insert [this key value]
    (let [cmp (.comparer this)
          emptyHeap (->LeftistHeap nil nil 0 nil nil cmp)]
      (heap-merge this
                  (->LeftistHeap key
                                 value
                                 1
                                 emptyHeap
                                 emptyHeap
                                 cmp))))

  (heap-extract-min [this]
    (heap-merge (.left this)
                (.right this)))

  (heap-item-priority [_ heap-item]
    (first heap-item))

  (heap-item-value [_ heap-item]
    (second heap-item)))

(defn createEmptyHeap [comparer]
  (->LeftistHeap nil nil 0 nil nil comparer))
