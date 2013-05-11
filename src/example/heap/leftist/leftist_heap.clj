(ns example.heap.leftist.leftist-heap
  (:use example.heap.leftist.heap))

(deftype LeftistHeap [key value rank left right comparer])

(deftype EmptyLeftistHeap [comparer]
  HeapWithRank
  (heap-rank [this] 0)
  
  MergeableHeap
  (heap-empty? [this] true)
  
  (heap-get-min [this]
    (throw (RuntimeException. "Cant retrieve element from empty heap.")))
  
  (heap-merge [this other-heap] other-heap)
  
  (heap-insert [this key value]
    (->LeftistHeap key value 1 this this comparer))
  
  (heap-extract-min [this]
    (throw (RuntimeException. "Cant retrieve element from empty heap.")))
  
  (heap-item-priority [this heap-item]
    (first heap-item))
  
  (heap-item-value [this heap-item]
    (second heap-item)))

(extend-type LeftistHeap
  HeapWithRank
  (heap-rank [this] (.rank this))

  MergeableHeap
  (heap-empty? [this] false)

  (heap-get-min [this] [(.key this)
                        (.value this)])

  (heap-merge [this other-heap]
    (cond
     (heap-empty? this) other-heap
     (heap-empty? other-heap) this
     :else
     (letfn [(ensure-leftist-property [key value heap1 heap2 comparer]
               (let [r1 (heap-rank heap1)
                     r2 (heap-rank heap2)]
                 (if (>= r2 r1)
                   (->LeftistHeap key value (inc r1) heap2 heap1 comparer)
                   (->LeftistHeap key value (inc r2) heap1 heap2 comparer))))]
       (let [this-p (.key this)
             other-m (heap-get-min other-heap)
             other-p (heap-item-priority this other-m)
             cmp (.comparer this)]
         (if (< (cmp this-p other-p) 0)           
           (ensure-leftist-property (.key this)
                                    (.value this)
                                    (.left this)
                                    (heap-merge (.right this) other-heap)
                                    cmp)
           (ensure-leftist-property other-p
                                    (heap-item-value this other-m)
                                    (.left other-heap)
                                    (heap-merge this (.right other-heap))
                                    cmp))))))

  (heap-insert [this key value]
    (let [cmp (.comparer this)]
      (heap-merge this
                  (->LeftistHeap key
                                 value
                                 1
                                 (->EmptyLeftistHeap cmp)
                                 (->EmptyLeftistHeap cmp)
                                 cmp))))

  (heap-extract-min [this]
    (heap-merge (.left this)
                (.right this)))

  (heap-item-priority [this heap-item]
    (first heap-item))

  (heap-item-value [this heap-item]
    (second heap-item)))
