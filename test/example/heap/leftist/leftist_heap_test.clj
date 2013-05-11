(ns example.heap.leftist.leftist-heap-test
  (:require [example.heap.leftist.leftist-heap :as lh])
  (:require [clojure.test :as test])
  (:use example.heap.leftist.heap))

(defn test-min-comparer [x y]
  (let [cmp (- x y)]
    (cond
     (< cmp 0) -1
     (> cmp 0) 1
     :else 0)))

(test/deftest empty-heap
  (let [h (lh/->EmptyLeftistHeap test-min-comparer)]
    (test/is (true? (heap-empty? h)))
    (test/is (zero? (heap-rank h)))))

(test/deftest add-to-empty-heap
  (let [h (heap-insert (lh/->EmptyLeftistHeap test-min-comparer) 123 456)
        m (heap-get-min h)
        e (heap-extract-min h)]
    (test/is (= [123 456] m))
    (test/is (= 1 (heap-rank h)))
    (test/is (true? (heap-empty? e)))))

(defn extract-all [heap]
  (loop [result [] h heap]
    (if (heap-empty? h)
      result
      (recur (conj result (heap-get-min h))
             (heap-extract-min h)))))

(defn multiple-insert-test-helper [elements]
  (let [sorted-elements (vec (sort elements))
        heap (reduce #(heap-insert %1 %2 :value)
                     (lh/->EmptyLeftistHeap test-min-comparer)
                     elements)
        from-heap (extract-all heap)
        only-priorities (vec (map #(heap-item-priority heap %) from-heap))]
    (test/is (= sorted-elements only-priorities))))

(test/deftest multiple-insert-into-min-leftist-heap
  (multiple-insert-test-helper [3 2 1])
  (multiple-insert-test-helper [1 1 -1])
  (multiple-insert-test-helper [1 2 3])
  (multiple-insert-test-helper [0 4 2 3 1]))
