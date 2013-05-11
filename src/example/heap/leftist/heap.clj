(ns example.heap.leftist.heap)

(defprotocol MergeableHeap
  (heap-empty? [this]
    "checks if heap is empty")
  (heap-get-min [this]
    "returns and not remove minimum element")
  (heap-merge [this other-heap]
    "merge with another heap")
  (heap-insert [this key value]
    "insert element with given priority (key) and given value")
  (heap-extract-min [this]
    "returns and remove minimum element")
  (heap-item-priority [this heap-item]
    "returns the priority for given element")
  (heap-item-value [this heap-item]
    "returns value for given element"))

(defprotocol HeapWithRank
  "returns rank for the heap"
  (heap-rank [this]))
