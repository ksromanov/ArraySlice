module main
import StdArray, _SystemArray, StdString
import _ArraySlice
import QuickSort

// Simple test of merge/split/quicksort
Start :: String
Start = "Result: '" +++ letter +++ "'"
    where slice = quicksort` (fromArray {"i", "I", "a", "A"})
          (sliceA, sliceB) = split slice 2
          slice` = merge sliceA sliceB
          letter = select slice` 0
