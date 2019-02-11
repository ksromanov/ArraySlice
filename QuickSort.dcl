definition module QuickSort
from StdClass import class Ord, class <
from StdArray import class Array
import _ArraySlice

quicksort :: !*(a e) !Int !Int -> *(a e) | Array a e & Ord e

quicksort` :: !*(ArraySlice e) -> *(ArraySlice e) | Ord e & Array ArraySlice e
