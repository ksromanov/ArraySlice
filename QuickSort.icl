/// Two implementations of Quicksort algorithm.
/// First - on arrays, second - on slices
implementation module QuickSort
import StdClass, StdInt, StdArray, StdTuple

import _ArraySlice

// Swaps two elements of mutable array or slice
swap:: !*(a e) !Int !Int -> *(a e) | Array a e
swap arr=:{[i]=ai,[j]=aj} i j =
                {arr & [i]=aj,[j]=ai}

// Quicksort on Arrays
partition :: !*(a e) !Int !Int -> (Int, *(a e)) | Array a e & Ord e
partition arr lo hi = go (uselect arr hi) lo lo
    where go (pivot, arr) i j
            | j >= hi   = (i, swap arr i hi)

            | a_j < pivot = go (pivot, swap arr` i j) (i + 1) (j + 1)
            | otherwise = go (pivot, arr`) i (j + 1)
                where (a_j, arr`) = uselect arr j

quicksort :: !*(a e) !Int !Int -> *(a e) | Array a e & Ord e
quicksort arr lo hi
            | lo >= hi  = arr
            | otherwise = quicksort arr`` p hi
                          where arr``     = quicksort arr` lo (p - 1)
                                (p, arr`) = partition arr  lo hi

// Quicksort on Array slices
partition` :: !*(ArraySlice e) -> *(*(ArraySlice e), *(ArraySlice e)) | Array ArraySlice e & Ord e
partition` slice = (uncurry split) (go (uselect slice` 0) 0 0)
    where (size, slice`) = usize slice

          go (pivot, slice) i j
            | j == (size - 1) = (swap slice i j, i + 1)

            | a_j < pivot = go (pivot, swap slice` i j) (i + 1) (j + 1)
            | otherwise   = go (pivot, slice`) i (j + 1)
                where (a_j, slice`) = uselect slice j

quicksort` :: !*(ArraySlice e) -> *(ArraySlice e) | Ord e & Array ArraySlice e
quicksort` slice
            | sz <= 1 = slice`
            | otherwise = let (sliceA, sliceB) = partition` slice` in
                          merge (quicksort` sliceA) (quicksort` sliceB)
            where (sz, slice`) = usize slice
