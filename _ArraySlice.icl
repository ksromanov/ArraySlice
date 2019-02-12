// Draft version of Array Slices (inspired by ARRAY_SLICE from SML)
implementation module _ArraySlice
import StdArray, StdInt, _SystemArray, StdMisc, StdEnv

// [arr] - underlying array, [shift] - the position of the
// first element of the slice, [size] - number of elements in
// the slice
:: ArraySlice a = {arr :: !.{a}, shift :: !Int, size :: !Int}

// We start indexing from the beginning of the slice.
// Same behavior in R, SML and probably everywhere
instance Array ArraySlice a where
	select {arr, shift, size} i = select arr (i + shift)

	uselect slice=:{arr, shift, size = sz} i
        | i < 0   = abort "Attempt to uselect before beginning of slice"
        | i >= sz = abort "Attempt to uselect after end of slice"
        | otherwise = (el, {slice & arr=arr`})
            where (el, arr`) = uselect arr (i + shift)

	size {arr, shift, size}  = size

	usize slice=:{size = sz} = (sz, slice)

	update slice=:{arr, shift, size = sz} i el
        | i < 0   = abort "Attempt to update before beginning of slice"
        | i >= sz = abort "Attempt to update after end of slice"
        | otherwise = { slice & arr = arr`}
            where arr` = update arr (i + shift) el

	createArray size el = {arr = (createArray size el), shift = 0, size}

	replace slice=:{arr, shift, size = sz} i el
        | i < 0   = abort "Attempt to replace before beginning of slice"
        | i >= sz = abort "Attempt to replace after end of slice"
        | otherwise = (el`, { slice & arr = arr`})
            where (el`, arr`) = replace arr (i + shift) el

    _createArray size = {arr = (_createArray size), shift = 0, size}

fromArray :: !u:{a} -> v:(ArraySlice a), [u <= v]
fromArray arr = {arr = arr`, shift = 0, size = size}
    where (size, arr`) = usize arr

toArray :: !u:(ArraySlice a) -> v:{a}, [u <= v]
toArray {arr, shift, size}
    | shift == 0 = arr
    | otherwise  = abort "Slice does not cover whole array"

split :: !*(ArraySlice a) !Int -> *(*(ArraySlice a), *(ArraySlice a))
split {arr, shift, size} j
    | j >= size = abort "Attempt to split after slice end"
    | j <= 0    = abort "Attempt to split before slice beginning"

    | otherwise = ({arr = arr`,  shift,             size = j},
                   {arr = arr``, shift = shift + j, size = size - j})
        where (arr`, arr``) = unsafeArrayDup arr
              // Helper function, which overcomes uniqueness restrictions
              unsafeArrayDup :: u:{a} -> u:(u:{a}, u:{a})
              unsafeArrayDup x = code {
                      push_a 0
                  }

merge :: !*(ArraySlice a) !*(ArraySlice a) -> *(ArraySlice a)
merge {arr, shift, size} {arr = arr`, shift = shift`, size = size`}
    | not (same arr arr`)      = abort "Attempt to merge slice of different arrays"
    | (shift + size) <> shift` = abort "Attempt to merge not adjacent slices"

    | otherwise = {arr = arr, shift = shift, size = size + size`}
    where
        same :: .a .a -> Bool
        same _ _ = code {
            push_a_b 0
            push_a_b 1
            pop_a 2
            eqI
        }
