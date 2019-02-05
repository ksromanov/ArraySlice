definition module _ArraySlice
import StdArray

:: ArraySlice a = {arr :: !.{a}, shift :: !Int, size :: !Int}

instance Array ArraySlice a

fromArray :: !u:{a} -> v:(ArraySlice a), [u <= v]
toArray :: !u:(ArraySlice a) -> v:{a}, [u <= v]

split :: !*(ArraySlice a) !Int -> *(*(ArraySlice a), *(ArraySlice a))
merge :: !*(ArraySlice a) !*(ArraySlice a) -> *(ArraySlice a)
