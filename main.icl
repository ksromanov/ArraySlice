module main
import StdEnv, StdArray, _SystemArray
import _ArraySlice

/*
Start :: Bool //({Int}, {Int})
Start = c.[0] == e.[0] //(a, a)
    where a = { 1, 2, 3}
          (c, d) = asplit a
          (_, e) = replace d 0 4
*/
Start :: String
Start = "Hello!" +++ m +++ "8"
    where i = fromArray {"i", "I"}
          (a, b) = splitSlice i 1
          j = mergeSlices a b
          m = select j 0
