module main
import StdArray, _SystemArray, StdString
import _ArraySlice

Start :: String
Start = "Hello!" +++ m +++ "8"
    where i = fromArray {"i", "I"}
          (a, b) = split i 1
          j = merge a b
          m = select j 0
