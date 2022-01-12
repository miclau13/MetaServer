module Util

// create an active pattern
open System.Text.RegularExpressions
let (|RegexGroup|_|) pattern (groupIndex: int) input  =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some m.Groups.[groupIndex].Value else None
let (|RegexTitle|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some input else None