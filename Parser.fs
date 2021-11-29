module Parser

open FParsec

// natural transformation ParserResult ~> Result
let toResult =
    function
    | Success (p, _, _) -> Result.Ok p
    | Failure _ as err -> Result.Error err

// transformation Result ~> string
let toResultString =
    function
    | Result.Ok p -> p.ToString()
    | Result.Error err -> err.ToString()

let removeSpacesFromString: Parser<string,unit> = manyChars (opt spaces >>. anyChar .>> spaces)

let lineWithoutSpaces line = 
  let result = 
    run removeSpacesFromString line 
    |> toResult
    |> toResultString

  result

// (sepBy (manySatisfy (fun c -> c <> '/')) (pchar '/'))
// manyChars anyChar
// manyChars anyChar) (pchar '/')
let textWithoutSpaces line = 
  let result = 
    run ((skipCharsTillString "&" false 10000) >>. sepBy (manySatisfy (fun c -> c <> '/')) (pchar '/')) line 
    |> toResult
    // |> toResultString

  result