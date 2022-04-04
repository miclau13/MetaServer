module InterpretationProgram

open Instruction

module AsyncResult =
  let asyncResultReturn (x: 'a) : Async<Result<'a, 'b>> = async { return Ok x }
  let asyncReturn (x: Async<'a>) : Async<Result<'a, exn>> =
    let choiceAsync = Async.Catch(x)
    async {
      let! choice = choiceAsync
      match choice with
      | Choice1Of2 x' -> return Ok x'
      | Choice2Of2 exn -> return Error exn
    }
  
  let map (f: 'a -> 'b) (asyncResult: Async<Result<'a, 'c>>) : Async<Result<'b, 'c>> =
    async {
      let! result = asyncResult
      match result with
      | Ok x ->
        return Ok (f x)
      | Error x -> return Error x
    }
  
  let bind (f: 'a -> Async<Result<'b, 'c>>) (asyncResult: Async<Result<'a, 'c>>) : Async<Result<'b, 'c>> = 
    async {
      let! result = asyncResult
      match result with
      | Ok x -> return! f x
      | Error x -> return Error x 
    }

  let bindAsync (f: 'a -> Async<Result<'b, exn>>) (async: Async<'a>) : Async<Result<'b, exn>> = 
    bind f (asyncReturn async)
 
type AsyncResultBuilder() =
  member _.Return(x) = AsyncResult.asyncResultReturn x
  member _.ReturnFrom x = x
  member _.Bind(x,f) = AsyncResult.bind f x
  member _.Bind(x,f) = AsyncResult.bindAsync f x
  member _.Map(x, f) = AsyncResult.map f x

let asyncResult = AsyncResultBuilder()

type Program<'a> =
  | Instruction of IInstruction<Program<'a>>
//  | NotYetDone of (unit -> Program<'a>)
  | Stop of 'a
  
let rec bind f program =
  match program with
  | Instruction inst ->
      inst.Map (bind f) |> Instruction
//  | NotYetDone work ->
//      (fun () -> bind f (work()) ) |> NotYetDone
  | Stop x ->
      f x
let combine expr1 expr2 =
      expr1 |> bind (fun () -> expr2)

//let delay func = NotYetDone (fun () -> func())

/// Return the final value
let result value = Stop value

/// The catch for the computations. Stitch try/with throughout
/// the computation, and return the overall result as an OkOrException.
let rec catch expr =
    match expr with
    | Instruction inst ->
      let res = try Ok(Instruction inst) with | exn -> exn |> result |> Error
      match res with
      | Ok cont ->
        result (Ok cont)
      | Error exn ->
        result (Error exn)
//    | NotYetDone work ->
//        NotYetDone (fun () ->
//            let res = try Ok(work()) with | exn -> exn |> result |> Error
//            match res with
//            | Ok cont -> catch cont 
//            | Error exn -> result (Error exn))
    | Stop _ ->
      result (Ok expr)
    
/// The tryWith operator.
/// This is boilerplate in terms of "result", "catch", and "bind".
let tryWith exn handler =
    catch exn
    |> bind (function Ok value -> value | Error exn -> handler exn)
      
type ProgramBuilder() =
  member _.Return(x) = Stop x
  member _.ReturnFrom(x) = x
  member _.Bind(x,f) = bind f x
  member _.Zero() = Stop ()
  
//  member x.Combine(expr1, expr2) = combine expr1 expr2
  
//  member x.Delay(func) = delay func
//  member x.TryWith(expr, handler) = tryWith expr handler

//// the builder instance
let program = ProgramBuilder()
