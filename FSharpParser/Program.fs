// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FParsec
open System

type Json = JString of string
          | JNumber of float
          | JBool   of bool
          | JNull
          | JList   of Json list
          | JObject of Map<string, Json>
    with 
        override self.ToString () =
            match self with
            | JString s -> s
            | JNumber f -> sprintf "%f" f
            | JBool b -> sprintf "%A" b
            | JNull -> "null"
            | JList l -> sprintf "%A" l
            | JObject m -> sprintf "%A" m

type UserState = unit
type Parser<'t> = Parser<'t, UserState>
let jnull  = stringReturn "null" JNull
let jtrue  = stringReturn "true"  (JBool true)
let jfalse = stringReturn "false" (JBool false)
let jbool = jtrue <|> jfalse
let jnumber = pfloat |>> JNumber

let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (spaces >>. sepBy (pElement .>> spaces) (str "," >>. spaces) |>> f)

let jstring =
    spaces
    >>. skipChar '"'
    >>. many1CharsTillApply anyChar (skipChar '"') (fun res _ -> JString res)
    
let parseJson =
    let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()
    let jobject =
        let keyValue = 
            between (str "\"") (str "\"") (manySatisfy (fun c -> c <> '"'))
            .>>.
            (spaces >>. str ":" >>. spaces >>. jvalue)
        listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)
    do jvalueRef := 
        choice [jobject
                jstring
                jnumber
                jtrue
                jfalse
                jnull]
    spaces
    >>. jvalue
    .>> spaces
    .>> eof


let inline test p str =
    match run p str with
    | Success(result, _, _)   -> 
        printfn "Success: %A" result
        printfn "(obj.ToString: %O)" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let testJson =
    """{
        "a": 123,
        "b": "tree",
        "c": "guguguu"
}"""

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    test pfloat "1.25"
    test jnull "null"
    test jtrue "true"
    test jfalse "false"
    test floatBetweenBrackets "[12323.22323]"
    test jstring "   \"foo bar\"        "
    System.Console.Clear()
    test parseJson testJson

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
