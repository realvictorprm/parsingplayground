#r @"..\packages\FParsec.1.0.4-RC\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.4-RC\lib\net40-client\FParsec.dll"
open FParsec

type Identifier = string

type Signature = 
    | LazySignature of string
    | FullSignature of string * string

    //with static member create (input:string) =
    //    let parts = (input).Split ':'
    //    match parts with
    //    | [| identifier; typ |] -> 

type Expr = 
    | LetBinding of name : string * inner : Expr list
    | Value of value : decimal
    | Function of name : string * params : Signature list * inner : Expr list

let tabStopDistance = 8 // must be a power of 2

module IndentationParserWithoutBacktracking =
    type LastParsedIndentation() =
        [<DefaultValue>]
        val mutable Value: int32
        [<DefaultValue>]
        val mutable EndIndex: int64

    type UserState = 
        {Indentation: int
         // We put LastParsedIndentation into the UserState so that we 
         // can conveniently use a separate instance for each stream.
         // The members of the LastParsedIndentation instance will be mutated
         // directly and hence won't be affected by any stream backtracking. 
         LastParsedIndentation: LastParsedIndentation}
        with
           static member Create() = {Indentation = -1
                                     LastParsedIndentation = LastParsedIndentation(EndIndex = -1L)}

    type CharStream = CharStream<UserState>
    type Parser<'t> = Parser<'t, UserState>

    // If this function is called at the same index in the stream
    // where the function previously stopped, then the previously
    // returned indentation will be returned again. 
    // This way we can avoid backtracking at the end of indented blocks.
    let skipIndentation (stream: CharStream) =    
        let lastParsedIndentation = stream.UserState.LastParsedIndentation
        if lastParsedIndentation.EndIndex = stream.Index then
            lastParsedIndentation.Value
        else
            let mutable indentation = stream.SkipNewlineThenWhitespace(tabStopDistance, false)
            while stream.Peek() = '#' do
                stream.SkipRestOfLine(false) // skip comment
                indentation <- stream.SkipNewlineThenWhitespace(tabStopDistance, false)
            lastParsedIndentation.EndIndex <- stream.Index
            lastParsedIndentation.Value <- indentation
            indentation

    let indentedMany1 (p: Parser<'t>) label : Parser<'t list> =
        fun stream ->
            let oldIndentation = stream.UserState.Indentation
            let indentation = skipIndentation stream
            if indentation <= oldIndentation then 
                Reply(Error, expected (if indentation < 0 then "newline" else "indented " + label))
            else
                stream.UserState <- {stream.UserState with Indentation = indentation}            
                let results = ResizeArray()
                let mutable stateTag = stream.StateTag
                let mutable reply = p stream // parse the first element
                let mutable newIndentation = 0
                while reply.Status = Ok 
                      && (results.Add(reply.Result)
                          newIndentation <- skipIndentation stream
                          newIndentation = indentation)
                   do
                     stateTag <- stream.StateTag
                     reply <- p stream
                if reply.Status = Ok 
                   || (stream.IsEndOfStream && results.Count > 0 && stream.StateTag = stateTag) 
                then
                    if newIndentation < indentation || stream.IsEndOfStream then
                        stream.UserState <- {stream.UserState with Indentation = oldIndentation}
                        Reply(List.ofSeq results)
                    else
                        Reply(Error, messageError "wrong indentation")
                else // p failed
                    Reply(reply.Status, reply.Error) 

open IndentationParserWithoutBacktracking

let isBlank = fun c -> c = ' ' || c = '\t'
let ws1 = skipMany1SatisfyL isBlank "at least one whitespace"
let ws = skipManySatisfy isBlank
//let comment = pstring "#" >>. skipRestOfLine false
//let wsBeforeEOL = skipManySatisfy isBlank >>. optional comment
let identifier = 
    let allowedChars = 
        asciiLetter
    (ws1 >>. manyCharsTill allowedChars ws1 )

let keyword str = pstring str >>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str

let genIdentifier = many1Chars asciiLetter

let betweenRoundBrackets = between (pchar '(') (pchar ')')

let indentedStatements, indentedStatementsRef = createParserForwardedToRef()

//let print = keyword "print" >>. (ws1 >>. identifier .>> wsBeforeEOL |>> Print)
//let loop = keyword "loop" >>. (pipe4 (ws1 >>. identifier) (ws1 >>. pint32) (ws1 >>. pint32 .>> wsBeforeEOL) 
//                                     indentedStatements
//                                     (fun id min max stmts -> Loop(id, min, max, stmts)))

let paramsParser =
    // Parser for parameters (e.g. `let a b c = ...` where b and c are parameters)
    let fullParameter = betweenRoundBrackets (genIdentifier .>> skipChar ':' .>>. genIdentifier)
    let lazyParameter = genIdentifier
    // Parameters must have at least one whitespace between each other
    sepBy ((fullParameter |>> FullSignature) <|> (lazyParameter |>> LazySignature)) (ws1 >>? nextCharSatisfiesNot(fun c -> c = '='))
    // <?> "at least one whitespace is required between parameters"

let letIdentifier = 
    keyword "let" >>. identifier

//let letBinding = 
//    letIdentifier
//    .>>. 
//    (pchar '=' >>. ws >>. indentedStatements) |>> (LetBinding)

let letFunction =
    pipe3 letIdentifier paramsParser (ws >>. pchar '=' >>. ws >>. indentedStatements)
        (fun name params exprs -> 
            if params.IsEmpty then
                LetBinding(name, exprs)
            else
                Function(name, params, exprs))

let valueExpr = pfloat |>> (decimal >> Value)

//let statement = print <|> loop

let exprs = 
    choice [ (*letBinding*)
             letFunction 
             valueExpr ]

do indentedStatementsRef := indentedMany1 exprs "statement" <|> (exprs |>> (fun res -> [ res ]))

let document = indentedStatements .>> spaces .>> eof

let test str =
    match runParserOnString document (UserState.Create()) "" str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test @"
let someFunction parameterA parameterB parameterC:int) = 2.0
let a = 2.1
";;

//test @"
//loop i 1 10
//   loop k 1 10
//    print k
//   print i
//print j

//";