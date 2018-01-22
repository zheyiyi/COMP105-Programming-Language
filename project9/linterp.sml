(***************** Problem 1: Basics of evaluation *****************)

  (* part 1: representation of terms *)

datatype void = VOID of void  (* a type with no values *)

type term = void (* proper definition left as exercise *)

exception LeftAsExercise

val var : string -> term         = fn _ => raise LeftAsExercise
val app : term   -> term -> term = fn _ => raise LeftAsExercise
val lam : string -> term -> term = fn _ => raise LeftAsExercise

val cpsLambda :
    term -> 
    (string -> term -> 'a) -> 
    (term -> term -> 'a) -> 
    (string -> 'a) -> 
    'a
  = fn _ => raise LeftAsExercise

  (* part 2: conversion to uScheme syntax *)

val toString : term -> string = fn _ => raise LeftAsExercise


(***************** Problem 2: substitution *****************)

  (* we suggest a useful auxiliary function, freeVars *)

val freeVars : term -> string list           = fn _ => raise LeftAsExercise
val subst    : string * term -> term -> term = fn _ => raise LeftAsExercise

        
(***************** Problem 3: reductions *****************)

  (* we pretend *every* term is in normal form --- obviously wrong,
     but a useful way to get started *)

exception NormalForm

val reduceN : term -> term = fn _ => raise NormalForm
val reduceA : term -> term = fn _ => raise NormalForm

(* for testing, you may want to change these variables *)

val reducer       = reduceN
val defaultMax = 100000
val maxReductions =
  case OS.Process.getEnv "MAXRED"
    of NONE => defaultMax
     | SOME s => getOpt (Int.fromString s, defaultMax)


(**************************************************)
(*                                                *)
(*       TO COMPLETE THE LAMBDA ASSIGNMENT,       *)
(*         YOU SHOULD NOT NEED TO MODIFY          *)
(*             ANYTHING PAST THIS LINE            *)
(*                                                *)
(**************************************************)

val normalize : term -> term = Lhelp.normalize cpsLambda reducer maxReductions

(**************************************************)
(*                                                *)
(*   PARSING                                      *)
(*                                                *)
(**************************************************)
type name = string
datatype def = VAL    of {reduce:bool} * name * term
             | TERM   of term
             | USE    of name

datatype Token = TokLambda
               | TokDot
               | TokUse of string
               | TokEq
               | TokNoreduce (* the keyword 'noreduce' *)
               | TokVar of string
               | TokOpenParen
               | TokCloseParen
               | TokEOT
               | TokEOF

fun split pred =
    let fun helper acc (x::xs) = 
            if pred x then (rev acc, x::xs) else helper (x::acc) xs  
          | helper acc [] = (rev acc, [])
    in helper [] end

fun tokenize nextline firstline = 
  let fun isFilenameChar #";" = false
        | isFilenameChar c = Char.isGraph c 
      fun isVarChar c = not (Char.contains "\\./();=" c) andalso Char.isGraph c
      fun getfilename s = 
            let val (x, rest)  = split (not o Char.isSpace)   s
                val (n, rest') = split (not o isFilenameChar) rest 
            in  (String.implode n, rest')
            end
      fun helper (acc as (TokEOT::_)) [] = rev acc
        | helper acc [] = helper acc (String.explode (nextline()))
        | helper acc (#"\\"::rest) = helper (TokLambda::acc) rest
        | helper acc (#"."::rest)  = helper (TokDot::acc) rest
        | helper acc (#"/"::(#"/"::rest))  = helper acc []
        | helper acc (#"("::rest)  = helper (TokOpenParen::acc) rest 
        | helper acc (#")"::rest)  = helper (TokCloseParen::acc) rest 
        | helper acc (#";"::rest)  = helper (TokEOT::acc) rest
        | helper acc (#"="::rest)  = helper (TokEq::acc) rest 
        | helper acc (l as (c::rest)) = 
          let val (name, rest') = split (not o isVarChar) l
          in case String.implode name
              of ""    => helper acc rest
               | "use" => let val (fname,rest'') = getfilename rest'
                          in  helper (TokUse fname::acc) rest''
                          end
               | "noreduce" => helper (TokNoreduce::acc) rest'
               | name  => helper (TokVar name::acc) rest'
          end 
  in  helper [] (String.explode firstline)
  end
        
exception SyntaxError of string
                         
fun parseTerm toks =
    let fun appchain NONE term = term
          | appchain (SOME pterm) term = app pterm term
        fun helper prev (TokLambda::(TokVar var)::TokDot::rest) = 
            let val (body, rest') = helper NONE rest
            in (appchain prev (lam var body), rest') end
          | helper prev (TokOpenParen::rest) = 
            let val (term, rest') = helper NONE rest
            in helper (SOME (appchain prev term)) rest' end
          | helper (SOME prev) (TokCloseParen::rest) = (prev, rest)
          | helper prev ((TokVar name)::rest) = 
            helper (SOME (appchain prev (var name))) rest
          | helper (SOME prev) (rest as (TokEOF::_)) = (prev, rest)
          | helper (SOME prev) (TokEOT::rest) = (prev, rest)
          | helper _ (TokNoreduce::_) =
               raise (SyntaxError "'noreduce' is a reserved word")
          | helper _ _ = raise (SyntaxError "Invalid Lambda Expression")
    in helper NONE toks end

fun parseTop (TokUse filename::TokEOT::rest) = USE filename :: parseTop rest
  | parseTop (TokVar name::TokEq::rest) = 
    let val (trm, rest') = parseTerm rest
    in VAL ({reduce=true}, name, trm) :: parseTop rest' end
  | parseTop (TokNoreduce::TokVar name::TokEq::rest) = 
    let val (trm, rest') = parseTerm rest
    in VAL ({reduce=false},name, trm) :: parseTop rest' end
  | parseTop [] = []
  | parseTop toks =
    let val (trm, rest) = parseTerm toks
    in TERM trm :: parseTop rest end
  
(**************************************************)
(*                                                *)
(*   CONVERTING TERMS TO STRINGS                  *)
(*                                                *)
(**************************************************)

val unparse = Lhelp.toString cpsLambda

(**************************************************)
(*                                                *)
(*   ENVIRONMENTS                                 *)
(*                                                *)
(**************************************************)

(* environments 186 *)
type name = string
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and assignment of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find(name, tail)

fun isBound env name = (find (name, env); true) handle NotFound _ => false

(* adding new bindings *)
exception BindListLength
fun bind(name, v, rho) = (name, v) :: rho
fun bindList(n::vars, v::vals, rho) = bindList(vars, vals, bind(n, v, rho))
  | bindList([], [], rho) = rho
  | bindList _ = raise BindListLength
(* type declararations for consistency checking *)
val _ = op emptyEnv : 'a env
val _ = op find     : name * 'a env -> 'a
val _ = op bind     : name      * 'a      * 'a env -> 'a env
val _ = op bindList : name list * 'a list * 'a env -> 'a env


(**************************************************)
(*                                                *)
(*   READERS                                      *)
(*                                                *)
(**************************************************)

exception EOF

type reader = unit -> string (* raises EOF *)

fun filereader fd () = 
  case TextIO.inputLine fd of SOME line => line | NONE => raise EOF

fun stringsreader l = 
  let val buffer = ref l
  in  fn () => case !buffer
                 of [] => raise EOF
                  | h :: t => h before buffer := t
  end              

val _ = op filereader    : TextIO.instream -> reader
val _ = op stringsreader : string list     -> reader

type topreader = { buffer    : def list ref
                 , nextline  : unit -> string
                 , firstline : unit -> string
                 }
fun readtop (r as { buffer, nextline, firstline }) =
  case !buffer
    of h::t => h before buffer := t
     | []   => ( buffer := parseTop (tokenize nextline (firstline()))
               ; readtop r
               )

fun topreader (getline, prompt) = 
  let fun promptIn prompt () = ( TextIO.output(TextIO.stdOut, prompt)
                               ; TextIO.flushOut(TextIO.stdOut)
                               ; getline ()
                               )
  in  if prompt then
        { buffer = ref [], nextline = promptIn "   ", firstline = promptIn "-> "
                                                                               }
      else 
        { buffer = ref [], nextline = getline, firstline = getline }
  end

fun echoTag f x =
  let val line = f x
      val _ = if (String.substring (line, 0, 2) = ";#" handle _ => false) then
                print line
              else
                ()
  in  line
  end

fun echoBuf { buffer=b, nextline=n, firstline=f } =
 { buffer=b, nextline=echoTag n, firstline=echoTag f } 

val topreader = fn args => echoBuf (topreader args)

(**************************************************)
(*                                                *)
(*   IMPLEMENTATION OF [[USE]]                    *)
(*                                                *)
(**************************************************)

fun printerr s = TextIO.output (TextIO.stdErr, s)

(* implementation of [[use]] 193c *)
fun use readEvalPrint filename rho =
      let val fd = TextIO.openIn filename
          fun writeln s = List.app print    [s, "\n"]
          fun errln s   = List.app printerr [s, "\n"]
      in  readEvalPrint (topreader (filereader fd, false), writeln, errln) rho
          before TextIO.closeIn fd
      end 

(**************************************************)
(*                                                *)
(*   EVALUATION                                   *)
(*                                                *)
(**************************************************)

exception FreeVarInLet of name

fun eval t rho set reduce = 
    let fun expand (v, t) =
          if isBound rho v then app (lam v t) (find (v, rho))
          else if not set  then t
          else                  raise (FreeVarInLet v)
        val t = foldr expand t (freeVars t)
    in  if reduce then normalize t else t
    end

val _ = op eval : term -> term env -> bool -> bool -> term

fun evaldef (t, rho, echo) =
  case t
    of USE filename  => use readEvalPrint filename rho
     | VAL ({reduce}, name, t) =>
         let val result = eval t rho true reduce
             handle Lhelp.Diverged =>
                 ( List.app print ["DIVERGENT DEFINITION ", name, "\n"]
                 ; eval t rho true false
                 )
         in  bind (name, result, rho) before echo name
         end
     | TERM e =>
         let val result = eval e rho false true
             handle Lhelp.Diverged =>
                 ( List.app print ["DIVERGENT TERM ", unparse e, "\n"]
                 ; e
                 )
         in  bind ("it", result, rho) before echo (unparse result)
         end

and readEvalPrint (reader, echo, errmsg) rho =
  let fun loop rho =
        let fun continue msg = (errmsg msg; loop rho)
            fun finish () = rho
        in  loop (evaldef (readtop reader, rho, echo))
            handle 
                EOF => finish()
              (* more read-eval-print handlers 194b *)
              | IO.Io {name, ...} => continue ("I/O error: " ^ name)
              | SyntaxError msg   => continue ("syntax error: " ^ msg)
              | FreeVarInLet name => 
                  continue ("error: unbound free variable in RHS of binding: "
                            ^ name)
              | NotFound n        => continue ("variable " ^ n ^ " not found")
        end
  in  loop rho end
val _ = op evaldef : def * term env * (string->unit) -> term env
val _ = op readEvalPrint : topreader * (string->unit) * (string->unit) -> 
                              term env -> term env

fun runInterpreter noisy = 
  let fun writeln s = List.app print    [s, "\n"]
      fun errln   s = List.app printerr [s, "\n"]
      val rc = ref OS.Process.success
      fun asFailure x = (rc := OS.Process.failure; x)
      val _ = ignore (readEvalPrint (topreader (filereader TextIO.stdIn, noisy),
                                     writeln, asFailure o errln) emptyEnv)
              handle EOF => ()
  in  !rc
  end 
val _ = op runInterpreter : bool -> OS.Process.status


(**************************************************)
(*                                                *)
(*   COMMAND LINE                                 *)
(*                                                *)
(**************************************************)

(* command line *)
fun main ["-q"] = runInterpreter false
  | main []     = runInterpreter true
  | main _      =
      ( TextIO.output (TextIO.stdErr, "Usage: " ^ CommandLine.name() ^ " [-q]\n")
      ; OS.Process.failure
      )
val _ = OS.Process.exit (main (CommandLine.arguments()))
