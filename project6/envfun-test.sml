val lookup  : string * 'a env -> 'a = lookup
val bindVar : string * 'a * 'a env -> 'a env = bindVar
val emptyEnv : 'a env = emptyEnv

val emptyEnv : string -> 'a = emptyEnv
