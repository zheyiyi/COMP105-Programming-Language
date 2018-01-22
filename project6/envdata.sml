

(* env is a type with (string* 'a) list *)
type 'a env = (string * 'a) list

exception NotFound of string
(* emptyEnv define a empty environment *)
val emptyEnv = []

(* bindVar bind new pair name and value into environment*)
fun bindVar (name, value, env) = (name, value) :: env

(* looup check whether a identifier exists in the environment, 
 * returns its value otherwise returns raise notFound("string") *)
fun lookup (name, [])                  = raise NotFound("string")
  | lookup (name, ((x, y)::xs)) = if name = x then y 
                                         else lookup (name, xs)







