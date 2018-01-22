

exception NotFound of string

(* env is a type with (string* 'a)) list *)
type 'a env = string -> 'a

(* emptyEnv return raise NotFound("string") *)
fun emptyEnv x = raise NotFound x

(* bindVar bind new value to the function rho *)
fun bindVar (name, value, rho) = 
              (fn z => if z = name then value else rho z)

(* looup check whether a identifier exists in the environment,
 * returns true if exists otherwise false *)
fun lookup (name, rho) = rho name











