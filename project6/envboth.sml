
(* isBound takes a variable and environment and returns true if
 * the variable in the environment otherwise returns false *)

fun isBound (name, env) = (lookup (name, env); true) 
                            handle NotFound _ => false


(* extendEnv takes a list of variables and list of values and environment
 and adds the corresponding bindings to an environment *) 

exception Match;
fun extendEnv (xs, ys, env) = 
let fun pairfold f z ([], []) = z
      | pairfold f z (x::xs, y::ys) = f (x, y, pairfold f z (xs, ys))
      | pairfold _ _ _ = raise Match;
in
    pairfold bindVar env (xs, ys) 
end;

