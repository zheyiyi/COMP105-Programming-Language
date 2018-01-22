(* Name: Zheyi Yi
 * TIme: 31h 
 * No collaborator *) 


(* Problem A 
 *
 * null given by a list returns true if the list is empty
 * otherwise, returns false *)


fun null []      = true
  | null (x::xs) = false

val null: 'a list -> bool =null


(* Problem B 
 *
 * sameLength given by two lists xs and ys returns true if two 
 * lists are the same length or both of them are empty, otherwise 
 * returns false *)
 

fun sameLength ([],[])       = true
  | sameLength (_::xs,_::ys) = sameLength (xs,ys) 
  | sameLength (_,_)         = false

val sameLength : 'a list * 'b list -> bool = sameLength


(* Problem C
 * 
 * fib n given by an integer returns the nth Fibonacci number *)


fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)

val fib : int -> int = fib
 

(* Problem D 
 *
 * firstVowel xs given a list of lower-case letters returns true   
 * if the first character is a vowel, otherwise returns false *)


fun firstVowel (x::xs) = Char.contains "aeiou" x
  | firstVowel  _      = false; 

val firstVowel : char list -> bool = firstVowel


(* Problem E *)

(* 1 
 * rev xs given a list xs returns reverse list of the xs *)


fun rev xs = foldl (op ::) [] xs

val rev : 'a list -> 'a list = rev


(* 2 
 * minlist given by a integer list returns the smallest element of a 
 * non-empty list of integer *)


exception Match;

fun minlist nil       = raise Match
  | minlist (x :: xs) = foldl (fn (x,y) => 
                               if (x < y) then x else y) x (x::xs)

val minlist : int list -> int = minlist  
 

(* Problem F  
 *
 * foldl take each element from begining of the list as one parameter of 
 * the function f recursively and foldr take each element from the last 
 * of the list but do the same function *)

fun foldl _ zero        [] = zero
  | foldl f zero (x :: xs) = foldl f (f (x, zero)) xs

val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = foldl

fun foldr _ zero        [] = zero
  | foldr f zero (x :: xs) = f (x, foldr f zero xs)
 
val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b = foldr


(* Problem G
 * 
 * zip given a pair of lists (of equal length) returns equivalent list 
 * of pairs, otherwise returns raise the exception Mismatch *)

exception Mismatch;

fun zip ([],    [])    = []
  | zip (x::xs, y::ys) = (x,y) :: zip (xs,ys)
  | zip _              = raise Mismatch;

val zip : 'a list * 'b list -> ('a * 'b) list = zip


(* Problem H 
 *
 * pairfoldr applies a three-argument function to a pair of lists of 
 * equal length,using the same order as foldr *)

fun pairfoldr f z ((x::xs), (y::ys)) = f (x, y, pairfoldr f z (xs, ys))
  | pairfoldr f z (_,  _) = z;

fun zip2 (xs, ys) = pairfoldr (fn (x, y, z) => (x, y)::z) [] (xs, ys)

val pairfoldr : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c = 
                                                               pairfoldr
(* Problem I 
 * unzip given a list of pairs returns a pair of lists *)

fun unzip pairs = 
  let 
      fun split ([], xs, ys)             = (rev xs, rev ys)
        | split ((x,y)::pairs, xs, ys)   = split (pairs, x::xs, y::ys)
  in   split (pairs, [], [])
  end

val unzip : ('a * 'b) list -> 'a list * 'b list = unzip


(* Problem J 
 * concat given a list of lists returns a single list containing all 
 * the elements in the correct order  *)


fun concat [] = []   
  | concat ((x::xs)::ys) = x :: concat (xs :: ys)

val concat : 'a list list -> 'a list = concat


(* Problem K *)

(* 1 algebraic laws for compound:
 * compound f 0 x = x
 * compound f n x = f(x, compound f (n-1) x) *)


(* 2 compound f n x given by a function of type 'a * 'a -> 'a,
 * which take two arguments of the same type returns a result also 
 * that type  *)


fun compound f 0 x = x
  | compound f n x = f(x, compound f (n-1) x)

val compound : ('a * 'a -> 'a) -> int -> 'a -> 'a = compound


(* 3 exp x y given two integer x and y returns the x exponent of y *)


fun exp _ 0 = 1
  | exp x y = compound op * (y-1) x

val exp : int -> int -> int = exp


(* Problem L
 *  
 * nth n given a integer and a list returns the nth of a list if the 
 * arguments given is defined otherwise returns raise IndexOutOfBounds *)


exception IndexOutOfBounds

fun nth 0 (x :: xs) = x
  | nth n (x :: xs) = nth (n-1) xs
  | nth _ []        = raise IndexOutOfBounds;

val nth : int -> 'a list -> 'a = nth


(* probelm N *)

datatype 'a tree = NODE of 'a tree * 'a * 'a tree 
                 | LEAF

fun cmp (x,y) = if x < y then LESS
                else if x = y then EQUAL
                else GREATER

fun insert cmp =
    let fun ins (x, LEAF) = NODE (LEAF, x, LEAF)
          | ins (x, NODE (left, y, right)) = 
              (case cmp (x, y)
                of LESS    => NODE (ins (x, left), y, right)
                 | GREATER => NODE (left, y, ins (x, right))
                 | EQUAL   => NODE (left, x, right))
    in  ins
    end

datatype 'a set = SET of ('a * 'a -> order) * 'a tree
fun nullset cmp = SET (cmp, LEAF)

(* addelt add an element to a set*)

fun addelt (x, SET (cmp,y)) = SET (cmp, insert cmp (x, y))

val addelt : 'a * 'a set -> 'a set = addelt

 
(* treeFoldr folds a functin over every element of a tree, rightmost 
 * element first *)

fun treeFoldr f x LEAF = x 
  | treeFoldr f x (NODE(left, y, right)) = 
                    f (y, treeFoldr f (treeFoldr f x right) left)    

val treeFoldr : ('a * 'b -> 'b) -> 'b -> 'a tree -> 'b = treeFoldr


(* setFoldr visit every element of the set exactly once, in an 
 * unspecified order *)

fun setFold f x (SET (cmp, LEAF)) = x
  | setFold f x (SET (cmp, NODE(left,y,right))) = 
                    treeFoldr f x (NODE(left,y,right))

val setFold : ('a * 'b -> 'b) -> 'b -> 'a set -> 'b = setFold


(* Problem O *)

(* 1 
 * 'a seq corresponds to three definition of sequences *)

datatype 'a seq = EMPTY
                | SINGLE of 'a
                | APPEND of 'a seq * 'a seq;


(* 2 
 * scons: adds a single element of x to front of sequence *)

fun scons (x, sq) = APPEND (SINGLE x, sq);

val scons : 'a * 'a seq -> 'a seq = scons


(* 3 
 * ssnoc: adds a single elemnt of x to end of sequence *)

fun ssnoc (x, sq) = APPEND (sq, SINGLE x);

val ssnoc : 'a * 'a seq -> 'a seq = ssnoc


(* 4 
 * sappend: appends two sequences *)

fun sappend (sq1, sq2) = APPEND (sq1, sq2);

val sappend : 'a seq * 'a seq -> 'a seq = sappend


(* 5 
 * listOfSeq seq converts a sequence into a list containing the same 
 * elements in the same order*)

fun listOfSeq seq =
  let fun listConvertion (EMPTY,           zs) = zs
        | listConvertion (SINGLE x,        zs) = x :: zs
        | listConvertion (APPEND (xs, ys), zs) =
            listConvertion (xs, listConvertion (ys, zs))
  in  listConvertion (seq, [])
  end;

val listOfSeq : 'a seq -> 'a list = listOfSeq


(* 6 
 * seqOfList: given list, uses foldr to generate sequence *)

fun seqOfList (xs) = foldr scons EMPTY xs;

val seqOfList : 'a list -> 'a seq = seqOfList


(* Problem P *)

type 'a flist = 'a list * 'a list

(* singletonOf returns a sequence containing x whose finger 
 * points at that value *)
fun singletonOf x = ([], [x])

val singletonOf : 'a -> 'a flist = singletonOf

(* atFinger returns the value that the finger points at *)
exception Not_Found
fun atFinger (_, [])     = raise Not_Found
  | atFinger (_, y::ys)  = y

val atFinger : 'a flist -> 'a = atFinger

(* returns 'a flist in which the finger is moved one elem left*)
exception OutBound
fun fingerLeft ([], _)             = raise OutBound
  | fingerLeft (head::left, right) = (left, head :: right)

val fingerLeft  : 'a flist -> 'a flist = fingerLeft

(* returns 'a flist in which the finger is moved one elem left*)
fun fingerRight (_, [])             = raise OutBound
  | fingerRight (left, head::right) = (head :: left, right)
val fingerRight : 'a flist -> 'a flist = fingerRight

(*deletes the elem left of the finger and returns 'a flist *)
fun deleteLeft ([], _):'a flist    = raise OutBound
  | deleteLeft (head::left, right) = (left,right)

val deleteLeft  : 'a flist -> 'a flist = deleteLeft

(* deletes the elem right of the finger and returns 'a flist *)
fun deleteRight (left, head::(delete::right)) = (left, head :: right)
  | deleteRight (_, _)                           = raise OutBound

val deleteRight : 'a flist -> 'a flist = deleteRight

(* inserts an elem x left of the finger and returns 'a flist *)
fun insertLeft (x, (left,right)) = (x :: left, right)

val insertLeft  : 'a * 'a flist -> 'a flist = insertLeft

(* inserts an elem x right of the finger and returns 'a flist *)
fun insertRight (x, (left, r::right)) = (left, r::(x::right))
  | insertRight (x, (left, []))       = (left, [x])

val insertRight : 'a * 'a flist -> 'a flist = insertRight

(* these are the traditional fold functions except modified to
work with 'a flist *)
fun ffoldl operator basis (xs,ys) = foldl operator basis (List.rev xs @ ys)
fun ffoldr operator basis (xs,ys) = foldr operator basis (List.rev xs @ ys)


val ffoldl : ('a * 'b -> 'b) -> 'b -> 'a flist -> 'b = ffoldl
val ffoldr : ('a * 'b -> 'b) -> 'b -> 'a flist -> 'b = ffoldr
 
