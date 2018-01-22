; take 
; return the first n elements of xs, or all of them

(val-rec
  (forall ('a) (int (list 'a) -> (list 'a)))
  take
  (type-lambda ['a]
    (lambda ([n : int] [xs : (list 'a)])
      (if ((@ null? 'a) xs)
        (@ '() 'a)
        (if (> n 0)
          ((@ cons 'a) ((@ car 'a) xs) ((@ take 'a) (- n 1) ((@ cdr 'a) xs)))
          (@ '() 'a))))))

(check-type take (forall ('a) (int (list 'a) -> (list 'a))))

; dropwhile
; return xs without the longest prefix of elements that satisfy p?

(val-rec
  (forall ('a) (('a -> bool) (list 'a) -> (list 'a)))
  dropwhile
  (type-lambda ['a]
    (lambda ([p? : ('a -> bool)] [xs : (list 'a)])
      (if ((@ null? 'a) xs)
        (@ '() 'a)
        (if (p? ((@ car 'a) xs))
          ((@ dropwhile 'a) p? ((@ cdr 'a) xs))
          xs)))))

(check-type dropwhile (forall ('a) (('a -> bool) (list 'a) -> (list 'a))))

