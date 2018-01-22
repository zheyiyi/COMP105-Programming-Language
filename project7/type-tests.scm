;
;test case 1
;

(check-type 
    (type-lambda ('a)
      (lambda ([xs : (list (list 'a))])
          ((@ car 'a) ((@ car (list 'a)) xs))))
    (forall ('a) ((list (list 'a)) -> 'a)))
;;
;; test case 2
;;

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

;;
;; test case 3
;;

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
