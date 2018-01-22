
;;Name:Zheyi Yi
;;Date due:02/28/2017
;;Comp105 Assignment 5: Continuations
;;File: solution.scm
;;Purpose: to give additional experience with higher order, polymorphic
;;functions and practice using continuations for a backtracking search
;;problem


;;============================================================
;; 
;;Problem Q
;;
;;============================================================
;;
;;
;;(qsort predicate?) given a predicate? and a integer list xs, returns
;;acsending list of xs if predicate is <, otherwise, it returns decsending
;;list of xs, if predicate is >.
;;
;;In my qsort,I use a skill called accumulating parameters, when all the 
;;elements in xs was transferred into tail which is '() in the begining, 
;;at this time, xs is null, so it returns tail by (if (null? xs) tail) 


(val qsort 
  (lambda (predicate?)
    (letrec 
      ((sort 
         (lambda (xs tail)
           (if (null? xs)
             tail
             (let*
               ((part-Right ((curry predicate?) (car xs)))
                (part-Left (o not part-Right)))
               (sort (filter part-Left (cdr xs)) 
                (cons (car xs) (sort (filter part-Right (cdr xs)) tail))))))))
     (lambda (xs) (sort xs '())))))


(check-expect((qsort <) '(7 4 6 3 5 8 10)) '(3 4 5 6 7 8 10))
;tests the case where qsort returns acsending list
(check-expect((qsort <) '(-2 -5 1 8 8 5 7 8 13)) '(-5 -2 1 5 7 8 8 8 13))
;tests the case where qsort returns acsending list with negative numbers 
;and repeting numbers
(check-expect((qsort >) '(2 4 1 3 5 10)) '(10 5 4 3 2 1)) 
;;tests the case where qsort returns decsending list


;;============================================================
;; 
;;Problem F
;;
;;============================================================
;;
;;
;;(formula? xs) returns true if xs is a boolean formulas ruled from
;;exercise 21, otherwise returns false.


(define formula? (xs)
  (letrec 
    ((formula2 
       (lambda (xs)
         (if (null? xs)
           #t
           (if (equal? (car xs) 'not)
             (if (equal? (car (cdr xs)) 'and)
               (formula2 (cdr (cdr xs)))
               (if (equal? (car (cdr xs)) 'or)
                 (formula2 (cdr (cdr xs)))
                 (if (symbol? (car (cdr xs)))
                   (if (null? (cdr (cdr xs)))
                     #t
                     #f)
                   #f)))
             (if (equal? (car xs) 'and)
               (formula2 (cdr xs))
               (if (equal? (car xs) 'or)
                 (formula2 (cdr xs))
                 (if (pair? (car xs))
                   (formula2 (car xs))
                   (if (symbol? (car xs))
                     (formula2 (cdr xs))
                     #f))))))))
     (formula1 
       (lambda (xs)
         (if (null? xs)
           #f
           (if (symbol? xs)
             #t
             (formula2 xs))))))
    (formula1 xs)))


(check-expect (formula? '()) #f)
;tests the case with '()
(check-expect (formula? 'a) #t)
;tests the case with symbol
(check-expect (formula? '(not not)) #t)
;tests the case with (not symbol)
(check-expect (formula? '(not a a)) #f)
;tests the case with (not symbol symbol)
(check-expect (formula? '(and a)) #t)
;tests the case with and case
(check-expect (formula? '(cons 'and '())) #t)
;tests the case where cons and and '()
(check-expect (formula? '(and a b c d e)) #t)
;tests the case with complicated and condition
(check-expect (formula? '(or (a b c d e))) #t)
;tests the case with complicated or condition
(check-expect (formula? '(and (or x y z) (or (not x)(not y) (not z)) 
                           (or x y (not z)))) #t)
;tests the case with complicated composition of and or not 


;;============================================================
;; 
;;Problem 21
;;
;;============================================================
;;
;;(find-formula-true-asst f fail succ) given three parameters
;;f is a formula, fail is a failure continuation function and 
;;succ is a success continuation function, returns t if f  meet
;;the reqiurement of questions 21, otherwise returns #f


(define find-formula-true-asst (f fail succ)
  (letrec
    ((find-all-asst 
       (lambda (formula bool cur fail succeed) 
         (if (null? formula)
           (succeed cur fail)
           (find-formula-asst (car formula) bool cur fail
             (lambda (cur resume)
               (find-all-asst (cdr formula) bool cur resume succeed))))))
     (find-any-asst 
       (lambda (formula bool cur fail succeed) 
         (if (null? formula)
           (fail)
           (find-formula-asst (car formula) bool cur
             (lambda() (find-any-asst (cdr formula) bool cur fail succeed))
             succeed))))
     (find-formula-asst 
       (lambda (formula bool cur fail succeed)
         (if (symbol? formula)
           (if (null? (find formula cur))
             (succeed (bind formula bool cur) fail)
             (if (equal? (find formula cur) bool)
               (succeed cur fail)
               (fail)))
           (if (equal? (car formula) 'and)   
             (if (equal? bool #t)
               (find-all-asst (cdr formula) bool cur fail succeed)
               (find-any-asst (cdr formula) bool cur fail succeed))
             (if (equal? (car formula) 'or)    
               (if (equal? bool #t)
                 (find-any-asst (cdr formula) bool cur fail succeed)
                 (find-all-asst (cdr formula) bool cur fail succeed))
               (if (equal? (car formula) 'not) 
                 (find-formula-asst (cadr formula) (not bool) cur fail succeed)
                 'fail)))))))
    (find-formula-asst f #t '() fail succ)))


(define one-solution (f)
  (find-formula-true-asst f (lambda () 'no-solution)
         (lambda (cur resume) cur)))

(define all-solutions (f)
  (find-formula-true-asst f (lambda () '())
         (lambda (cur resume) (cons cur (resume)))))


(check-expect (procedure? find-formula-true-asst) #t) ; correct name
(check-error (find-formula-true-asst))                ; not 0 arguments
(check-error (find-formula-true-asst 'x))             ; not 1 argument
(check-error (find-formula-true-asst 'x 
                                     (lambda () 'fail)))   ; not 2 args
(check-error(find-formula-true-asst 'x 
                                    (lambda () 'fail) 
                                    (lambda (c r) 'succeed) z)) ; not 4 args

(check-error (find-formula-true-asst 'x 
                                     (lambda () 'fail) 
                                     (lambda () 'succeed)))
;success continuation expects 2 arguments, not 0
(check-error (find-formula-true-asst 'x 
                                     (lambda () 'fail) 
                                     (lambda (_) 'succeed)))
;success continuation expects 2 arguments, not 1
(check-error (find-formula-true-asst '(and x (not x)) 
                                     (lambda (_) 'fail) 
                                     (lambda (_) 'succeed)))
;failure continuation expects 0 arguments, not 1

(check-expect   ; x can be solved
       (find-formula-true-asst 'x 
                               (lambda () 'fail) 
                               (lambda (cur resume) 'succeed))
       'succeed)
(check-expect   ; x is solved by '((x #t))
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       '#t)
(check-expect   ; (not x) can be solved
       (find-formula-true-asst '(not x)
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

(check-expect   ; (not x) is solved by '((x #f))
       (find-formula-true-asst '(not x)
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       '#f)

(check-expect   ; (and x (not x)) cannot be solved
       (find-formula-true-asst '(and x (not x))
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'fail)


(val f1 'x)
(check-expect (one-solution f1) '((x #t)))
;tests the case with single symbol
(val f2 '(and x z))
(check-expect (one-solution f2) '((x #t) (z #t)))
;tests the case with formula with and
(val f3 '(not (or (not (not (not x))) (or y (and (or x y) z)))))
(check-expect (one-solution f3) '((x #t)(y #f)(z #f)))
;tests the case with complicated formula with repeating not
(val f4 '((x) (not x)))
(check-expect (one-solution f4) 'fail)
;tests the case with no possible solution

