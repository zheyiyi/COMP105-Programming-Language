; Template for SAT Solver Test Cases 

; This is a case with all conditions
(val f1 '(and (or x (not y)) (not z)))
(val s1 '((x #t) (z #t)))

; This is complicated case with repleting not
(val f6 '(not (or (not (not (not x))) (or y (and (or x y) z)))))
(val s6 '((x #t) (y #f) (z #f)))

; This is complicated no solution case
(val f9 '(and (and (not x) (not y) (not z)) (and x y z)))
(val s9 'fail)
             

