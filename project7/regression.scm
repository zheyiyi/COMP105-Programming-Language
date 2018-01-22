;; Literal test

(check-type 3 bool)
(check-type # bool)
(check-type 'hello sym)

;; IF test

(check-type-error (if a 1 2))
(check-type-error (if 1 2 3))
(check-type-error (if #t a 1))

;; val
(check-type-error b)
(val a 1)
(check-type a int)


