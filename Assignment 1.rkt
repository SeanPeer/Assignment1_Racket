#lang pl
#|
open-list function is appending each every list recursivly to (from the last to the first)
|#
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list lst)
  (cond [(null? lst) null]
        ;;pair?
        [else (append (first lst) (open-list (rest lst)))]))
;; ToDo TESTS for this functions
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
(test (open-list '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90) ( ))) => '(1 2 3 2 3 3 4 9 2 -1 233 11 90))
#|
min&max function is searching the min and max number by using open-list function for constrain each list
to a one list and then searching the minimum and a maximum
|#

(: min&max :(Listof(Listof Number)) -> (Listof Number))
(define (min&max lst)
  (cond [(null? lst) '(-inf.0 +inf.0)]
        ;;pair?
        [else (list (exact->inexact(helper_min (open-list lst))) (exact->inexact (helper_max (open-list lst))))]
        ))
(: helper_min : (Listof Number) -> Number)
(define (helper_min lst)
  (cond [(null? lst) 0 ]
        [else (min (first lst) (helper_min (rest lst)))]
        ))

(: helper_max : (Listof Number) -> Number)
(define (helper_max lst)
  (cond [(null? lst) 0 ]
        [else (max (first lst) (helper_max (rest lst)))]
        ))
#|
ToDO Test for min&max ***
|#

(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1.2 2 3.5) (2 3 3 4) (9 2 -1.5) (233.3 11 90))) => '(-1.5 233.3))
(test (min&max '()) => '(-inf.0 +inf.0))

#|
ToDO Min&Max_apply Comments
|#

(: min&max_apply :(Listof(Listof Number)) -> (Listof Number))
(define (min&max_apply lst)
  (cond [(null? lst) '(-inf.0 +inf.0)]
        ;;pair?
        [else (list (exact->inexact(toPair_min (open-list lst)))
                    (exact->inexact (toPair_max (open-list lst))))]
        ))
(: toPair_min : (Listof Number) -> Number)
(define (toPair_min lst)
  (cond [(null? lst) 0]
        [else (helper_min_apply (first lst) (toPair_min (rest lst)))]
  
  ))

(: toPair_max : (Listof Number) -> Number)
(define (toPair_max lst)
  (cond [(null? lst) 0]
        [else (helper_max_apply (first lst) (toPair_max (rest lst)))]
  
  ))

(: helper_min_apply : Number Number -> Number)
(define (helper_min_apply x y)
  (cond [(<= x y) x ]
        [else y]
        ))

(: helper_max_apply : Number Number -> Number)
(define (helper_max_apply x y)
  (cond [(<= x y) y ]
        [else x]
        ))
#|
TODO description for apply !!
|#
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '((1.2 2 3.5) (2 3 3 4) (9 2 -1.5) (233.3 11 90) ( ))) => '(-1.5 233.3))
(test (min&max '((1.2 2 3.5) (2 3 3 4) (9 2 -1.5) (233.3 11 90))) => '(-1.5 233.3))
(test (min&max '()) => '(-inf.0 +inf.0))



#|
TODO COMMENTS FOR TABLE
|#

(define-type Table
  [EmptyTbl ]
  [Add Symbol String Table]
  )


(: search-table : Symbol Table -> (U Boolean String))
(define (search-table sym t)
  (cases t
      [(EmptyTbl)#f]
      [(Add sym2 str2 table2) (cond
                                [(equal? sym sym2) str2]
                                [else(search-table sym table2)])]))
    

(: remove-item : Table Symbol -> Table )
(define (remove-item t s)
  (cases t
    [(EmptyTbl) (EmptyTbl)]
    [(Add sym2 str tbl) (cond
                              [(equal? s sym2) tbl ]
                              [else (Add sym2 str (remove-item tbl s))])]))
    
  
(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A"(EmptyTbl))))) => #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => "AAA")
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'a) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b) => (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))
