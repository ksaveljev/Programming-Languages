#lang racket

;; I did not write these tests :( shame on me

(require rackunit "hw5.rkt")
(require rackunit/text-ui)

(define hw5-tests
  (test-suite "Tests for HW5"
    (test-equal? "MUPL list -> Racket list"
       (list (int 3) (int 5) (var "hi"))
       (mupllist->racketlist (apair (int 3) (apair (int 5) (apair (var "hi") (aunit))))))
    
    (test-equal? "Racket list -> MUPL list"
       (apair (var "hi") (apair (int 666) (apair (var "test") (aunit))))
       (racketlist->mupllist (list (var "hi") (int 666) (var "test"))))
    
    (test-equal? "Local scoping"
       (int 2)
       (eval-exp (mlet "f1"
                       (fun "f1" "a" (mlet "x" (var "a") (fun "f2" "z" (add (var "x") (int 1)))))
                       (mlet "f3" (fun "f3" "f" (mlet "x" (int 1729) (call (var "f") (aunit))))
                             (call (var "f3") (call (var "f1") (int 1)))))))
    
    (test-equal? "basic-call"
       (int 43)
       (eval-exp (call (fun "incr" "x" (add (var "x") (int 1))) (int 42))))  
  
    (test-equal? "ifgreater with invalid e4"
       (int 0)
       (eval-exp (ifgreater (add (int 2) (int 2)) (add (int 2) (int 1)) (add (int 3) (int -3)) (add "wrong" "bad"))))

    (test-equal? "fst/snd test"
       (apair (int 1) (int 4))
       (eval-exp (apair (fst (apair (int 1) (int 2)))
                        (snd (apair (int 3) (int 4))) )))
    
    (test-equal? "Sum over list"
       (int 6)
       (eval-exp (mlet "fnc"
                       (fun "f1" "x"
                            (ifgreater (isaunit (var "x")) (int 0)
                                       (int 0)
                                       (add (fst (var "x")) (call (var "f1") (snd (var "x"))))))
                       (call (var "fnc") (apair (int 1) (apair (int 2) (apair (int 3) (aunit))))))))

    (test-equal? "ifaunit test #1"
       (int 2)
       (eval-exp (ifaunit (aunit) (int 2) (int 3))))
    
    (test-case "mlet"
      (check-equal? (int 1)
                    (eval-exp (mlet* (cons (cons "x" (int 1)) null) (var "x"))))

      (check-equal? (int 20)
                    (eval-exp (mlet* (list (cons "f" (int 2)) (cons "y" (int 15))) (add (var "f") (add (var "y") (int 3)))))))

    (test-case "ifeq"
      (check-equal? (int 1)
                    (eval-exp (ifeq (int 2) (add (int 1) (int 1)) (int 1) (int 2))))

      (check-equal? (int 2)
                    (eval-exp (ifeq (int 2) (add (int 1) (int 2)) (int 1) (int 2)))))
    
    (test-case "mupl-map"
      (define addtwo (fun "addone" "x" (add (var "x") (int 2))))
      (define mupl-map-addtwo (call mupl-map addtwo))
      (check-equal? (eval-exp (call mupl-map-addtwo (aunit))) (aunit))

      (define my-mupl-list (apair (int 23) (apair (int 42) (aunit))))
      (define my-answers (apair (int 25) (apair (int 44) (aunit))))
      (check-equal? (eval-exp (call mupl-map-addtwo my-mupl-list)) my-answers))
    
    (test-case "mupl-mapAddN"
      (define input (apair (int 25) (apair (int 44) (aunit))))
      (define output (apair (int 26) (apair (int 45) (aunit))))
      (check-equal? (eval-exp (call (call mupl-mapAddN (int 1)) input)) output))

      (check-equal? (eval-exp (call (call mupl-mapAddN (int 7))
                                    (racketlist->mupllist '())))
                    (aunit) "mapAddN empty list")
      (check-equal? (eval-exp (call (call mupl-mapAddN (int 7))
                                    (racketlist->mupllist (list (int 3) (int 4) (int 9)))))
                    (racketlist->mupllist (list (int 10) (int 11) (int 16))) "mapAddN +7")
      (check-equal? (eval-exp (call (call mupl-mapAddN (int 7))
                                    (racketlist->mupllist (list (int 3)))))
                    (racketlist->mupllist (list (int 10))) "mapAddN single item list")
    ))

(run-tests hw5-tests)