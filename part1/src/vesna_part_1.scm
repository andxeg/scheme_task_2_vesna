#lang scheme/base
;===================================INPUT=======================================
(define (string-head string1)
  (cond ((> (string-length string1) 0) (substring string1 0 1))
        (else "")))

(define (string-tail string1)
  (substring string1 1 (string-length string1)))

;create list of letters from str
;there is built-in function: (string->list str) -> (listof char?)
;(string-ref str k) -> char? возвращает символ строки в позиции k
(define (string-to-letters-lst str)
  (let ((letter (string-head str)))
    (if (equal? letter "") '()
      (cons (string-ref letter 0) (string-to-letters-lst (string-tail str)))
    )
  )
)

;convert list of letters to string
;there is built-in function: (list->string lst) -> string?
(define (convert-letters-lst-to-string lst)
  (if (null? lst) ""
     (string-append (string (car lst)) (convert-letters-lst-to-string (cdr lst)))
  )
)

(define (exist? word lst)
  (and (not (null? lst))
       ( or (equal? word (car lst))
            (exist? word (cdr lst))
       )))

(define (reverse2 lst)
  (define (loop lst res)
    (if (null? lst) res
        (loop (cdr lst) (cons (car lst) res)
              )))
  (loop lst '()))

;'.' ',' ';' in quota presentation -> |,| |.| |;|
(define Separators '(#\, #\- #\: #\; #\'))
(define Terminators  '(#\. #\! #\?))
(define EnglishLetter '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                        #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(define Digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (isalpha? letter)
  (exist? letter EnglishLetter))

(define (isdigit? letter)
  (char-numeric? letter))

(define (isseparator? letter)
  (exist? letter Separators))

(define (isterminators? letter)
  (exist? letter Terminators))

(define (isspace? letter)
  (char-whitespace? letter))

(define (incorrectletter? letter)
  (not (or (isspace? letter) (isalpha? letter) (isdigit? letter) (isseparator? letter) (isspace? letter))))

(define (char->sym char)
  (string->symbol (string char)))

(define (last-elem lst)
  (if (null? (cdr lst)) (car lst) (last-elem (cdr lst))))



;lst - list of symbols
(define (last-is-term? lst)
  (if (null? lst) #f
      (let ((back (symbol->string (last-elem lst))))
        (if ( > (string-length back) 1) #f (exist? (string-ref back 0) Terminators)))))

;lst - list of chars
(define (first-is-term? lst)
  (if (null? lst) #t
      (or (isterminators? (car lst)) (incorrectletter? (car lst)))))



;(char-alphabetic? char) <-- return true if letter (Unicode)
;(char-numeric? char) <-- retunr true if digit (Unicode)
;!!!there is built-in function: (string->symbol str) -> symbol?   <----conversion string to quota

(define (create-word word)
  (if (null? word) '()
      (list (string->symbol (list->string (reverse word))))))

(define (convert-letters-lst-to-internal-presentation lst)
  (define (loop lst word seq res)
     (cond ((null? lst) (let ((last-seq (append seq (create-word word))))
                          (if (null? last-seq) res (cons last-seq res))))
          ((isalpha? (car lst)) (loop (cdr lst) (cons (car lst) word) seq res))
          ((isdigit? (car lst)) (loop (cdr lst) (cons (car lst) word) seq res))
          ((isseparator? (car lst)) (loop (cdr lst) '() (append seq (create-word word) (list (char->sym (car lst)))) res))

          ((isterminators? (car lst)) 
            (if (first-is-term? (cdr lst)) (loop (cdr lst) '() (append seq (create-word word) (list (char->sym (car lst)))) res )
                (loop (cdr lst) '() '() (cons (append seq (create-word word) (list (char->sym (car lst)))) res))
            )
          )
          ;((isterminators? (car lst)) (loop (cdr lst) '() '() (cons (append seq (create-word word) (list (char->sym (car lst)))) res)))
          ((isspace? (car lst)) (loop (cdr lst) '() (append seq (create-word word)) res))
          (else  ;(if (last-is-term? seq) (loop (cdr lst) '() '() (cons (append seq (create-word word) (list (char->sym (car lst)))) res))
                 ;    (loop (cdr lst) word seq res)
                 ;)
             (cond ((and (last-is-term? seq) (first-is-term? (cdr lst))) (loop (cdr lst) word seq res))
                   ((and (last-is-term? seq) (not (first-is-term? (cdr lst)))) (loop (cdr lst) '() '() (cons (append seq (create-word word)) res)))
                   ((and (not (last-is-term? seq)) (first-is-term? (cdr lst))) (loop (cdr lst) word seq res))
                   (else (loop (cdr lst) word seq res))
             )
          )
     )
  )
  (loop lst '() '() '())
)

(define (doctor-read)
  (begin
    (let ((input (read-line)))
      (reverse2 (convert-letters-lst-to-internal-presentation (string-to-letters-lst input)))
      )))



(define Punc (list "." "," ":" ";" "." "!" "?"))
(define WithoutSpace (list "'"))

(define (punctuation? str)
  (if (> (string-length str) 1) #f
      (exist? str Punc)))

(define (withoutspace? str)
    (if (> (string-length str) 1) #f
      (exist? str WithoutSpace)))

(define (doctor-string lst)
  (define (loop lst res)
    (cond ((null? lst) res)
          ;((null? (cdr lst)) (loop (cdr lst) (string-append res " " (symbol->string (car lst)))))
          ;((punctuation? (symbol->string (cadr lst))) (loop (cdr lst) (string-append res (symbol->string (car lst)))))
          ((or (punctuation? (symbol->string (car lst))) (withoutspace? (symbol->string (car lst)))) (loop (cdr lst) (string-append res (symbol->string (car lst)))))
          (else
           (if (withoutspace? (string (string-ref res (- (string-length res) 1))))   (loop (cdr lst) (string-append res (symbol->string (car lst))))
               (loop (cdr lst) (string-append res  " " (symbol->string (car lst))))))
    )          
  )
  (if (null? lst) ""
      (loop (cdr lst) (symbol->string (car lst)))
  )
)

(define (doctor-print lst)
  (define (loop lst res)
    (cond ((null? lst) res)
          ((null? (cdr lst)) (loop (cdr lst) (string-append res (doctor-string (car lst)))))
          (else (loop (cdr lst) (string-append res (doctor-string (car lst)) " ")))
    )
   )
   (loop lst "")
)

(define (input-output)
  (doctor-print (doctor-read)))
;===================================DOCTOR======================================

(define (visit-doctor)
  (define (doctor-driver-loop name old-phrases)
    (define (reply user-response)
      
      (define (change-person pairs phrase)
        (many-replace pairs phrase))
      
      (define (qualifier)
        (pick-random '((you seem to think)
                       (you feel that)
                       (why do you believe)
                       (why do you say)
                       (what do you mean when say)
                       (You said)
                       (It |'| s very interesting What can you say except)
                       (We are clearly in the right direction |,| if you said)))
      )
      
      (define (hedge)
        (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (can you say in more detail about your problem)
                       (Do you believe in what you said)
                       (We are on the right track)
                       (If you need anything |,| just ask)
                       (Make yourself at home)
                       (How long have you had these thoughts)
                       (If you could wave a magic what positive changes would you make happen in your life)))
      )

      (define (fifty-fifty)
        ( cond ((null? old-phrases) (random 3) )
               (else (random 4)))
      )

      ;Задание №5. Список списков вида ( (key1 ... keyN) (seq1) ... (seqM) )
      (define KEYWORDS-LIST
        '( ((depressed suicide)
             (when you feel depressed |,| go out for ice cream)
             (depression is a disease that can be threated)
          )
          ((mother father family parents)
           (tell me more about your *)
           (why do you feel that way about your * ?)
           (Are your * good?)
          )
          ((rain snow)
           (Do you like *)
           (What thoughts arise for you when the * ?)
          )
         )
      )

      (define (keywords keywords-list)
        (cond ((null? keywords-list) '())
              (else (append (caar keywords-list) (keywords (cdr keywords-list )) ))
        )
      )

;!!!!NEW
      (define (intersect? lst1 lst2)
        (and  (not(null? lst1))
              (not(null? lst2))
              (or (exist? (car lst1) lst2)
              (intersect? (cdr lst1) lst2))
        )
      )

      (define (intersectOLD? lst1 lst2)
        (cond ((null? lst1) #f)
              ((null? lst2) #f)
              ((exist? (car lst1) lst2) #t)
              (else (intersectOLD? (cdr lst1) lst2))
        )
      )

      ;Задание №5. 6. Заменяем в списке все '* на word и возвращаем полученный список в качестве результата
      (define (replace-all-words-in-lst word lst)
        (cond ((null? lst) lst)
              ((equal? '* (car lst)) ( cons word (replace-all-words-in-lst word (cdr lst))))
              (else (cons (car lst) (replace-all-words-in-lst word (cdr lst))))
        )
      )

;!!!!NEW      ;Задание №5. 5. Проверяет наличие word в списке lst
      (define (exist? word lst)
          (and (not (null? lst))
              ( or (equal? word (car lst))
                   (exist? word (cdr lst))
              )
          )
      )

      (define (existOLD? word lst)
        (cond ((null? lst) #f)
              ((equal? word (car lst)) #t)
              (else (existOLD? word (cdr lst)))
         )
      ) 
      
      ;Задание №5. 4. Проверяем наличие word в первом подсписке keys, если есть,
      ;то возвращаем рандомный подсписок, отличный от первого(в котором хранятся ключевые слова)
      ;Если во втором подсписке списка keys есть символ '*, то выбираем произвольную фразу из (cdr keys)
      ;и вызываем функцию replace-all-words-in-lst
;!!!!NEW      ;Используем уже написанную функцию many-replace для замены * на ключевое слово
      (define (check-keys word keys)
        (cond ((exist? word (car keys))
                (cond ((exist? '* (cadr keys)) (many-replace (list(list '* word)) (pick-random (cdr keys))))
                      ( else (pick-random (cdr keys)))
                )
              )
              (else '())
        )
      )

      ;Задание №5. 3. Для каждого keys из keyslst вызываем функцию check-all-keys
      (define (check-all-keys keyslst word)
        (cond ((null? keyslst) '())
              (else (let ((result (check-keys word (car keyslst))))
                      ( if (null? result) (check-all-keys (cdr keyslst) word)
                           result
                      )
                    )
              )
        )
      )
      
      ;Задание №5. 2.Пробегаемся по всем словам из ответа клиента вызываем функцию check-key-words
      (define (check-key-words lst keyslst)
        (cond ((null? lst) '())
              (else (let ((result (check-all-keys keyslst (car lst))))
                      (if (null? result) (check-key-words (cdr lst) keyslst)
                          (cons result (check-key-words (cdr lst) keyslst))
                      )
                    )
              )
        )
      )

      ;Все функции, реализующие различные стратегии ответа должны иметь функции обертки, которые имеют один входной параметр user-response
      ;Задание №5. 1. Обертка для check-key-words
      (define (keywords-strategy client-response)
        (check-key-words client-response KEYWORDS-LIST)
      )

      (define (trite-expression client-response)
        (list
         (hedge)
         )
      )

      (define (answer-old-phrase client-response)
        (list
         (cond ((null? old-phrases) '(You haven't said anything before that))
               (else (append '(early you said that) (change-person '((i you) (I you) (me you) (am are) (my your) (you i) (You I) (are am) (your my)) (pick-random old-phrases)))))
        )
      )

      (define (answer-change-pronoun client-response)
        (list
         (append (qualifier) (change-person '((i you) (I you) (me you) (am are) (my your) (you i) (You I) (are am) (your my)) client-response))
        )
      )

      ;Задание №6 1. Предикаты и соответствующий им список функций
      (define pred-F (  list ( list (lambda (x) (<= (length x) 5)) trite-expression answer-old-phrase)
                             ( list (lambda (x) (intersect? '(i you I me am are my your You) x) ) answer-change-pronoun trite-expression)
                             ( list (lambda (x) (intersect? (keywords KEYWORDS-LIST) x )) keywords-strategy)
                      )
      )
      
      ;Задание №6 2. Пробегаемся по списку, выполняем предикат (первый элемент каждого подсписка),
      ;если #t, следовательно, выполняем рандомную функцию(вместо pick-random можно сделать выбор с некоторой вероятностью/весами),
      ;соответствующую данному предикату, иначе ничего не делаем
      ;После всегда переходим к другому подсписку
      (define (execute-strategy pred-func-lst client-response)
        (cond ((null? pred-func-lst) '())
              (else (let ((pred-func (car pred-func-lst)))
                      ( if ((car pred-func) client-response)
                           (let ((result ((pick-random (cdr pred-func)) client-response)))
                             (cond ((null? result) (execute-strategy (cdr pred-func-lst) client-response))
                                   (else (append result (execute-strategy (cdr pred-func-lst) client-response)))
                             )
                           )
                           (execute-strategy (cdr pred-func-lst) client-response)
                      )
                    )
              )
        )
      )

      ;New version - with predicates
      (begin
        (let ((response (pick-random user-response))) 
          (list (pick-random (execute-strategy pred-F response)))))
    )

    (define (unique-push element vector)
        (cond ((equal? element '() ) vector)
              ((null? vector) ( cons element vector))
              (else (let ((curr-element (car vector)))
                      (cond ((equal? curr-element element) vector)
                            (else (cons curr-element (unique-push element (cdr vector))))
                      )
                    )
              )
        )
    )

    (define (update-history lst history)
      (if (null? lst) history
          (update-history (cdr lst) (unique-push (car lst) history)))) 

    (newline)
    (print '**)
    (let ((user-response (doctor-read)))
      (cond ((null? user-response) (begin (printf "Please continue\n") (doctor-driver-loop name old-phrases)))
            ((equal? (doctor-print user-response) "goodbye" )
             (printf "Goodbye, ~a!\n" (doctor-print name))
             (print '(see you next week))
             ;(print old-phrases) Можно распечатать список всех ответов клиента
             (newline))
            (else (begin (printf "HISTORY -> ") (print old-phrases) (newline))
                  (printf (doctor-print (reply user-response)))
                  ;unique push| old-phrases is a set
                  (doctor-driver-loop name (update-history user-response old-phrases) ))))
                  
  )

  (define (ask-patient-name)
    (begin
    (print '(NEXT!))
    (newline)
    (print '(Who are you?))
    (doctor-read)))

  (define name (ask-patient-name))

  (cond ((equal? (doctor-print name) "suppertime") (print '(Time to sleep)))
        (else (begin
                     (printf "Hello, ~a!\n" (if (null? name) "My friend"  (doctor-print name)))
                     (print '(what seems to be the trouble?))
                     (doctor-driver-loop (if (null? name) '((My friend)) name) '())
                     (visit-doctor)
               )
        )
  )
)

(define (replace replacement-pairs word)
  (cond ((null? replacement-pairs) word)
        ((equal? (caar replacement-pairs) word) (cadar replacement-pairs))
        (else (replace (cdr replacement-pairs) word ) )
  )
)

(define (pick-random lst)
  ( cond ((null? lst) '())
         ( else (list-ref lst (random (length lst))))
  )
)

;New many-replace function
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) '())
              (else (let ((current-word (car lst)))
                      ( cons (replace replacement-pairs current-word)  (many-replace replacement-pairs (cdr lst))))
              )
        )
)