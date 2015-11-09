#lang scheme/base

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
                       (It's very interesting What can you say except)
                       (We are clearly in the right direction, if you said)))
      )
      
      (define (hedge)
        (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (can you say in more detail about your problem)
                       (Do you believe in what you said)
                       (We are on the right track)
                       (If you need anything, just ask)
                       (Make yourself at home)
                       (How long have you had these thoughts)
                       (If you could wave a magic wand what positive changes would you make happen in your life)))
      )

      (define (fifty-fifty)
        ( cond ((null? old-phrases) (random 3) )
               (else (random 4)))
      )

      ;Задание №5. Список списков вида ( (key1 ... keyN) (seq1) ... (seqM) )
      (define KEYWORDS-LIST
        '( ((depressed suicide)
             (when you feel depressed, go out for ice cream)
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
      (define pred-F (  list ( list (lambda (x) (< (length x) 5)) trite-expression answer-old-phrase )
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
      (pick-random (execute-strategy pred-F user-response)
      )
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

    (newline)
    (print '**)
    (let ((user-response (read)))
      (cond ((equal? user-response '(goodbye))
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             ;(print old-phrases) Можно распечатать список всех ответов клиента
             (newline))
            (else ;(print old-phrases)
                  (print (reply user-response))
                  ;standart push| old-phrases is a vector
                  ;(doctor-driver-loop name (cons user-response old-phrases)))))
                  ;unique push| old-phrases is a set
                  (doctor-driver-loop name (unique-push user-response old-phrases)))))
                  
  )

  (define (ask-patient-name)
    (begin
    (print '(NEXT!))
    (newline)
    (print '(Who are you?))
    (car (read))))

  (define name (ask-patient-name))

  (cond ((equal? name 'suppertime) (print '(Time to sleep)))
        (else (begin
                     (printf "Hello, ~a!\n" name)
                     (print '(what seems to be the trouble?))
                     (doctor-driver-loop name '())
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