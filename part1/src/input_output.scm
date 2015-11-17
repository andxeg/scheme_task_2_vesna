#lang scheme/base
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
(define Separators '(#\, #\- #\: #\;))
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


(define (char->sym char)
  (string->symbol (string char)))


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
          ((isterminators? (car lst)) (loop (cdr lst) '() '() (cons (append seq (create-word word) (list (char->sym (car lst)))) res)))
          ((isspace? (car lst)) (loop (cdr lst) '() (append seq (create-word word)) res))
          (else (loop (cdr lst) word seq res))
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

(define (punctuation? str)
  (if (> (string-length str) 1) #f
      (exist? str Punc)))

(define (doctor-string lst)
  (define (loop lst res)
    (cond ((null? lst) res)
          ;((null? (cdr lst)) (loop (cdr lst) (string-append res " " (symbol->string (car lst)))))
          ;((punctuation? (symbol->string (cadr lst))) (loop (cdr lst) (string-append res (symbol->string (car lst)))))
          ((punctuation? (symbol->string (car lst))) (loop (cdr lst) (string-append res (symbol->string (car lst)))))
          (else (loop (cdr lst) (string-append res  " " (symbol->string (car lst)))))
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