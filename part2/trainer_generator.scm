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

;===================================TRAINER=======================================
;============================get-text-from-lst-of-lst=============================
(define (concate lst1 lst2)
  (define (loop lst1 lst2 res)
    (cond ((not (null? lst1)) (loop (cdr lst1) lst2 (cons (car lst1) res)))
          ((null? lst2) res)
          (else (loop '() (cdr lst2) (cons (car lst2) res)))
          ;alternative variant
          ;((not (null? lst2)) (loop '() (cdr lst2) (cons (car lst2) res)))
          ;(else res)
    )
  )
  (reverse2 (loop lst1 lst2 '()))
)

(define (convert-to-lst lst-of-lst)
    (define (loop lst-of-lst res)
      (if (null? lst-of-lst) res
            (loop (cdr lst-of-lst) (concate res (car lst-of-lst)))
      )
    )
  (loop lst-of-lst '())
)

;=================================create-graph====================================
(define (add-elem-to-hash hash elem)
  (if (hash-has-key? hash elem)
      (begin (hash-set! hash elem (+ (hash-ref hash elem) 1)) hash)
      (begin (hash-set! hash elem 1) hash)
  )
)

(define (add-new-word hash-table prev curr next)
  (if (hash-has-key? hash-table curr)
      (let ((lst (hash-ref hash-table curr)))
        (begin
          (add-elem-to-hash (car lst) prev)
          (add-elem-to-hash (cadr lst) next)
          hash-table
        )
      )
      (begin
        (hash-set! hash-table curr (list (make-hash) (make-hash)))
        (hash-set! (car (hash-ref hash-table curr)) prev 1)
        (hash-set! (cadr (hash-ref hash-table curr)) next 1)
        hash-table
      )
  )
)
 
(define (create-graph text)
  (define (loop text hash-table prev)
    (cond ((= (length text) 1) (add-new-word hash-table prev (car text) '|.|))
          ;((= (length text) 2)   (add-new-word hash-table prev (car text) (cadr text)))
          (else (loop (cdr text) (add-new-word hash-table prev (car text) (cadr text)) (car text)))
    )
  )
  (loop text (make-hash) '|.|)
)

(define (trainer)
  (begin
    (create-graph (convert-to-lst (doctor-read)))
  )
)

