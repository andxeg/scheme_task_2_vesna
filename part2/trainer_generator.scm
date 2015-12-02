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

(define (doctor-read-from-string str)
  (reverse2 (convert-letters-lst-to-internal-presentation (string-to-letters-lst str))))



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
  ;Предполагаем, что в начале каждого текста стоит точка. Нужно добавить ее в словарь. Для этой начальной точки prev будет тоже точкой
  (add-new-word (loop text (make-hash) '|.|) '|.| '|.| (car text))
)

;=============================train-from-all======================================
(define (train-from-stdin)
  (create-graph (convert-to-lst (doctor-read)))
)

;text <-> result from doctor-read function, i.e. list of sequences(list of list)
(define (train-from-text text)
  (create-graph (convert-to-lst text))
)

;Read file line by line
(define (read-file filename)
  (define (loop file res)
    (let ((str (read-line file)))
        (if (eof-object? str) res
            (loop file (string-append res str "\n"))
        )
    )
  )
    (loop (open-input-file filename) "")
)

;MAIN
;Read file and create graph
(define (train-from-file inputfile)
  (train-from-text (doctor-read-from-string (read-file inputfile))))

;MAIN
;if file - output is already exist then -> raise exception
(define (save-graph graph filename)
  (begin
    (define out (open-output-file filename))
    (print graph out)
    (close-output-port out)
  )
)

;MAIN
(define (read-graph filename)
  (begin
    (define in (open-input-file filename))
    (define graph (read in))
    (close-input-port in)
    graph
  )
)

;MAIN
;(define (merge-graphs graph1 graph2)
  
;)

;MAIN
;Read file - inputfile, create graph and save it into outputfile
(define (train inputfile outputfile)
  (save-graph (train-from-file inputfile) outputfile)
)


;=================================================================================
(define (pick-random lst)
  ( cond ((null? lst) '())
         ( else (list-ref lst (random (length lst))))
  )
)
;=================================GENERATOR=======================================
;TODO: Контроль регистра букв нужно сделать
;Генератор должен быть встроен в "Доктор"
;Считывание из файла см. (read-graph filename)
(define PHRASE_LENGTH_LIMIT 50)

(define (find-word-with-weight hash weight)
  (define (loop keys)
   (cond ((null? keys) '|.|)
         ((=(hash-ref hash (car keys)) weight) (car keys))
         (else (loop (cdr keys)))
    )
  )
  (loop (hash-keys hash))
)

;Так как мы начинаем с точки и сами генерируем фразу, следовательно, слово word в графе
;всегда существует, и проверять его наличие не нужно
(define (take-next-word graph word)
  (begin (define next-words (cadr (hash-ref graph word)))
         (define max-weight (apply max (hash-values next-words)))
         (if (<= (random 20) 5)
           (find-word-with-weight next-words max-weight)
           (pick-random (hash-keys next-words))
         )
  )
)

;CHECK_WORD is terminator or not
(define (end? word)
  (if (=(string-length (symbol->string word)) 1) (isterminators? (string-ref (symbol->string word) 0))
      #f
  )
)

(define (phrase-complete? next-word phrase)
  (or (end? next-word) (>= (length phrase) PHRASE_LENGTH_LIMIT))
)

;GENERATOR_DIRECT_METHOD
(define (generator-direct-method filename init-word)
  (define (result-phrase ph last-word)
    (if (end? last-word) (reverse2 (cons last-word ph))
        (reverse2 (cons '|.| (cons last-word ph)) )
    )
  )
  (begin
    (define graph (read-graph filename))
    (define (loop prev-word res)
      (let ((next-word (take-next-word graph prev-word)))
        (if (phrase-complete? next-word res) (result-phrase res next-word)
            (loop next-word (cons next-word res))
        )
      )
    )  
    ;(loop '|.| '())
    (loop init-word '())
  )
)

;=================================================================================
(define (get-terminators-from-graph graph)
  (define (loop terminators res)
    (if (null? terminators) res
        (loop (cdr terminators)
              (if (hash-has-key? graph (char->sym (car terminators)))
                  (cons (char->sym (car terminators)) res)
                  res
              )
        )
    )
  )
  (loop Terminators '())
)

(define (take-prev-word graph word)
  (begin (define prev-words (car (hash-ref graph word)))
         (define max-weight (apply max (hash-values prev-words)))
         (if (<= (random 20) 5)
             (find-word-with-weight prev-words max-weight)
             (pick-random (hash-keys prev-words))
         )
  )
)

;GENERATOR_REVERSE_METHOD
(define (generator-reverse-method filename init-word)
  (define (result-ph ph last-word)
    (if (end? last-word) ph
        (cons last-word ph)
    )
  )
  (begin
    (define graph (read-graph filename))
    (define (loop next-word res)
      (let ((prev-word (take-prev-word graph next-word)))
        (if (phrase-complete? prev-word res) (result-ph res prev-word)
            (loop prev-word (cons prev-word res))
        )
      )
    )
    ;(define last-word (pick-random (get-terminators-from-graph graph)))
    ;(loop  last-word (list last-word))
    (loop  init-word '())
  )
)

;GENERATOR_HYBRID_METHOD
(define (generator-hybrid-method filename word)
  (append (generator-reverse-method filename word) (cons word (generator-direct-method filename word)))
)