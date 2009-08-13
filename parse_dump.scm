(require-extension ssax)
(require-extension sqlite3)
(require-extension srfi-13)
(require-extension regex)

(define quote-port (open-input-file "enwikiquote-20090121-pages-articles.xml"))
;(define quote-port (open-input-file "testquote.xml"))
(define min-quote-length 15)

(define (make-chain-input-port chain)
  (if (null? chain)
      #f
   (cons (open-input-string (car chain))
         (cdr chain))))

(define (read-chain-char port)
  (let ((c (read-char (car port))))
    (if (eof-object? c)
        (if (null? (cdr port))
            c
            (begin
             (set-car! port (open-input-string (cadr port)))
             (set-cdr! port (cddr port))
             (read-chain-char port)))
        c)))

(define (quote-skip-char c)
  (or (eq? c #\')))

; Need to remove <...> xml style tags.

(define (de-wikify lst)
  (if (null? lst)
      '()
      (cond
       ((quote-skip-char (car lst))
        (de-wikify (cdr lst)))
       ((and
         (not (null? (cdr lst)))
         (eq? (car lst) #\[)
         (eq? (cadr lst) #\[))
        (de-wikify
         (letrec ((find-link-inner (lambda (lst)
                                     (cond
                                      ((null? lst) '())
                                      ((eq? (car lst) #\|)
                                       (cdr lst))
                                      ((eq? (car lst) #\])
                                       #f)
                                      (else
                                       (find-link-inner (cdr lst))))))
                  (rest (find-link-inner (cddr lst))))
           (if rest
               rest
               (cddr lst)))))
       ((and
         (eq? (car lst) #\<)
         (not (null? (cdr lst)))
         (eq? (cadr lst) #\!)
         (not (null? (cddr lst)))
         (eq? (caddr lst) #\-)
         (not (null? (cdddr lst)))
         (eq? (cadddr lst) #\-))
        (de-wikify
         (letrec ((find-comment-end (lambda (lst)
                                      (if (null? lst)
                                          '()
                                          (if (and
                                               (eq? (car lst) #\-)
                                               (eq? (cadr lst) #\-)
                                               (eq? (caddr lst) #\>))
                                              (cdddr lst)
                                              (find-comment-end (cdr lst)))))))
           (find-comment-end (cddddr lst)))))
       ((and (eq? (car lst) #\])
             (not (null? (cdr lst)))
             (eq? (cadr lst) #\]))
        (de-wikify (cddr lst)))
       (else
        (cons (car lst) (de-wikify (cdr lst)))))))

(define (unwiki lst)
  (cond
   ((null? lst)
    '())
   ((eq? (car lst) #\space)
    (unwiki (cdr lst)))
   ((eq? (car lst) #\[)
    '())
   (else
    (de-wikify lst))))

(define (quote-end-char? c)
  (or (eq? c #\*)
      (eq? c #\=)))

(define (parse-quote-start port chain)
  (let ((c (read-chain-char port)))
    (if (eof-object? c)
        '()
        (if (quote-end-char? c)
            (cons
             (unwiki chain)
             (parse-quote-list port c))
            (parse-quote-start port (append! chain (cons c '())))))))

(define (parse-title-start port)
  (let ((c (read-chain-char port)))
    (cond
     ((eq? c #\=)
      (parse-title-start port))
     ((eq? c #\space)
      (parse-title-start port))
     (else
      (let ((title (letrec ((parse-title-end (lambda (c port chain)
                                        (cond
                                         ((eq? c #\=)
                                          (parse-title-end (read-chain-char port) port chain))
                                         ((eq? c #\newline)
                                          chain)
                                         (else
                                          (parse-title-end (read-chain-char port) port (append! chain (cons c '()))))))))
          (list->string (parse-title-end c port '())))))
        (if
         (string-match ".*External links.*" title)
         '()
         (parse-quote-list port #\newline)))))))

(define (parse-quote-list port co)
  (let ((c (read-chain-char port)))
    (if (eof-object? c)
        '()
        (cond
         ((and (eq? co #\newline) (eq? c #\*))
          (parse-quote-start port '()))
         ((and (eq? co #\newline) (eq? c #\=))
          (parse-title-start port))
         (else
          (parse-quote-list port c))))))

(define quote-db (sqlite3:open "quote.db"))

(sqlite3:exec quote-db "CREATE TABLE IF NOT EXISTS quotes (id INTEGER PRIMARY KEY AUTOINCREMENT, quote TEXT);")

(define statement (sqlite3:prepare quote-db "Insert into quotes (quote) VALUES (?)"))

(define quote-parser
  (SSAX:make-parser
   NEW-LEVEL-SEED
   (lambda (elem-gi attributes namespaces expected-content seed)
     (if (eq? (cdr elem-gi) 'text)
         '()
         #f))

   FINISH-ELEMENT
   (lambda (elem-gi attributes namespaces parent-seed seed)
     (display #\.)
    (if seed
        (map (lambda (quote-in)
               (let ((quote (list->string quote-in)))
                  (display #\.)
                  (if (>= (string-length quote) min-quote-length)
                      (sqlite3:exec statement quote))))
              (parse-quote-list (make-chain-input-port seed) #f)))
     parent-seed)

   CHAR-DATA-HANDLER
   (lambda (string1 string2 seed)
     (if seed
         (append! seed (cons string1 '()))
         seed))))

(quote-parser quote-port #f)

(close-input-port quote-port)

(sqlite3:finalize! statement)

(sqlite3:finalize! quote-db)

;; (call-with-output-file "quotes.out" (lambda (out)
;;                                       (write
;;                                        (map list->string quote-list)
;;                                       out)))