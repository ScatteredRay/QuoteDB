(require-extension sqlite3)
(require-extension srfi-95)

(define min-token-size 3)
(define max-quote-length 300)

(define (count-duplicates! lst)
  (if (null? lst)
      '()
   (letrec ((collapse
             (lambda (lst curr count)
               (cond
                ((null? lst)
                 (cons (cons curr count) '()))
                ((eq? (car lst) curr)
                 (collapse (cdr lst) curr (+ count 1)))
                (else
                 (cons (cons curr count) (collapse (cdr lst) (car lst) 1))))))
            (sort-lst (sort! lst <)))
     (collapse (cdr sort-lst) (car sort-lst) 1))))

(define (match-quote-id db lst)
  (let* ((statement (sqlite3:prepare db "select DISTINCT quote_id from quotes_index where token = ? and (select length(quote) from quotes where id = quote_id) < ?;"))
         (quote-id-lst (concatenate
                        (map (lambda (tok) (sqlite3:map-row (lambda (x) x)
                                                       statement
                                                       tok
                                                       max-quote-length))
                             lst))))
    (sqlite3:finalize! statement)
    quote-id-lst))

(define (select-quote-id lst)
  (if (null? lst)
      '()
      (let ((pri-lst (sort! (count-duplicates! lst)
                            (lambda (x y) (> (cdr x) (cdr y))))))
        (caar pri-lst))))

(define (match-quote lst)
  (let* ((quote-db (sqlite3:open "quote.db"))
         (statement (sqlite3:prepare quote-db "select quote from quotes where id = ?;"))
         (quote (sqlite3:first-result statement (select-quote-id (match-quote-id quote-db lst)))))
    (sqlite3:finalize! statement)
    (sqlite3:finalize! quote-db)
    quote))

(define (match-quote-string str)
  (match-quote (remove (lambda (x) (< (string-length x) min-token-size)) (string-split (string-trim-both str) " "))))