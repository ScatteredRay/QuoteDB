;; Copyright (c) 2009, Nicholas "Indy" Ray. All rights reserved.
;; See the LICENSE file for usage, modification, and distribution terms.
(require-extension sqlite3)

(define (read-all #!optional in) (let ((R (read (if in in (current-input-port)))))
                                   (if (eof-object? R)
                                       '()
                                       (cons R (read-all in)))))

(define min-token-size 3)

(define quote-db (sqlite3:open "quote.db"))

(sqlite3:exec quote-db "CREATE TABLE IF NOT EXISTS quotes_index (token, quote_id);")

(define statement (sqlite3:prepare quote-db "Select id, quote from quotes;"))
(define ins-statement (sqlite3:prepare quote-db "INSERT INTO quotes_index (token, quote_id) VALUES (?, ?);"))

(define (add-index quote_id lst)
  (if (null? lst)
      '()
      (begin
        (if (>= (string-length (string-trim-both (car lst))) min-token-size)
         (sqlite3:exec ins-statement (string-trim-both (car lst)) quote_id))
        (add-index quote_id (cdr lst)))))

(define (index-record)
  (if (sqlite3:step! statement)
      (begin
        (add-index (sqlite3:column-data statement 0) (string-split (sqlite3:column-data statement 1) " "))
        (index-record))))

(index-record)

(sqlite3:finalize! ins-statement)
(sqlite3:finalize! statement)

(sqlite3:finalize! quote-db)