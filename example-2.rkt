#lang racket/base
(require setup/dirs
         racket/file)

(define path-list
  (get-lib-search-dirs))

(define (proc p)
  (error (format "~a" (directory-list p))))

(for-each proc path-list)