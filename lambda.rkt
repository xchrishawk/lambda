;;
;; lambda.rkt
;; Chris Vig (chris@invictus.so)
;;

#lang racket

;; -- Provides --

(provide
 (contract-out

  ;; Reads a datum from the specified input port.
  [rename lang-read read (-> input-port? any/c)]

  ;; Reads syntax from the specified input port.
  [rename lang-read-syntax read-syntax (-> path-string? input-port? syntax?)]))

;; -- Public Procedures --

(define (lang-read input)
  (syntax->datum
   (lang-read-syntax #f input)))

(define (lang-read-syntax src input)
  (syntax
   (module blah racket
     "hi")))

;; -- Types --

;; Struct representing a token parsed from an input stream.
(struct token (type literal line column position)
  #:transparent)

;; -- Lexer --

(define (lex input)
  (reverse
   (let loop ([tokens null])
     (if (eof-object? (peek-byte input))
         tokens
         (let*-values (;; Current location in input file
                       [(line column position)
                        (port-next-location input)]
                       ;; Utility procedure to generate token
                       [(make-token)
                        (λ (type [index 1])
                          (λ (groups)
                            (let ([literal (bytes->string/utf-8 (list-ref groups index))])
                              (token type literal line column position))))]
                       ;; Next token
                       [(token)
                        (cond
                          [(regexp-try-match #px"^[[:space:]]+" input) => (thunk* #f)]
                          [(regexp-try-match #px"^(λ)" input) => (make-token 'op-lambda)]
                          [(regexp-try-match #px"^(\\.)" input) => (make-token 'op-dot)]
                          [(regexp-try-match #px"^(\\()" input) => (make-token 'op-open-bracket)]
                          [(regexp-try-match #px"^(\\))" input) => (make-token 'op-close-bracket)]
                          [(regexp-try-match #px"^(=)" input) => (make-token 'op-assign)]
                          [(regexp-try-match #px"^([A-Za-z0-9\\-_]+)" input) =>
                           (λ (groups)
                             (match (list-ref groups 1)
                               [#"def" ((make-token 'op-define) groups)]
                               [identifier ((make-token 'identifier) groups)]))]
                          [else
                           (error
                            (format
                             "Unrecognized token at ~A."
                             (position-string line column position)))])])
           ;; Append the token to the list if it's valid
           (if token
               (loop (cons token tokens))
               (loop tokens)))))))

;; -- Utility --

(define (position-string line column position)
  (cond
    [(and line column) (format "line ~A, column ~A" line column)]
    [position (format "character ~A" position)]
    [else "unknown position"]))
