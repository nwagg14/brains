#lang racket

(provide brainfuck)

; (de)increments the byte at buf-idx, then returns the new buffer
(define (buf-deinc buffer buf-idx op)
  (bytes-set! buffer buf-idx (modulo (op (bytes-ref buffer buf-idx) 1) 256))
  buffer)

; collects input and sets byte at buf-idx. then returns new buffer
(define (buf-input buffer buf-idx)
  (bytes-set! buffer buf-idx (read-byte))
  buffer)

; finds matching open-loop, and returns its index
(define (find-open code code-idx offset)
  (if (>= (- code-idx 1) 0)
   (case (string-ref code (- code-idx 1))
    [(#\[)
     (if (equal? offset 0)
      (- code-idx 1)
      (find-open code (- code-idx 1) (- offset 1)))]
    [(#\])
     (find-open code (- code-idx 1) (+ offset 1))]
    [else (find-open code (- code-idx 1) offset)])
   (raise 'No-Open-Exception)))

; finds matching close-loop, and returns its index
(define (find-close code code-idx offset)
  (if 
   (< (+ code-idx 1) (string-length code))
   (case (string-ref code (+ code-idx 1))
    [(#\])
     (if (equal? offset 0)
      (+ code-idx 1)
      (find-close code (+ code-idx 1) (- offset 1)))]
    [(#\[)
     (find-close code (+ code-idx 1) (+ offset 1))]
    
    [else (find-close code (+ code-idx 1) offset)])
   (raise 'No-Close-Exception)))

; handles parsed [
(define (handle-open code code-idx buffer buf-idx)
  (if (equal? (bytes-ref buffer buf-idx) 0)
   (parse code (find-close code code-idx 0) buffer buf-idx)
   (parse code (+ code-idx 1) buffer buf-idx)))

; handles parsed ]
(define (handle-close code code-idx buffer buf-idx)
  (if (equal? (bytes-ref buffer buf-idx) 0)
   (parse code (+ code-idx 1) buffer buf-idx)
   (parse code (find-open code code-idx 0) buffer buf-idx)))

; prints the byte at buf-idx, then continues parsing
(define (handle-print code code-idx buffer buf-idx)
  (printf "~c" (integer->char (bytes-ref buffer buf-idx)))
  (parse code (+ code-idx 1) buffer buf-idx))

; recursive function to parse code string
(define (parse code code-idx buffer buf-idx)
  (if (> (string-length code) code-idx) 
  (case (string-ref code code-idx)
    
    ; handle position changes
    [(#\>)
     (parse code (+ code-idx 1) buffer (+ buf-idx 1))]
    [(#\<)
     (parse code (+ code-idx 1) buffer (- buf-idx 1))]
    
    ; handle byte value changes
    [(#\+)
     (parse code (+ code-idx 1) (buf-deinc buffer buf-idx +) buf-idx)]
    [(#\-)
     (parse code (+ code-idx 1) (buf-deinc buffer buf-idx -) buf-idx)]
    
    ; handle loops
    [(#\[)
     (handle-open code code-idx buffer buf-idx)]
    [(#\])
     (handle-close code code-idx buffer buf-idx)]
    
    ; handle input/output
    [(#\.)
     (handle-print code code-idx buffer buf-idx)]
    [(#\,)
     (parse code (+ code-idx 1) (buf-input buffer buf-idx) buf-idx)]
    
    ; ignore everything else
    [else
     (parse code (+ code-idx 1) buffer buf-idx)])
  (printf "~nEnd~n")))

;wrapper for parse
(define (brainfuck code)
  (parse code 0 (make-bytes 64 0) 0))