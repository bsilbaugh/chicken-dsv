;;;
;;; DSV Utilities
;;;

(module dsv (dsv-record
			 dsv-columns
			 dsv-rows)
  (import scheme)

;; Returns a dsv record display function
(define (dsv-record delim values)
  (define (display-value vals port)
	(if (not (null? vals))
		(begin
		  (display (car vals) port)
		  (display-delim (cdr vals) port))))
  (define (display-delim vals port)
	(if (not (null? vals))
		(begin
		  (display delim port)
		  (display-value vals port))))
  (lambda (p) 
	(display-value values p)
	(newline p)))

;; Returns a display function for delimeter separated data records
(define (dsv-rows delim rows)
  (define (display-row delim rows port)
	(if (not (null? rows))
		(begin
		  ((dsv-record delim (car rows)) port)
		  (display-row delim (cdr rows) port))))
  (lambda (p) (display-row delim rows p)))

;; Returns a display function for delimeter separated data columns
(define (dsv-columns delim columns)
  (define (head col) (if (null? col) "" (car col)))
  (define (tail col) (if (null? col) '() (cdr col)))
  (define (display-row cols port)
	(if (< 0 (apply + (map (lambda (x) (if (null? x) 0 1)) cols)))
		(begin
		  ((dsv-record delim (map head cols)) port)
		  (display-row (map tail cols) port))))
  (lambda (p) (display-row columns p)))

;; Returns a dsv record parser
(define (dsv-record-parser delim)
  (let ((d (if (char? delim) (string delim) delim)))
	(lambda (line)
	  (string-split line d))))

;; Returns a dsv parser
(define (dsv-parser delim)
  (define (read-records records parse port)
	(let ((line (read-line port)))
	  (if (eq? 'eof line)
		  records
		  (read-records (cons (parse line) records) parse port))))
  (let ((parse (dsv-record-parser delim)))
	(lambda (p)
	  (read-records '() parse p))))

); module
