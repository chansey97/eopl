;; Poor man module system
;; R5RS has no module system, so create a poor man module system.

(define (load-module prefix mod)
  (display (string-append "-------- Loading: " mod " --------" (string #\newline)))
  (for-each load (with-input-from-file (string-append prefix "/" mod "/" mod ".manifest")
                   (lambda ()
                     (map (lambda (x)
                            (string-append prefix "/" mod "/" x))
                          (read)))))
  )
