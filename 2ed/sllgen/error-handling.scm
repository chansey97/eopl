
;;; ****************************************************************
;;; error handling
;;; ****************************************************************

(define sllgen:error 
  (lambda (who format . data)
    ;; print the message
    (eopl:printf "Error reported by sllgen during ~s:~%" who)
    (apply eopl:printf (cons format data))
    (newline)
    (eopl:error-stop)))
