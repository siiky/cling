(import
  (only chicken.base print)
  (only chicken.process-context command-line-arguments program-name))

(import
  (only cling help process-arguments usage)
  (only defstruct defstruct))

(defstruct options help some-arg rest)

(define opts
  `((((-h --help))
     "Show this help text"
     ,(lambda (ret _) (update-options ret #:help #t)))

    ((--some-arg . some-arg)
     "Some help message"
     ,(lambda (ret some-arg) (update-options ret #:some-arg some-arg)))))

(define (main args)
  (let ((options (process-arguments
                   opts
                   (make-options #:help #f #:some-arg #f #:rest #f)
                   (lambda (ret rest)
                     (update-options ret #:rest rest))
                   args)))
    ; Use options
    (print "Help: " (options-help options))
    (print "Some arg: " (options-some-arg options))
    (print "Rest: " (options-rest options))
    (usage (program-name))
    (help (program-name))))

(main (command-line-arguments))
