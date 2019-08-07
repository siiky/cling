(import
  (only chicken.base print)
  (only chicken.process-context command-line-arguments program-name))

(import
  (only cling arg cling help process-arguments usage)
  (only defstruct defstruct))

(defstruct options help some-arg rest)

(define opts
  (cling
    ; Handle positional arguments
    (lambda (ret _ rest)
      (update-options ret #:rest rest))
    (arg '((-h --help))
         #:help "Show this help text"
         #:kons (lambda (ret _ _)
                  (update-options ret #:help #t)))
    (arg '(--some-arg . some-arg)
         #:help "Some help message"
         #:kons (lambda (ret _ some-arg)
                  (update-options ret #:some-arg some-arg)))))

(define (main args)
  (let ((options (process-arguments
                   opts
                   (make-options #:help #f #:some-arg #f #:rest #f)
                   args)))
    ; Use options
    (print "Help: " (options-help options))
    (print "Some arg: " (options-some-arg options))
    (print "Rest: " (options-rest options))
    (usage (program-name))
    (help opts (program-name))))

(main (command-line-arguments))
