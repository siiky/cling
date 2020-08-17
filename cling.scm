(module
  cling
  (
   *program-name*
   *usage*
   arg
   cling
   help
   process-arguments
   usage
   )

  (import
    scheme
    (only chicken.base alist-ref cute foldl make-parameter o print)
    (only chicken.process-context command-line-arguments program-name))

  (import
    (only fmt dsp fmt fmt-join tabular)
    (only optimism parse-command-line)
    (only srfi-1 map)
    (only srfi-13 string-upcase)
    (only typed-records defstruct))

  (define (usage #!optional (pn (*program-name*)))
    (print pn " [OPTION ...] [--] [ARG ...]"))

  (define *usage* (make-parameter usage))
  (define *program-name* (make-parameter (program-name)))

  (define (kons-default ret switch args) ret)

  (defstruct arg switches help kons)
  (defstruct cling grammar help konses rest-kons)

  (define (arg switches #!key (help "") (kons kons-default))
    (make-arg #:switches (cons (smth->list (car switches)) (cdr switches))
              #:help     help
              #:kons     kons))

  (define make-help-entry cons)
  (define help-entry-text car)
  (define help-entry-switches/args cdr)

  (define (cling #!optional rest-kons #!rest args)
    (define (get-help arg)
      (let ((sa (arg-switches arg)))
        (make-help-entry
          (arg-help arg)
          `(,@(map symbol->string (car sa))
             ,@(map (o string-upcase symbol->string)
                    (smth->list (cdr sa)))))))

    (define (get-kons arg)
      (cons (car (arg-switches arg))
            (arg-kons arg)))

    (let* ((rest-kons/args ; Get optional positional arguments handler
             (if (and (not (arg? rest-kons))
                      (procedure? rest-kons))
                 (cons rest-kons args)
                 (cons kons-default args)))
           (rest-kons (car rest-kons/args))
           (args      (cdr rest-kons/args))
           (grammar   (map arg-switches args))
           (help      (map get-help     args))
           (konses    (map get-kons     args)))
      (make-cling #:grammar   grammar
                  #:help      help
                  #:konses    konses
                  #:rest-kons rest-kons)))

  (define (singl x) `(,x))
  (define (smth->list smth)
    (if (or (pair? smth)
            (null? smth))
        smth
        (singl smth)))

  (define (help cling #!optional (pn (*program-name*)))
    (define (get-switches/args-column ret)
      (let* ((ret (map help-entry-switches/args ret))
             (ret (map (cute fmt-join dsp <> " ") ret))
             (ret (fmt-join dsp ret "\n")))
        ret))

    (define (get-text-column ret)
      (let* ((ret (map help-entry-text ret))
             (ret (fmt-join dsp ret "\n")))
        ret))

    (let* ((help (cling-help cling))
           (usage (*usage*))
           (switches/args-column (get-switches/args-column help))
           (text-column (get-text-column help)))
      (usage pn)
      (newline)
      (fmt #t (tabular switches/args-column "\t" text-column))
      (newline)))

  (define (process-arguments cling knil #!optional (args (command-line-arguments)))
    (define ((make-kons konses rest-kons) ret opt)
      (let ((switch (car opt))
            (args   (cdr opt)))
        (if (eq? switch '--)
            (rest-kons ret switch args)
            (let ((kons (alist-ref switch konses memq kons-default)))
              (kons ret switch args)))))

    (let* ((grammar (cling-grammar cling))
           (konses  (cling-konses cling))
           (pargs   (parse-command-line args grammar))
           (kons    (make-kons konses (cling-rest-kons cling))))
      (foldl kons knil pargs))))
