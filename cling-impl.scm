(import
  scheme
  (only chicken.base compose cut foldl make-parameter print)
  (only chicken.process-context command-line-arguments program-name))

(import
  (only defstruct defstruct)
  (only fmt dsp fmt fmt-join tabular)
  (only optimism parse-command-line)
  (only srfi-1 assoc filter for-each map)
  (only srfi-13 string-upcase))

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

(define (cling . args)
  (define (get-help arg)
    (let ((sa (arg-switches arg)))
      (make-help-entry
        (arg-help arg)
        `(,@(map symbol->string (car sa))
           ,@(map (compose string-upcase symbol->string)
                  (smth->list (cdr sa)))))))

  (define (get-kons arg)
    (cons (car (arg-switches arg))
          (arg-kons arg)))

  (let* ((rest-kons/args ; Get optional positional arguments handler
           (if (or (null? args)
                   (arg? (car args)))
               (cons kons-default args)
               args))
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
           (ret (map (cut fmt-join dsp <> " ") ret))
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
  (define (make-kons konses rest-kons)
    (lambda (ret opt)
      (let ((!f?    (lambda (x f) (if x (f x) x)))
            (switch (car opt))
            (args   (cdr opt)))
        (if (eq? switch '--)
            (rest-kons ret switch args)
            (let ((proc (or (!f? (assoc switch konses memq) cdr)
                            kons-default)))
              (proc ret switch args))))))

  (let* ((grammar (cling-grammar cling))
         (konses  (cling-konses cling))
         (pargs   (parse-command-line args grammar))
         (kons    (make-kons konses (cling-rest-kons cling))))
    (foldl kons knil pargs)))
