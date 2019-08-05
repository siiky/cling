(module
  cling
  (
   *program-name*
   *usage*
   help
   process-arguments
   usage
   )

  (import
    scheme
    (only chicken.base assert compose cut foldl make-parameter print unless)
    (only chicken.process-context command-line-arguments program-name))

  (import
    (only fmt columnar dsp fmt)
    (only optimism parse-command-line)
    (only srfi-1 assoc filter for-each map)
    (only srfi-13 string-join string-upcase))

  (define (make-entry grammar help kons) `(,grammar ,help ,kons))
  (define (entry-grammar entry)           (car entry))
  (define (entry-help entry)              (cadr entry))
  (define (entry-kons entry)              (caddr entry))
  (define (maybe-entry-grammar entry)
    (and (pair? entry)
         (entry-grammar entry)))
  (define (maybe-entry-help entry)
    (and (pair? entry)
         (pair? (cdr entry))
         (entry-help entry)))
  (define (maybe-entry-kons entry)
    (and (pair? entry)
         (pair? (cdr entry))
         (pair? (cddr entry))
         (entry-kons entry)))

  (define (usage #!optional (pn (*program-name*)))
    (print pn " [OPTIONS...] [--] [ARGS...]"))

  (define *usage* (make-parameter usage))
  (define *program-name* (make-parameter (program-name)))
  (define *help* (make-parameter #f))

  (define (singl x) `(,x))
  (define (smth->list smth)
    (if (or (pair? smth)
            (null? smth))
        smth
        (singl smth)))

  (define (help #!optional (pn (*program-name*)))
    (define (get-switches/args-column ret)
      (let* ((ret (map
                    (lambda (l)
                      `(,@(map symbol->string (caar l))
                         ,@(map (compose string-upcase symbol->string)
                                (smth->list (cdar l)))))
                    ret))
             (ret (map (cut string-join <> " ") ret))
             (ret (string-join ret "\n")))
        ret))

    (define (get-text-column ret)
      (let* ((ret (map cdr ret))
             (ret (string-join ret "\n")))
        ret))

    (let ((help (*help*))
          (usage (*usage*)))
      (assert help "Arguments must be processed before calling help")

      (let ((switches/args-column (get-switches/args-column help))
            (text-column (get-text-column help)))
        (usage pn)
        (newline)
        (fmt #t (columnar (dsp switches/args-column) (dsp text-column)))
        (newline))))

  (define (default-rest-kons ret _) ret)

  (define (filter.map pred? f . ls)
    (filter pred? (apply map f ls)))

  (define (process-arguments options knil #!optional (rest-kons default-rest-kons) (args (command-line-arguments)))
    (define (process-grammar/info grammar/info)
      (define (sanitize-grammar-entry e)
        (and e (pair? e) (cons (smth->list (car e)) (cdr e))))

      (let ((grammar/info
              (map (lambda (gi)
                     (make-entry (sanitize-grammar-entry (maybe-entry-grammar gi))
                                 (maybe-entry-help gi)
                                 (maybe-entry-kons gi)))
                   grammar/info))
            (>< (lambda (f g)
                  (lambda (gi)
                    `(,(f gi) . ,(g gi))))))
        (let ((grammar (map maybe-entry-grammar grammar/info))
              (help (filter.map cdr (>< entry-grammar entry-help) grammar/info))
              (kons (filter.map cdr (>< (compose car entry-grammar) entry-kons) grammar/info)))
          (*help* help)
          `(,grammar . ,kons))))

    (let* ((grammar/kons (process-grammar/info options))
           (grammar (car grammar/kons))
           (kons (cdr grammar/kons)))
      (let ((pargs (parse-command-line args grammar))
            (kons
              (lambda (ret opt)
                (let ((!f? (lambda (x f) (if x (f x) x)))
                      (switch (car opt))
                      (args (cdr opt)))
                  (if (eq? switch '--)
                      (rest-kons ret args)
                      (let ((proc (!f? (assoc switch kons memq) cdr)))
                        (if proc
                            (proc ret switch args)
                            ; NOTE: shouldn't happen
                            ret)))))))
        (foldl kons knil pargs))))
  )
