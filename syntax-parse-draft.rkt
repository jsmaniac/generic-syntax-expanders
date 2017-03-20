#lang racket

(require generic-syntax-expanders
         (for-syntax syntax/parse
                     generic-syntax-expanders/private/expanders
                     generic-syntax-expanders)
         (for-meta 2
                   racket/base
                   racket/syntax
                   generic-syntax-expanders))

(begin-for-syntax
  (define-syntax ~expand
    (pattern-expander
     (λ (stx)
       (syntax-case stx ()
         [(_ type id extra-pat ...)
          ;(expander-type? #'type)
          (with-syntax ([t-expander? (format-id #'type "~a-expander?" #'type)]
                        [t-expander-type (format-id #'type "~a-expander-type" #'type)]
                        [id.fully-expanded (format-id #'id "~a.fully-expanded" #'id)])
            #'(~and (~var id)
                    ((~var exp (static t-expander?
                                       (format "a ~a expander" (syntax-e #'id))))
                     exp-arg (... ...))
                    (~bind [id.fully-expanded
                            (expand-syntax-tree-with-expanders-of-type t-expander-type
                                                                       #'id)])
                    (~parse (~and _ extra-pat ...) #'id.fully-expanded)))])))))


(define-expander-type foo)
(define-syntax foo
  (syntax-parser
    [(_ a:id (~expand foo b) c)
     #'b.fully-expanded]))

(define-foo-expander f-ex
  (λ (stx)
    #''works!))

(foo a (f-ex blah blah) c) ;; => 'works!