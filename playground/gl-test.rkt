#lang racket/gui
(displayln "loaded")
(require opengl)
(displayln "opengl'd")
(provide with-gl (all-from-out opengl))

(define f (new frame% [label "foo"]))
(displayln "framed")
(define c
  (new 
   (class canvas%
     (inherit with-gl-context)
     (super-new)
     
     ;; (-> X) -> void
     ;; run thynk t in GL context 
     (define/public (run t) (with-gl-context t)))
   [parent f] [min-width 500] [min-height 500] [style '(gl)]))
(displayln "canvas'd")
(define-syntax-rule (with-gl x ...)
  (begin
    (send c run (lambda () x ...))
    (send c swap-gl-buffers)
    (send c refresh-now))) 

(send f show #t)
(displayln "show'd")