#lang racket
(with-output-to-file "debug1.txt"
  #:exists 'replace
  #:mode 'binary
  (lambda () 
    (printf "~a\n~a\n" 
            (f32vector->cpointer vbo-cube-colors)
            (for/list ((x (f32vector->list vbo-cube-colors))) x)))))

;;(printf (string-append "sizeof(cube_colors): " (number->string (* (f64vector-length vbo-cube-vertices) (compiler-sizeof 'float)))))


;; Store vertices in Vertex Buffer Object of type GL_ARRAY_BUFFER
;; Store vertices in Index Buffer Object of type GL_ELEMENT_ARRAY_BUFFER
;; GLuint id;
;; glGenBuffers(1, &id);
;; glBindBuffer(type, id);
;; glBufferData(type, sizeof(bvector), bvectir, GL_STATIC_DRAW);
(define (allocate-buffer-object bvector size type gl-ptr)
  ;; Generate 1 buffer object
  (define new-bufs (glGenBuffers 1)) 
  ;; Give id the address of 0th element of a u32 vector
  (define id (u32vector-ref new-bufs 0))
  (define id-ty (tee 'gl->vet-conversion (gl-vector->type new-bufs)))
  (glBindBuffer type id)
  (glBufferData type size gl-ptr GL_STATIC_DRAW)
  (define-syntax-rule (debug e ...)
    (eprintf "~v\n" (vector (cons 'e e) ...)))
  (debug bvector size type
         new-bufs (gl-vector? new-bufs)
         (u32vector->list new-bufs)
         id GL_UNSIGNED_INT id-ty (gl-type? id-ty) gl-ptr)
  id)

(define (tee tag e)
  (eprintf "~a: ~e\n" tag e)
  e)ss