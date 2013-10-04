#lang racket
(require mred ; similar racket/gui
         ffi/unsafe
         ffi/unsafe/define
         sgl/gl
         sgl/init
         sgl/gl-types
         sgl/gl-vectors)

; Load opengl from MAC
(define gl-lib (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")) 


(define-ffi-definer define-gl gl-lib)


(define-gl glGenBuffers (_fun (size : _gl-sizei) (buffer : _gl-intv) 
                              -> _void
                              -> buffer))

(define-gl glBindBuffer (_fun _gl-uint _gl-uint -> _void))
(define-gl glBufferData (_fun _gl-uint _gl-sizei _gl-floatv _gl-uint -> _void))

(define (gl-gen-buffers size)
  (gl-vector->vector (glGenBuffers size (make-gl-int-vector size))))

(define frame (new frame% [label "fuck"]))

(define canvas 
  (new 
   (class canvas%
     
     (super-instantiate () (style '(gl no-autoclear)) (stretchable-width #f) (stretchable-height #f)) ; Like "Super" in java super(arg, arg, arg)
     
     (inherit with-gl-context)
     
     (define triangle-vertex-buffer #f)
     (define triangle-color-buffer #f)
     (define vertexes
       (vector (- (/ (send this get-width) 2))
               (- (/ (send this get-height) 2))
               0
               5
               (/ (send this get-width) 2)
               (- (/ (send this get-height) 2))
               0
               5
               0
               (/ (send this get-height) 2)
               0
               5))
     (define color (vector 1.0 0.0 0.0 1.0 0.0 1.0 0.0 0.5 0.0 0.0 1.0 0.5))
     (define scale
       (vector (/ 1 (send this get-width)) 0 0 0 
               0 (/ 1 (send this get-height)) 0 0
               0 0 1 0
               0 0 0 1))
     
     (define/override (on-paint)
       (with-gl-context draw))
     
     (define (init)
       (glClearColor 0 0 0 .4)
       (glDepthRange .1 100)
       (glViewport 0 0 (send this get-width) (send this get-height))
       (glClear GL_COLOR_BUFFER_BIT)
       
       (define v (gl-gen-buffers 2))
       (set! triangle-vertex-buffer (vector-ref v 0))
       (set! triangle-vertex-buffer (vector-ref v 1))
       
       (glBindBuffer GL_ARRAY_BUFFER triangle-vertex-buffer)
       (glBufferData GL_ARRAY_BUFFER (vector-length vertexes) (vector->gl-float-vector vertexes) GL_STATIC_DRAW)
       (glBindBuffer GL_ARRAY_BUFFER triangle-color-buffer)
       (glBufferData GL_ARRAY_BUFFER (vector-length color) (vector->gl-float-vector color) GL_STATIC_DRAW))
     
     (define (draw) #f)
     (inspect #f)
     (with-gl-context init))
   [parent frame] [min-width 500] [min-height 500]))

(send frame show #t)