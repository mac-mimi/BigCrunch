
;; A simple viewer window for OpenGL.
#lang racket/base
(require racket/class
         racket/gui/base
         opengl
         ffi/vector
         ffi/unsafe)
(provide view
         current-elapsed-time
         current-screen-width
         current-screen-height
         znear)

(define current-elapsed-time (make-parameter 0))
(define current-screen-width (make-parameter 300))
(define current-screen-height (make-parameter 300))
(define znear 3.0)

;; glutDisplayFunc(...) -> on-paint
;; glutReshapeFunc(...) -> on-size

;; ============================================================================
;; CLASS gl-viewer%
;; ============================================================================
(define gl-viewer%
  ;; canvas% is a general purpose window for drawing and handling events
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers refresh)
    
    (init-field draw)
    (init-field setup)
    
    (define start-time (current-inexact-milliseconds))
    
    ;; Setup all resources and settings
    (define/public (setup-all)
      (with-gl-context
       (lambda ()
         (setup)
         (glEnable GL_BLEND)
         (glEnable GL_DEPTH_TEST)
         (glDepthFunc GL_LESS)
         (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)))
      (printf "setup all called.\n"))
    
    ;; on-size is called when the window is resized
    ;; Gives width and height in pixels of entire window
    (define/override (on-size new-width new-height)
      (with-gl-context
       (lambda ()
         (parameterize ([current-screen-width new-width]
                        [current-screen-height new-height])
           ;; Setup viewport in frame
           ;; (0,0) is lower-left corner of viewport
           (glViewport 0 0 (current-screen-width) (current-screen-height))))))
    
    ;; on-paint is called when the canvas is exposed or resized
    ;; so that the image in the canvas can be repainted.
    ;; (onDisplay)
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         ;; Draw the model!
         (parameterize ([current-elapsed-time 
                         (- (current-inexact-milliseconds) start-time)]) 
           (draw))))
      (swap-gl-buffers))
    
    ;; on-char is called when the canvas receives a *key* event
    (define/override (on-char event)
      (case (send event get-key-code)
        [(#\+) (set! znear (+ znear 0.1)) (refresh)]
        [(#\-) (set! znear (- znear 0.1)) (refresh)]))))

;; ============================================================================
;; Provided function: view
;; ============================================================================
;; Function Function ->
(define (view model-render init-resources)
  ;; Create a frame to hold our canvas
  (define frame
    (new frame%
         [label "OpenGL viewer"]
         [width (current-screen-width)]
         [height (current-screen-height)]))
  
  ;; Create our canvas with gl related setup
  (define canvas
    (new gl-viewer%
         (style '(gl no-autoclear)) ; canvas% field
         (parent frame)             ; this canvas is held inside this frame
         (draw model-render)        ; init the draw function
         (setup init-resources)))   ; init the resources
  
  (thread 
   (λ ()
     (let loop ()
       (sleep (/ 1.0 24.0))
       (send canvas refresh)
       (loop))))
  
  ;; Show the frame which contains the canvas
  (send canvas setup-all)
  (send frame show #t))
