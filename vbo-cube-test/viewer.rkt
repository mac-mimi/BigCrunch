;; A simple viewer window for OpenGL.
#lang racket/gui
(require opengl ffi/vector ffi/unsafe)
(provide view) 

(define screen-width 800)
(define screen-height 600)


;; glutDisplayFunc(...) -> on-paint
;; glutReshapeFunc(...) -> on-size


;; =========================================================================================
;; CLASS gl-viewer%
;; =========================================================================================
(define gl-viewer%
  ; canvas% is a general purpose window for drawing and handling events
  (class canvas%
    (super-new)
    
    ; inherit the following methods
    ; with-gl-context: 
    ; swap-gl-buffers: calls swap-gl-buffers on the result of get-gl-context
    ; refresh: enqueues an event to repaint the window
    (inherit with-gl-context swap-gl-buffers refresh)
    
    (init-field draw)
    (init-field setup)
    
    ;; Setup all resources and settings
    (define/public (setup-all)
      (with-gl-context      
       (lambda ()
         (setup)
         (glEnable GL_BLEND) 
         (glEnable GL_DEPTH_TEST)
         (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)))
      (printf "setup all called.\n"))
    
    ;; Variable for paint events
    (define setup-all-called #f)
    ;; Variables for key and mouse events
    (define handle-motion void)
    (define x-rotation 0)
    (define y-rotation 0)
    (define zoom 1)
    
    ;; on-size is called when the window is resized
    ;; Gives width and height in pixels of entire window
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (set! screen-width width)
         (set! screen-height height)
         ; Setup viewport in frame
         ; (0,0) is lower-left corner of viewport
         (glViewport 0 0 width height))))
    
    ;; on-paint is called when the canvas is exposed or resized 
    ;; so that the image in the canvas can be repainted.
    ;; (onDisplay)
    (define/override (on-paint)
      (with-gl-context      
       (lambda ()
         (glClearColor 1.0 1.0 1.0 1.0) ;; Clear the color buffer /w darkish blue
         
         ; Clear the buffers to preset values:
         ; GL_COLOR_BUFFER_BIT: buffers that are currently enabled for color writing
         ; GL_DEPTH_BUFFER_BIT: the depth buffer
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         
         ; Draw the model!
         (draw))
       )
      (swap-gl-buffers) ; Equivalent of glutSwapBuffers()
      )
    
    ;;  on-event is called when the canvas receives a *mouse* event 
    (define/override (on-event event)
      (define x (send event get-x))
      (define y (send event get-y)) 
      (case (send event get-event-type)
        [(left-down)
         (set! handle-motion
               (let ([old-x x] [old-y y])
                 (lambda (new-x new-y)
                   (set! x-rotation (+ x-rotation (- new-x old-x)))
                   (set! y-rotation (+ y-rotation (- new-y old-y)))
                   (set! old-x new-x)
                   (set! old-y new-y)
                   (refresh))))]
        [(left-up)
         (set! handle-motion void)]
        [(motion) (handle-motion x y)]))
    
    ;; on-char is called when the canvas receives a *key* even
    (define/override (on-char event)
      (case (send event get-key-code)
        [(#\+) (set! zoom (* zoom 4/3)) (refresh)]
        [(#\-) (set! zoom (/ zoom 4/3)) (refresh)]
        [(wheel-up) (set! zoom (* zoom 9/8)) (refresh)]
        [(wheel-down) (set! zoom (/ zoom 9/8)) (refresh)]))))

;; =========================================================================================
;; FUNCTION view (called by shader.rkt)
;; =========================================================================================
;; Function Function -> 
(define (view model-render init-resources)
  ;; Create a frame to hold our canvas
  (define frame 
    (new frame% 
         [label "OpenGL viewer"]
         [width screen-width]
         [height screen-height]))
  
  ;; Create our canvas with gl related setup
  (define canvas
    (new gl-viewer% 
         (style '(gl no-autoclear)) ;; canvas% field
         (parent frame) ;; this canvas is held inside this frame
         (draw model-render) ;; init the draw function  
         (setup init-resources))) ;; init the resources
  
  ;; Show the frame which contains the canvas
  (send canvas setup-all)
  (send frame show #t))