;; A simple viewer window for OpenGL.
;; Allows user to rotate and zoom the scene.
#lang racket/gui

(require opengl)
(provide view)

; CLASS gl-viewer%
(define gl-viewer%
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers refresh)
    
    (init-field draw)
    (init-field (setup void))
    
    (define setup-called #f)
    
    ;; Setup the matrices
    
    ;; model->world:     From model coords to world coords (Don't need to do this since models are already in world coords)
    ;; world->view:      From world coords to camera viewing coords aka Model View Matrix 
    ;; view->projection: Form view coords to projection(2D screen) coords aka Projection Matrix
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         
         ; Setup viewport in frame
         ; (0,0) is lower-left corner of viewport
         (glViewport 0 0 width height)
         
         ; Make the model view matrix be the current matrix
         ; Model view matrix is one that converts all the
         ; vertices of our models to certain translations/rotations...
         (glMatrixMode GL_MODELVIEW)
         (glLoadIdentity)
         ; Translate it back
         (glTranslated 0.0 0.0 -10.0)
         
         ; Make the projection matrix be the current matrix
         ; Projection matrix is one that converts 3D to 2D 
         (glMatrixMode GL_PROJECTION) 
         (glLoadIdentity)
         ; Setup the perspective matrix (a frustrum)
         ; which sets the l, r, b, t, zNear and zFar clipping plane. 
         (if (< width height)
             (let ((h (/ height width)))
               (glFrustum -1.0 1.0 (- h) h 8.0 12.0))
             (let ((h (/ width height)))
               (glFrustum (- h) h -1.0 1.0 8.0 12.0)))
         
         )))
    
    
    
    (define/override (on-paint)
      (with-gl-context               
       (lambda ()
         (unless setup-called
           (setup)
           (set! setup-called #t))
         ; Clear the color buffer
         (glClearColor 0.0 0.0 0.3 0.0) ; darkish blue
         ; Clear the buffers to preset values:
         ; GL_COLOR_BUFFER_BIT: buffers that are currently enabled for color writing
         ; GL_DEPTH_BUFFER_BIT: the depth buffer
         (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
         
         ; Push the current matrix, GL_MODELVIEW, onto the stack
         (glMatrixMode GL_MODELVIEW)
         (glPushMatrix)
         ; Multiply the current matrix, GL_MODELVIEW, by a genereal scaling matrix
         (glScaled zoom zoom zoom)
         ; Rotate the current matrix at an angle along given vector coords
         (glRotated y-rotation 1 0 0)
         (glRotated x-rotation 0 1 0)
         
         ; Draw the model!
         (draw)
         
         ; Pop the GL_MODELVIEW matrix off the stack
         (glPopMatrix)))
      
      (swap-gl-buffers))
    
    
    ;; Variables for key and mouse events
    (define handle-motion void)
    (define x-rotation 0)
    (define y-rotation 0)
    (define zoom 1)
    
    (define/override (on-event event)
      (let ((x (send event get-x))
            (y (send event get-y)))
        (case (send event get-event-type)
          [(left-down)
           (set! handle-motion
                 (let ((old-x x) (old-y y))
                   (lambda (new-x new-y)
                     (set! x-rotation (+ x-rotation (- new-x old-x)))
                     (set! y-rotation (+ y-rotation (- new-y old-y)))
                     (set! old-x new-x)
                     (set! old-y new-y)
                     (refresh))))]
          [(left-up)
           (set! handle-motion void)]
          [(motion) (handle-motion x y)])))
    
    (define/override (on-char event)
      (case (send event get-key-code)
        ((#\+) (set! zoom (* zoom 4/3)) (refresh))
        ((#\-) (set! zoom (/ zoom 4/3)) (refresh))
        ((wheel-up) (set! zoom (* zoom 9/8)) (refresh))
        ((wheel-down) (set! zoom (/ zoom 9/8)) (refresh))))))


(define (show-gl-info frame canvas)
  (define-values (renderer version vendor)
    (send canvas with-gl-context
          (lambda () 
            (values
             (glGetString GL_RENDERER)
             (glGetString GL_VERSION)
             (glGetString GL_VENDOR)))))
  (define label
    (format "RENDERER: ~a~%VERSION: ~a~%VENDOR: ~a"
            renderer version vendor))
  (define dialog (new dialog% [parent frame] [label "OpenGL info"]))          
  (define msg (new message%
                   [parent dialog]
                   [label label]))
  (define extensions-list (new list-box% 
                               [parent dialog] 
                               [label "EXTENSIONS:"]
                               [style '(single vertical-label)]
                               [choices
                                (sort
                                 (for/list ((ext (in-set (gl-extensions))))
                                   (symbol->string ext))
                                 string<?)]))
  (send dialog show #t))

;; START HERE
;; view is called by shader.rkt
(define (view model-render [init-resources void])
  ;(define es (make-eventspace)) ;by mf
  ;(parameterize ((current-eventspace es)) ; by mf
    
    ;; Create a frame to hold our canvas
    (define frame 
      (new frame% 
           [label "OpenGL viewer"]
           [width 300]
           [height 300]))
    
    ;; Define our custom menu bar
    (define menubar
      (new menu-bar% [parent frame]))
    
    ;; Define custom help menu
    (define help-menu
      (new menu% [parent menubar] [label "&My Help Menu"]))
    
    ;; Create our canvas with gl related setup
    (define canvas
      (new gl-viewer% 
           (style '(gl no-autoclear)) 
           (parent frame) 
           (draw model-render) 
           (setup init-resources)))
    
    ;; Add our custom item to the help menu
    (define gl-info-item
      (new menu-item% [parent help-menu] [label "GL info"]
           [callback (lambda (i e) (show-gl-info frame canvas))]))
    
    ;; Show the app 
    (send frame show #t))