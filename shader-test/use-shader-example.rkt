#lang racket/gui
(require opengl)
(require ffi/vector)
(require "viewer.rkt")

;; shader-id input-port -> string
;; Bind the source code to the shader id
(define (bind-id-to-src shader-id port)
  (define src-code (for/vector ((line (in-lines port))) line))
  (define sizes (for/list ([line (in-vector src-code)]) (string-length line)))
  (define vsizes (list->s32vector sizes))
  (glShaderSource shader-id (vector-length src-code) src-code vsizes))

;; [input port] -> [program id]
;; Compile shaders and create a full shader program.
;; 1. Get a program id prog-id
;; 2. Get an id for each shader 
;; e.g. vertex shader id vsh-id 
;; e.g. fragment shader if fsh-id
;; 3. Bind source code of each shader to its relevant ID
;; 4. Compile each shader
;; 5. Attach each shader ID to the program ID
;; 6. Link the shaders in the program
;; 7. Return the program ID that's ready to be used before rendering
(define (make-shader-program)
  (define prog-id (glCreateProgram))
  ;(vsh-id (make-a-shader GL_VERTEX_SHADER "vshader.vsh"))
  (define fsh-id (make-a-shader GL_FRAGMENT_SHADER "test.glsl"))
  ;(glAttachShader prog-id vsh-id)    
  (glAttachShader prog-id fsh-id)
  (glLinkProgram prog-id)
  prog-id)

;; [input port for src code] [shader type] [shader src] -> [shader id]
;; Compiles a shader
(define (make-a-shader shader-type shader-src)
  (define sh-id (glCreateShader shader-type))
  (call-with-input-file shader-src (lambda (port) (bind-id-to-src sh-id port)))
  (glCompileShader sh-id)
  (if (glGetShaderiv sh-id GL_COMPILE_STATUS) 
      (printf (string-append "compiled shader " shader-src "\n")) 
      (printf (string-append "Did not compile shader " shader-src "\n")))
  sh-id)
  

(define program-id #f)
(define attribute_coord2d #f)
;; Initialise resources if the opengl version on
;; the system is valid. 
(define (init-resources)
  (if (or (gl-version-at-least? '(2 0))
          (gl-has-extension? 'GL_ARB_shader_objects))
    (set! program-id (make-shader-program))
    (printf "This OpenGL does not support shaders, you'll get a plain white rectangle.~%")))

(define (model-render)
  ; the coordinates
  #|
  (define vertex-array
    (f64vector -0.5 -0.5
               0.5 -0.5
               0.5 0.5
               -0.5 0.5))
|#

  (define s 0.5)
  (define -s (* s -1))
       
  (define vertex-array
    (f64vector  ; FRONT
                s -s  s
                s  s  s
               -s  s  s
               -s -s  s
               ; BACK
                s -s -s
                s  s -s
               -s  s -s
               -s -s -s
               ; TOP
                s  s  s
                s  s -s
               -s  s -s
               -s  s  s
               ; Bottom
                s -s  s
                s -s -s
               -s -s -s
               -s -s  s
               ; Right
                s  s  s 
                s -s  s
                s -s -s
                s  s -s
               ; Left
               -s  s  s
               -s -s  s
               -s -s -s
               -s  s -s
               ))

  #|
  (define texcoord-array
    (f64vector 0 0
               0.5 0
               0.5 0.5
               0 0.5))
|#

  (when program-id
    (glUseProgram program-id))

  ; Let's be "modern" and use the array functions (introduced in OpenGL 1.1).
  ; Note that you need to ask GL everything 3 times:
  ; 1. Here is an array I'd like you to draw...
  (let-values (((gltype cptr) (gl-vector->type/cpointer vertex-array)))
    (glVertexPointer 3 gltype 0 cptr)) ; Arg0: Number of components per vertex (e.g. 3 for 3D coords)
                                       ; Arg1: GL type for the components eg. float
                                       ; Arg2: stride?
                                       ; Arg3: pointer to vertex array
  
  #|
  (let-values (((gltype cptr) (gl-vector->type/cpointer texcoord-array)))
    (glTexCoordPointer 2 gltype 0 cptr))
  |#
  
  ;; Tells OGL that we are feeding it a vertex array from the
  ;; client side (the application) rather than the server, which is a graphics card. 
  (glEnableClientState GL_VERTEX_ARRAY)
  ;(glEnableClientState GL_TEXTURE_COORD_ARRAY)
  
  
  (glDrawArrays GL_QUADS 0 24)     ; Arg0: Telling OGL we're drawing triangles
                                  ; GL_QUADS, GL_TRIANGLE, GL_LINES
                                  ; Arg1: Where we want to start in the vertex array
                                  ; Arg2: How many vertices we want to draw

  ; Clean up state.
  ;(glDisableClientState GL_TEXTURE_COORD_ARRAY)
  (glDisableClientState GL_VERTEX_ARRAY)
  (when program-id
    (glUseProgram 0)))

  



(view model-render init-resources)