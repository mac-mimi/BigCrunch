#lang racket

(require opengl ffi/vector ffi/unsafe) 
(provide make-shader-program)

;; [input port] -> [program id]
;; Compile all shaders and create a full shader program.
(define (make-shader-program vertex-shader-file fragment-shader-file)
  (define prog-id (glCreateProgram))
  (define vsh-id (make-a-shader GL_VERTEX_SHADER vertex-shader-file)) 
  (define fsh-id (make-a-shader GL_FRAGMENT_SHADER fragment-shader-file))
  (glAttachShader prog-id vsh-id)    
  (glAttachShader prog-id fsh-id)
  (glLinkProgram prog-id)
  prog-id)

;; [input port for src code] [shader type] [shader src] -> [shader id]
;; Compiles one shader
(define (make-a-shader shader-type shader-src)
  (define sh-id (glCreateShader shader-type))
  ;(bind-to-id-src sh-id (open-input-file shader-src))
  (call-with-input-file shader-src (lambda (port) (bind-id-to-src sh-id port)))
  (glCompileShader sh-id)
  (if (glGetShaderiv sh-id GL_COMPILE_STATUS) 
      (printf (string-append "compiled shader " shader-src "\n")) 
      (printf (string-append "Did not compile shader " shader-src "\n")))
  sh-id)

;; shader-id input-port -> string
;; Bind the source code to the shader id
(define (bind-id-to-src shader-id port)
  (define src-code (for/vector ((line (in-lines port))) line))
  (define sizes (for/list ([line (in-vector src-code)]) (string-length line)))
  (define vsizes (list->s32vector sizes))
  (glShaderSource shader-id (vector-length src-code) src-code vsizes))
