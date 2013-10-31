#lang racket/base
(require racket/bool
         opengl
         ffi/vector
         ffi/unsafe
         math/matrix
         "viewer.rkt"
         "3d-math.rkt"
         "shader.rkt")

(define vbo-cube-vertices-id #f)
(define vbo-cube-colors-id #f)
(define ibo-cube-elements-id #f)
(define program-id #f)

(define attribute-coord3d #f)
(define attribute-v-color #f)
(define uniform-mvp #f)

;; ==============================================================================
;; INIT RESOURCES
;; ==============================================================================
;; ISSUE HERE!!!!
(define (allocate-buffer bvector size type)
  (define new-bufs (glGenBuffers 1))
  (define id (u32vector-ref new-bufs 0))
  (define id-ty (gl-vector->type new-bufs))
  (define gl-ptr (gl-vector->cpointer bvector))
  (glBindBuffer type id)
  (glBufferData type size gl-ptr GL_STATIC_DRAW)
  (define-syntax-rule (debug e ...)
    (eprintf "~v\n" (vector (cons 'e e) ...)))
  (debug bvector size type
         new-bufs (gl-vector? new-bufs)
         (u32vector->list new-bufs)
         id GL_UNSIGNED_INT id-ty (gl-type? id-ty) gl-ptr)
  id)

(define (init-resources)

  (define s  1.0)
  (define -s (- s))
  ;; GLfloat
  (define vbo-cube-vertices
    (f64vector
     ;; Front
     -s -s  s
     s -s  s
     s  s  s
     -s  s  s
     ;; Back
     -s -s -s
     s -s -s
     s  s -s
     -s  s -s))
  (set! vbo-cube-vertices-id
        (allocate-buffer
         vbo-cube-vertices
         ;; 96 bytes
         (* (f64vector-length vbo-cube-vertices) (compiler-sizeof 'float))
         GL_ARRAY_BUFFER))
  ;;(printf (string-append "sizeof(cube_vertices): " (number->string (* (f64vector-length vbo-cube-vertices) (compiler-sizeof 'float)))))

  (define vbo-cube-colors
    (f64vector
     ;; Front colors
     1.0 0.0 0.0
     1.0 0.0 0.0
     1.0 0.0 0.0
     1.0 0.0 0.0
     ;; Back colors
     0.0 0.0 1.0
     0.0 0.0 1.0
     0.0 0.0 1.0
     0.0 0.0 1.0))
  (set! vbo-cube-colors-id
        (allocate-buffer
         vbo-cube-colors
         ;; 96 bytes
         (* (f64vector-length vbo-cube-vertices) (compiler-sizeof 'float))
         GL_ARRAY_BUFFER))
  ;;(printf (string-append "sizeof(cube_colors): " (number->string (* (f64vector-length vbo-cube-vertices) (compiler-sizeof 'float)))))

  (define ibo-cube-elements
    (u32vector
     ;; Front
     0 1 2
     2 3 0
     ;; Top
     1 5 6
     6 2 1
     ;; Back
     7 6 5
     5 4 7
     ;; Bottom
     4 0 3
     3 7 4
     ;; Left
     4 5 1
     1 0 4
     ;; Right
     3 2 6
     6 7 3))
  (set! ibo-cube-elements-id
        (allocate-buffer
         ibo-cube-elements
         ;; 72 bytes
         (* (u32vector-length ibo-cube-elements) (compiler-sizeof 'short))
         GL_ELEMENT_ARRAY_BUFFER))
  ;;(printf (string-append "sizeof(cube_elements): " (number->string (* (u32vector-length ibo-cube-elements) (compiler-sizeof 'short)))))

  ;; Shaders
  (if (or (gl-version-at-least? '(2 0))
          (gl-has-extension? 'GL_ARB_shader_objects))
    (set! program-id (make-shader-program "cube.v.glsl" "cube.f.glsl"))
    (printf "This OpenGL does not support shaders, you'll get a plain white rectangle.~%"))

  ;; Check that the shader program is made properly
  (if (glGetProgramiv program-id GL_LINK_STATUS)
    (printf "Shader program compiled successfully\n")
    (printf "Shader program did not compile successfully\n"))

  ;; Bind attributes to shader program
  (set! attribute-coord3d (glGetAttribLocation program-id "coord3d"))
  (if attribute-coord3d
    (printf "Successfully binded attribute coord3d\n")
    (printf "Did not successfully bind attribute coord3d\n"))

  (set! attribute-v-color (glGetAttribLocation program-id "v_color"))
  (if attribute-v-color
    (printf "Successfully binded attribute v_color\n")
    (printf "Did not successfully bind attribute v_color\n"))

  (set! uniform-mvp (glGetUniformLocation program-id "mvp"))
  (if uniform-mvp
    (printf "Succesfully binded uniform mvp\n")
    (printf "Did not successfully bind uniform mvp\n")))

;; ==============================================================================
;; DRAW
;; ==============================================================================

(define (draw)
  (when program-id
    (glUseProgram program-id))
  (glEnableVertexAttribArray attribute-coord3d)

  ;; Describe our vertices array to OpenGL
  ;; OpenGL CANNOT guess its format automatically
  (glBindBuffer GL_ARRAY_BUFFER vbo-cube-vertices-id)
  (glVertexAttribPointer
   attribute-coord3d ; Attribute
   3                 ; Number of elements per vertex (x, y, z)
   GL_FLOAT          ; The type of each element
   false             ; Take our vertices as is
   0                 ; No extra data between each position
   0                 ; Offset of first element
   )

  (glEnableVertexAttribArray attribute-v-color)
  (glBindBuffer GL_ARRAY_BUFFER vbo-cube-vertices-id)
  (glVertexAttribPointer
   attribute-v-color ; Attribute
   3                 ; Number of elements per vertex (r, g, b)
   GL_FLOAT          ; The type of each element
   false             ; Take our values as is
   0                 ; No extra data between each position
   0                 ; Offset of first element
   )

  ;; Push each element in cube vertices to the vertex shader
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo-cube-elements-id)

  ;; buffer-size is 144
  (let ((buffer-size (s32vector-ref (glGetBufferParameteriv GL_ELEMENT_ARRAY_BUFFER GL_BUFFER_SIZE) 0)))
    (glDrawElements GL_TRIANGLES
                    ;; Checked: 2
                    (/ buffer-size (compiler-sizeof 'short))
                    GL_UNSIGNED_SHORT
                    0))
  ;; GLushort is the same size as 'short

  (glDisableVertexAttribArray attribute-coord3d)
  (glDisableVertexAttribArray attribute-v-color)

  ;; Do the math
  (define model (translate identity-mat4 #(0 0 -4)))
  (define view (lookat #(0.0 2.0 0.0) #(0.0 0.0 -4.0) #(0.0 1.0 0.0)))
  (define projection (perspective 45.0 0.5 0.1 10.0))
  (define mvp (matrix* projection view model))
  (glUseProgram program-id)
  (glUniformMatrix4fv uniform-mvp 1 false (matrix->f32vector mvp)))

;; ==============================================================================
;; VIEW
;; ==============================================================================
(module+ main
  (view draw init-resources))
