#lang racket/base
(require racket/bool
         opengl
         ffi/vector
         ffi/unsafe
         math/matrix
         "viewer.rkt"
         "3d-math.rkt"
         "shader.rkt")

(define vbo-cube-vertices-id #f) ; GLuint
(define vbo-cube-colors-id #f)   ; GLuint
(define ibo-cube-elements-id #f) ; GLuint
(define program-id #f)           ; GLuint

(define attribute-coord3d #f)    ; GLint
(define attribute-v-color #f)    ; GLint
(define uniform-mvp #f)          ; GLint


;; ==============================================================================
;; INIT RESOURCES
;; ==============================================================================
;; Store vertices in Vertex Buffer Object of type GL_ARRAY_BUFFER
;; Store vertices in Index Buffer Object of type GL_ELEMENT_ARRAY_BUFFER
;; GLuint id;
;; glGenBuffers(1, &id);
;; glBindBuffer(type, id);
;; glBufferData(type, sizeof(bvector), bvector, GL_STATIC_DRAW);
(define (allocate-buffer-object size type vector-ptr)
  ;; Generate 1 buffer object
  (define buff-obj (glGenBuffers 1)) 
  ;; Give buff-id the address of the buffer object
  (define buff-id (u32vector-ref buff-obj 0))
  ;; Specify the type of buffer that buff-id is associated with
  (glBindBuffer type buff-id)
  ;; Create and initialise the buffer objects data store
  (glBufferData type size vector-ptr GL_STATIC_DRAW)
  ;; Return the buffer id that has a type and memory binded to it
  buff-id)


(define (init-resources)
  ;; GLfloat
  ;; Vertices of cube
  (define vbo-cube-vertices
    (f32vector
     ;; Front
     ;; Starting bottom left corner and anti-clockwise
     -1.0 -1.0 +1.0 
     +1.0 -1.0 +1.0
     +1.0 +1.0 +1.0
     -1.0 +1.0 +1.0
     ;; Back
     ;; Starting bottom left corner and anti-clockwise
     -1.0 -1.0 -1.0
     +1.0 -1.0 -1.0
     +1.0 +1.0 -1.0
     -1.0 +1.0 -1.0))
  
  (set! vbo-cube-vertices-id
        (allocate-buffer-object
         (gl-vector-sizeof vbo-cube-vertices) ; 96 bytes
         GL_ARRAY_BUFFER
         (f32vector->cpointer vbo-cube-vertices)))
  
  ;; GLfloat
  ;; Colors for corresponding vertices
  (define vbo-cube-colors
    (f32vector
     ;; Front colors
     ;; Starting bottom left corner and anti-clockwise
     1.0 0.0 0.0  ; At (-1 -1 +1)
     0.0 1.0 0.0  ; At (+1 -1 +1)
     0.0 0.0 1.0  ; At (+1 +1 +1)
     1.0 1.0 1.0  ; At (-1 +1 +1)
     ;; Back colors
     ;; Starting bottom left corner and anti-clockwise
     1.0 0.0 0.0  ; At (-1 -1 +1)
     0.0 1.0 0.0  ; At (+1 -1 -1)
     0.0 0.0 1.0  ; At (+1 +1 -1)
     1.0 1.0 1.0)); At (-1 +1 -1)
  
  (set! vbo-cube-colors-id
        (allocate-buffer-object
         (gl-vector-sizeof vbo-cube-colors) ; 96 bytes
         GL_ARRAY_BUFFER
         (f32vector->cpointer vbo-cube-colors)))
  
  ;;    7_ _ _ _6
  ;;   /|      /|
  ;; 3/_|_ _ 2/ |
  ;; | 4|_ _ |_5|
  ;; | /     | / 
  ;;0|/_ _ _1|/
  
  ;; GLushort
  ;; Specify triangle faces counter-clockwise
  ;; Indices that refer to the vbo-cube-vertices
  (define ibo-cube-elements
    (u16vector   
     ;; Front
     0 1 2
     2 3 0
     ;; Right
     1 5 6
     6 2 1
     ;; Back
     7 6 5
     5 4 7
     ;; Left
     4 0 3
     3 7 4
     ;; Bottom 
     4 5 1
     1 0 4
     ;; Top
     3 2 6
     6 7 3
     ))
  
  (set! ibo-cube-elements-id
        (allocate-buffer-object
         (gl-vector-sizeof ibo-cube-elements) ; 72 bytes
         GL_ELEMENT_ARRAY_BUFFER
         (u16vector->cpointer ibo-cube-elements)))
  
  (setup-shader)
  (bind-attributes))


(define (setup-shader)
  (if (or (gl-version-at-least? '(2 0))
          (gl-has-extension? 'GL_ARB_shader_objects))
      (set! program-id (make-shader-program "cube.v.glsl" "cube.f.glsl"))
      (printf "This OpenGL does not support shaders, you'll get a plain white rectangle.~%"))
  
  ;; Check that the shader program is made properly
  (if (glGetProgramiv program-id GL_LINK_STATUS)
      (printf "Shader program compiled successfully\n")
      (printf "Shader program did not compile successfully\n")))


;; Bind attributes (names) to shader program and 
;; save the location of each attribute
(define (bind-attributes)    
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
;; Draw this every tick
(define (draw)
  (glClearColor 1.0 1.0 1.0 1.0)
  
  ;; Clear the buffers to preset values:
  ;; GL_COLOR_BUFFER_BIT: buffers that are currently enabled for color writing
  ;; GL_DEPTH_BUFFER_BIT: the depth buffer
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  
  (when program-id (glUseProgram program-id))
  
  ;; Describe our vertices array to OpenGL
  ;; Bind the attributes to the vertices
  (glEnableVertexAttribArray attribute-coord3d)
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
  (glBindBuffer GL_ARRAY_BUFFER vbo-cube-colors-id)
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
  
  ;; Get size of ibo-cube-elements
  (define buf-params (glGetBufferParameteriv GL_ELEMENT_ARRAY_BUFFER GL_BUFFER_SIZE))
  (define buffer-size (s32vector-ref buf-params 0)) ; 144 bytes
  
  ;; Draw the elements specified by ibo-cube-elements
  (glDrawElements GL_TRIANGLES
                  (/ buffer-size (compiler-sizeof 'short)) ; 36 bytes
                  GL_UNSIGNED_SHORT
                  0)
  
  (glDisableVertexAttribArray attribute-coord3d)
  (glDisableVertexAttribArray attribute-v-color)
  
  ;; Do the math
  (define angle
    (* (/ (current-elapsed-time) 1000.0) 
       45.0)) ; 45 degrees/sec
  
  (define anim
    (rotate identity-mat4 
            angle
            #(0.0 1.0 0.0)))
  
  (define model
    (translate identity-mat4 
               #(0.0 0.0 -4.0)))
  
  (define view
    (lookat #(0.0 2.0 0.0)   ; position of camera
            #(0.0 0.0 -4.0)  ; point camera is looking at
            #(0.0 1.0 0.0))) ; direction of up vector
  
  (define projection
    (perspective 45.0                             ; field of view in y dir 
                 (* 1.0                           ; aspect ratio
                    (/ (current-screen-width) 
                       (current-screen-height))) 
                 3.0                               ; znear
                 10.0))                            ; zfar
  
  ;; Global Transformation Matrix
  (define mvp
    (matrix* projection view model anim))
  
  (glUseProgram program-id)
  (glUniformMatrix4fv uniform-mvp 1 false (matrix->f32vector mvp)))

;; ==============================================================================
;; VIEW
;; ==============================================================================
(module+ main
  (view draw init-resources))
