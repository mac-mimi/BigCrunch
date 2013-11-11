#lang racket/base
(require racket/vector
         racket/math
         math/matrix
         ffi/vector
         ffi/unsafe)
(provide identity-mat4
         translate
         lookat
         perspective
         rotate
         matrix->f64vector
         matrix->f32vector)

;; A 4x4 identity matrix with floats
(define identity-mat4 (matrix-scale (identity-matrix 4) 1.0))


;; [Matrix 4x4] [Vector 3] -> [Matrix 4x4]
;; Multiplies the translation matrix to the given matrix
(define (translate m4 v3)
  (matrix-augment
   (list
    (matrix-col m4 0)
    (matrix-col m4 1)
    (matrix-col m4 2)
    (matrix+ (matrix-col m4 3)
             (vec3->col4 v3)))))


;; [Vector 3] -> [Matrix 4x1]
;; Converts the given vector to a column matrix
;; with 0 in the last row
(define (vec3->col4 vec)
  (vector->matrix 4 1 (vector-append vec #(0.0))))


;; [Vector 3] -> [Matrix 1x4]
;; Converts the given vector to a row matrix
;; with 0 in the last column
(define (vec3->row4 vec)
  (vector->matrix 1 4 (vector-append vec #(0.0))))


;; Number Number Number Number -> [Matrix 4x4]
;; Sets up the perspective projection matrix
;; fov: field of view angle, in degrees, in the y direction
;; aspect: aspect ratio that determines the fov in the x direction (width:height)
;; znear: distance from the viewer to the near clipping plane (always positive)
;; zfar: distance from the viewer to the far clipping plane (always positive)
(define (perspective fov aspect znear zfar)
  (define h (/ 1.0 (tan (* fov (/ pi 360)))))
  (define neg-depth (- znear zfar))
  ;(define neg-depth (- zfar znear))
  (list->matrix
   4 4
   (list (/ h aspect) 0.0 0.0 0.0
         0.0 h 0.0 0.0
         0.0 0.0 (/ (+ zfar znear) neg-depth) (/ (* 2.0 znear zfar) neg-depth)
         0.0 0.0 -1.0 0.0)))


;; [Vector 3] [Vector 3] -> [Vector 3]
;; Subtracts vec2 from vec1
(define (vector- vec1 vec2)
  (vector
   (- (vector-ref vec1 0) (vector-ref vec2 0))
   (- (vector-ref vec1 1) (vector-ref vec2 1))
   (- (vector-ref vec1 2) (vector-ref vec2 2))))


;; [Vector 3] -> [Vector 3]
;; Negates the given vector
(define (vector-negate vec)
  (vector- #(0.0 0.0 0.0) vec))


;; [Vector 3] -> [Vector 3]
;; Normalize the given vector
(define (vector-normalize vec3)
  (matrix->vector (matrix-normalize(vector->matrix 1 3 vec3))))


;; [Vector 3] [Vector 3] -> [Vector 3]
;; Perform cross product on the given vectors vec1 x vec2
(define (vector* vec1 vec2)
  (vector
   (- (* (vector-ref vec1 1) (vector-ref vec2 2))
      (* (vector-ref vec1 2) (vector-ref vec2 1)))
   (- (* (vector-ref vec1 2) (vector-ref vec2 0))
      (* (vector-ref vec1 0) (vector-ref vec2 2)))
   (- (* (vector-ref vec1 0) (vector-ref vec2 1))
      (* (vector-ref vec1 1) (vector-ref vec2 0)))))


;; [Vector 3] [Vector 3] [Vector 3] -> [Vector 3]
;; Creates a viewing matrix
;; eye-vec:    position of the camera
;; center-vec: point that the camera is looking at
;; up-vec:     direction of the up vector. ie top of the camera in 
;;             case the camera is tilted. 
(define (lookat eye-vec center-vec up-vec)
  (define norm-forward
    (vector-normalize (vector- center-vec eye-vec))) 
  (define norm-side
    (vector-normalize (vector* norm-forward (vector-normalize up-vec))))
  (define up
    (vector* norm-side norm-forward))
  (matrix*
   (matrix-stack (list
                  (vec3->row4 norm-side)
                  (vec3->row4 up)
                  (vec3->row4 (vector-negate norm-forward))
                  (row-matrix [0.0 0.0 0.0 1.0])))
   (translate identity-mat4 (vector-negate eye-vec))))
;; (lookat #(0.0 2.0 0.0) #(0.0 0.0 -4.0) #(0.0 1.0 0.0))


;; [Matrix 4x4] Degrees [Vector 3] -> [Matrix 4x4]
(define (rotate m angle v)
  (define a (* angle (/ pi 180.0)))
  (define s (sin a))
  (define c (cos a))
  (define t (- 1.0 c))
  
  (define axis (vector-normalize v))
  (define x (vector-ref axis 0))
  (define y (vector-ref axis 1))
  (define z (vector-ref axis 2))
  
  (define tx (* t x))
  (define ty (* t y))
  (define tz (* t z))
  
  (define sx (* s x))
  (define sy (* s y))
  (define sz (* s z))
  
  (matrix-augment (list
                   (col-matrix 
                    [(+ (* tx x) c)
                     (+ (* tx y) sz)
                     (- (* tx z) sy)
                     0])
                   (col-matrix
                    [(- (* tx y) sz)
                     (+ (* ty y) c)
                     (+ (* ty z) sx)
                     0])
                   (col-matrix
                    [(+ (* tx z) sy)
                     (- (* ty z) sx)
                     (+ (* tz z) c)
                     0])
                   (col-matrix 
                    [0.0 
                     0.0 
                     0.0 
                     1.0]))))


;; [Matrix rxc] -> f32Vector
;; Coneverts the given matrix to an f32vector
(define (matrix->f32vector m)
  (list->f32vector (matrix->list m)))


;; [Matrix rxc] -> f64Vector
;; Converts the given matrix to an f64vector
(define (matrix->f64vector m)
  (list->f64vector (matrix->list m)))
