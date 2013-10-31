#lang racket/base
(require racket/vector
         racket/math
         math/matrix
         ffi/vector
         ffi/unsafe)
(provide identity-mat4
         rotate
         translate
         lookat
         perspective
         matrix->f64vector
         matrix->f32vector)

(define identity-mat4 (identity-matrix 4))

(define (rotate m angle v)
  (define a angle)
  (define c (cos a))
  (define s (cos a))
  (define axis (vector-normalize v))
  (define temp (vector* (vector (- 1 c) (- 1 c) (- 1 c)) axis))
  (define Rotate
    (matrix
     [[(+ c (vector-ref temp 0) (vector-ref axis 0))
       (+ 0 (vector-ref temp 0) (vector-ref axis 1) (* +1 s (vector-ref axis 2)))
       (+ 0 (vector-ref temp 0) (vector-ref axis 2) (* -1 s (vector-ref axis 1)))
       0]
      [(+ 0 (vector-ref temp 1) (vector-ref axis 0) (* -1 s (vector-ref axis 2)))
       (+ c (vector-ref temp 1) (vector-ref axis 1))
       (+ 0 (vector-ref temp 1) (vector-ref axis 2) (* +1 s (vector-ref axis 0)))
       0]
      [(+ 0 (vector-ref temp 2) (vector-ref axis 0) (* +1 s (vector-ref axis 1)))
       (+ 0 (vector-ref temp 2) (vector-ref axis 1) (* -1 s (vector-ref axis 0)))
       (+ c (vector-ref temp 2) (vector-ref axis 2))
       0]
      [0 0 0 0]]))
  (define Result
    (matrix-stack 
     (list
      (matrix+ (matrix-scale (matrix-row m 0) (matrix-ref Rotate 0 0))
               (matrix-scale (matrix-row m 1) (matrix-ref Rotate 0 1))
               (matrix-scale (matrix-row m 2) (matrix-ref Rotate 0 2)))
      (matrix+ (matrix-scale (matrix-row m 0) (matrix-ref Rotate 1 0))
               (matrix-scale (matrix-row m 1) (matrix-ref Rotate 1 1))
               (matrix-scale (matrix-row m 2) (matrix-ref Rotate 1 2)))
      (matrix+ (matrix-scale (matrix-row m 0) (matrix-ref Rotate 2 0))
               (matrix-scale (matrix-row m 1) (matrix-ref Rotate 2 1))
               (matrix-scale (matrix-row m 2) (matrix-ref Rotate 2 2)))
      (matrix-row m 3))))
  Result
  m)

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
;;
;; fov: field of view angle, in degrees, in the y direction
;; aspect: aspect ratio that determines the fov in the x direction (width:height)
;; znear: distance from the viewer to the near clipping plane (always positive)
;; zfar: distance from the viewer to the far clipping plane (always positive)
(define (perspective fov aspect znear zfar)
  (define h (/ 1.0 (tan (* fov (/ pi 360)))))
  (define neg-depth (- znear zfar))
  (list->matrix
   4 4
   (list (/ h aspect) 0.0 0.0 0.0
         0.0 h 0.0 0.0
         0.0 0.0 (/ (+ zfar znear) neg-depth) (/ (* 2.0 znear zfar) neg-depth)
         0.0 0.0 -1.0 0.0)))

;;(perspective 60.0 (/ 305.0 300.0) 0.2 12.0)

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
;;
;; eye-vec: position of the eye point
;; center-vec: position of the point that the eye is looking at
;; up-vec: direction of the up vector
(define (lookat eye-vec center-vec up-vec)
  (define norm-forward
    (vector-normalize (vector- center-vec eye-vec))) ;ch
  (define norm-side
    (vector-normalize (vector* norm-forward (vector-normalize up-vec))))
  (define up
    (vector* norm-side norm-forward))
  (matrix*
   (matrix-stack (list
                  (vec3->row4 norm-side) ; correct
                  (vec3->row4 up)
                  (vec3->row4 (vector-negate norm-forward))
                  (row-matrix [0.0 0.0 0.0 1.0])))
   (translate identity-mat4 (vector-negate eye-vec))))
;; (lookat #(0.0 2.0 0.0) #(0.0 0.0 -4.0) #(0.0 1.0 0.0))

;; [Matrix rxc] -> f32Vector
;; Coneverts the given matrix to an f32vector
(define (matrix->f32vector m)
  (list->f32vector (matrix->list m)))

;; [Matrix rxc] -> f64Vector
;; Converts the given matrix to an f64vector
(define (matrix->f64vector m)
  (list->f64vector (matrix->list m)))
