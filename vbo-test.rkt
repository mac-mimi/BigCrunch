#lang racket
(require opengl ffi/vector ffi/unsafe "shader-test/viewer.rkt")
#|
	public struct Vector3f : IEquatable<Vector3f>
	{
		public float x, y, z;
 
		public static readonly int ByteSize = Marshal.SizeOf (new Vector3f ());
 
		public Vector3f (float x, float y, float z)
		{
			this.x = x;
			this.y = y;
			this.z = z;
		}
 
		public bool Equals (Vector3f other)
		{
			return x == other.x && y == other.y && z == other.z;
		}
	}
|#

(define elementCount 0) ;int

(define rotationAngle 0);double

(define cube-vertex-id #f)
(define cube-normal-id #f)
(define cube-color-id #f)
(define indecies-vbo-id #f)
(define element-count #f)

(define (allocate-buffer vector vector->size type)
  (define id (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer type id)
  (glBufferData type (vector->size vector) vector  GL_STATIC_DRAW)
  id)

(define (init) 
  (glClearColor 0.0 0.0 0.3 0.0);;DarkBlue
  (define s  1.0)
  (define -s (- s))
  (define cube-vertex-data
    (f64vector -s -s  s
               s -s  s
               s  s  s
               -s  s  s
               -s -s -s
               s -s -s
               s  s -s
               -s  s -s))
  (define cube-normal-data
    (f64vector -s -s  s
               s -s  s
               s  s  s
               -s  s  s
               -s -s -s
               s -s -s
               s  s -s
               -s  s -s))
  (define cube-color-data
    (s32vector #xff000
               #xff000
               #x33cc33
               #x33cc33
               #xff000
               #xff000
               #x33cc33
               #x33cc33))
  (define indecies-vbo-data
    (u32vector  
     0 1 2 2 3 0 
     3 2 6 6 7 3 
     7 6 5 5 4 7 
     4 0 3 3 7 4 
     0 1 5 5 4 0
     1 5 6 6 2 1))
  
  (define bufferSize #f)
  
  ;; color-data ;;
  (set! cube-color-id (allocate-buffer cube-color-data s32vector-length GL_ARRAY_BUFFER))
  #|
TODO error checks
  ;; Validate that the buffer is the correct size
  (glGetBufferParameteriv GL_ARRAY_BUFFER BufferParameterName.BufferSize, out bufferSize);
 
				if (colorData.Length * sizeof(int) != bufferSize)
					throw new ApplicationException ("Vertex array not uploaded correctly");
 
				// Clear the buffer Binding
				GL.BindBuffer (BufferTarget.ArrayBuffer, 0);
			}
|#
  
  
  
  ;; Normal Array Buffer ;;
  (set! cube-normal-id (allocate-buffer cube-normal-data f64vector-length GL_ARRAY_BUFFER))
  
  #|
  // Validate that the buffer is the correct size
  GL.GetBufferParameter (BufferTarget.ArrayBuffer, BufferParameterName.BufferSize, out bufferSize);
  if (cubeNormalData.Length * Vector3f.ByteSize != bufferSize)
  throw new ApplicationException ("Normal array not uploaded correctly");
  
  // Clear the buffer Binding
  GL.BindBuffer (BufferTarget.ArrayBuffer, 0);
  }
|#
  
  ;; Vertex Array Buffer
  (set! cube-vertex-id (allocate-buffer cube-vertex-data f64vector-length GL_ARRAY_BUFFER))
  #|
    // Validate that the buffer is the correct size
    GL.GetBufferParameter (BufferTarget.ArrayBuffer, BufferParameterName.BufferSize, out bufferSize);
    if (cubeVertexData.Length * Vector3f.ByteSize != bufferSize)
    throw new ApplicationException ("Vertex array not uploaded correctly");
    
    // Clear the buffer Binding
    GL.BindBuffer (BufferTarget.ArrayBuffer, 0);
    }
|#
  
  ; Element Array Buffer
  (set! indecies-vbo-id (allocate-buffer indecies-vbo-data u32vector-length GL_ELEMENT_ARRAY_BUFFER))
  #|
    // Validate that the buffer is the correct size
    GL.GetBufferParameter (BufferTarget.ElementArrayBuffer, BufferParameterName.BufferSize, out bufferSize);
    if (indicesVboData.Length * sizeof(int) != bufferSize)
    throw new ApplicationException ("Element array not uploaded correctly");
    
    // Clear the buffer Binding
    GL.BindBuffer (BufferTarget.ElementArrayBuffer, 0);
    }
|#
  (set! element-count (u32vector-length indecies-vbo-data))
  ;;TODO blow up on error
  )
(define (draw)
  ;; Color Array Buffer (Colors not used when lighting is enabled)
  
  ;; Bind to the Array Buffer ID
  (glBindBuffer GL_ARRAY_BUFFER cube-color-id)
  
  ;; Set the Pointer to the current bound array describing how the data ia stored
  (glColorPointer 4 GL_UNSIGNED_BYTE (ctype-sizeof _uint32) 0)
  
  ;; Enable the client state so it will use this array buffer pointer
  (glEnableClientState GL_COLOR_ARRAY)
  
  
  ;; Vertex Array Buffer
  
  ;; Bind to the Array Buffer ID
  (glBindBuffer GL_ARRAY_BUFFER cube-vertex-id)
  
  ;;Set the Pointer to the current bound array describing how the data ia stored
  (glVertexPointer 3 GL_FLOAT (ctype-sizeof _byte) 0) ;; DEPRICATE, fix later
  
  ;; Enable the client state so it will use this array buffer pointer
  (glEnableClientState GL_VERTEX_ARRAY)
  
  
  ;; Element Array Buffer
  ;; Bind to the Array Buffer ID
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecies-vbo-id)
  
  ;; Draw the elements in the element array buffer
  ;; Draws up items in the Color, Vertex, TexCoordinate, and Normal Buffers using indices in the ElementArrayBuffer
  (glDrawElements GL_TRIANGLES element-count GL_UNSIGNED_INT 0))

(view draw init)