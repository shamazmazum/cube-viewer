(in-package :cube-viewer)

(declaim (type (simple-array single-float (#.(* 3 3 12))) +cube-vertices+))
(alex:define-constant +cube-vertices+
    (map '(vector single-float)
         #'float
         '( 1 -1  1
           -1 -1 -1
            1 -1 -1

           -1  1 -1
            1  1  1
            1  1 -1

            1  1 -1
            1 -1  1
            1 -1 -1

            1  1  1
           -1 -1  1
            1 -1  1

           -1 -1  1
           -1  1 -1
           -1 -1 -1

            1 -1 -1
           -1  1 -1
            1  1 -1

            1 -1  1
           -1 -1  1
           -1 -1 -1

           -1  1 -1
           -1  1  1
            1  1  1

            1  1 -1
            1  1  1
            1 -1  1

            1  1  1
           -1  1  1
           -1 -1  1

           -1 -1  1
           -1  1  1
           -1  1 -1

            1 -1 -1
           -1 -1 -1
           -1  1 -1))

  :test #'equalp)

(declaim (type (simple-array single-float (#.(* 3 3 12))) +cube-normals+))
(alex:define-constant +cube-normals+
    (map '(vector single-float)
         #'float
         '( 0 -1  0
            0 -1  0
            0 -1  0

            0  1  0
            0  1  0
            0  1  0

            1  0  0
            1  0  0
            1  0  0

            0  0  1
            0  0  1
            0  0  1

           -1  0  0
           -1  0  0
           -1  0  0

            0  0 -1
            0  0 -1
            0  0 -1

            0 -1  0
            0 -1  0
            0 -1  0

            0  1  0
            0  1  0
            0  1  0

            1  0  0
            1  0  0
            1  0  0

            0  0  1
            0  0  1
            0  0  1

           -1  0  0
           -1  0  0
           -1  0  0

            0  0 -1
            0  0 -1
            0  0 -1))
  :test #'equalp)
