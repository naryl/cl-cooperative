
(in-package :cl-cooperative)

(defmacro p (str)
  #+cooperative-debug `(v:debug :cl-cooperative "~A~%" ,str)
  #-cooperative-debug nil)

