
(in-package :cl-cooperative)

(defmacro p (str &rest args)
  #+cooperative-debug `(v:debug :cl-cooperative ,str ,@args)
  #-cooperative-debug nil)

