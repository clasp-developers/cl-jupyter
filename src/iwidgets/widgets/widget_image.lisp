(in-package :cl-jupyter-widgets)

#||
using this, the value slot changes to a b64 string, but the b64value slot is still empty

(defparameter i (make-instance 'cl-jupyter-widgets::image :value 
        (cl-base64:usb8-array-to-base64-string 
         (cl-jupyter-widgets::read-file-into-byte-vector 
          "/home/app/work/home/Development/widget-dev/images/icon.png"))))

https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_image.py#L42
||#


(defun read-file-into-byte-vector (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_image.py#L19
(defclass image (domwidget value-widget core-widget)
  ((image-format :initarg :format :accessor image-format
	    :type unicode
	    :initform (unicode "png")
	    :metadata (:sync t
			     :json-name "format"))
   (width :initarg :width :accessor width
	   :type cunicode 
	   :initform (unicode "")
	   :metadata (:sync t
			    :json-name "width"))
         
   (height :initarg :height :accessor height
	    :type cunicode 
	    :initform (unicode "")
	    :metadata (:sync t
			     :json-name "height"))

   (_b64value :initarg :b64value :accessor b64value
	      :type unicode
	      :initform (unicode "")
	      :metadata (:sync t
			       :json-name "_b64value"))
   (value :initarg :value :accessor value 
	   :type bytes 
	   :initform #()
	   :metadata (:sync t
			    :json-name "value")))

   (:default-initargs  
       :view-name (unicode "ImageView")
     :model-name (unicode "ImageModel")
     :model-module (unicode "@jupyter-widgets/controls")
     :view-module (unicode "@jupyter-widgets/controls"))
   (:metaclass traitlets:traitlet-class))


(defmethod (setf closer-mop:slot-value-using-class) :after
    (new-value (class traitlets:traitlet-class) (object image) (slotd traitlets:effective-traitlet))
  (cl-jupyter:logg 2 "Updating the ~s slot of ~s~%" slotd object)
  (when (eq (closer-mop:slot-definition-name slotd) 'value)
    (let ((b64image (cl-base64:usb8-array-to-base64-string new-value)))
      (cl-jupyter:logg 2 "    Converting the image to: ~s~%" b64image)
      (setf (b64value object) b64image))))

