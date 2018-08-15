(in-package #:cl-jupyter-widgets)

(defmacro log-error (fmt &rest args)
  `(format t ,fmt ,@args))

(defvar *kernel-comm-managers* (make-hash-table)
  "Map kernels to comm-managers")


(defclass comm-manager ()
  ((kernel :initarg :kernel :reader kernel)
   (comms :initform (make-hash-table :test #'equal) :reader comms)
   (targets :initform (make-hash-table :test #'equal) :reader targets)))

(defun make-comm-manager (kernel)
  (cl-jupyter:logg 2 "In make-comm-manager~%")
  (make-instance 'comm-manager :kernel kernel))

(defmethod register-target ((self comm-manager) target-name func)
  "Register a function func for a given target name.
   func takes two arguments (comm-instance comm-message)"
  (setf (gethash target-name (targets self)) func))

(defmethod unregister-target ((self comm-manager) target-name)
  (remhash target-name (targets self)))

(defmethod register-comm ((self comm-manager) comm)
  (cl-jupyter:logg 2 "register-comm   comm-id -> ~a~%" (comm-id comm))
  (unless (comm-id comm)
    (cl-jupyter:logg 2 "The comm-id for a comm in register-comm is NIL!!!!~%backtrace -> ~%")
    (cl-jupyter:logg 2 "~a~%" (with-output-to-string (sout) (trivial-backtrace:print-backtrace-to-stream sout))))
  (let ((comm-id (comm-id comm)))
    (setf (kernel comm) (kernel self))
    (setf (gethash comm-id (comms self)) comm)
    (cl-jupyter:logg 2 "   (kernel self) -> ~a~%" (kernel self))
    (cl-jupyter:logg 2 "   (comms self) -> ~a~%" (comms self))
    (cl-jupyter:logg 2 "   comm  -> ~a~%" comm)
    (cl-jupyter:logg 2 "   lookup (gethash comm-id (comms self)) -> ~a~%" (gethash comm-id (comms self)))
    comm-id))

(defmethod unregister-comm ((self comm-manager) comm)
  (cl-jupyter:logg 2 "unregister-comm comm-id ~a~%" (comm-id comm))
  (remhash (comm-id comm) (comms self)))

(defmethod get-comm ((self comm-manager) comm-id)
  (let ((comm (gethash comm-id (comms self) nil)))
    (or comm
	(progn
	  (warn "No such comm: ~a" comm-id)
	  nil))))

(defmethod comm-open ((self comm-manager) stream identities msg)
  "Handle comm_open messages"
  (cl-jupyter:logg 2 "In manager.lisp comm-open - msg -> ~s~%" msg)
  (let ((content (extract-message-content msg)))
    (cl-jupyter:logg 2 "In manager.lisp content -> ~s~%" content)
    (let* ((comm-id ([] content "comm_id"))
	   (target-name ([] content "target_name"))
	   (func (gethash target-name (targets self)))
	   (comm (progn
		   (cl-jupyter:logg 2 "[comm-open]   About to make-instance 'comm :comm-id ~a~%" comm-id)
		   (make-instance 'comm
				  :comm-id comm-id
				  :primary nil
				  :target-name target-name))))
      (register-comm self comm)
      (if func
	  (with-error-handling "In comm-open"
	      (funcall func comm msg)
	      (return-from comm-open))
	  (log-error "No such comm target registered: ~a" target-name))
      (handler-case
	  (close comm)
	(error (err)
	  (log-error "Could not close comm during comm-open failure cleanup. The comm may not have been opened yet err: ~a" err))))))

(defvar *send-updates* t)

(defmethod comm-msg ((self comm-manager) stream identities msg)
  (cl-jupyter:logg 2 "[comm-msg] msg -> ~a~%" msg)
  (let* ((content (extract-message-content msg))
	 (comm-id ([] content "comm_id"))
	 (comm (get-comm self comm-id)))
    (cl-jupyter:logg 2 "Message for comm_id ~a  -> comm: ~a~%" comm-id comm)
    (cl-jupyter:logg 2 "     *parent-msg* -> ~s~%" cl-jupyter:*parent-msg*)
    (unless comm (return-from comm-msg nil))
    (with-error-handling (format nil "In comm-msg with comm-id ~a" comm-id)
      (handle-msg comm msg))))


(defmethod comm-close ((self comm-manager) stream identities msg)
  (let* ((content (extract-message-content msg))
	 (comm-id ([] content "comm_id"))
	 (comm (get-comm self comm-id)))
    (unless comm (return-from comm-close nil))
    (remhash comm-id (comms self))
    (with-error-handling (format nil "In comm-close with comm-id ~a" comm-id)
      (handle-close comm msg))))    
