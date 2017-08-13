
(defpackage :tuntap
  (:use :common-lisp :cffi
                                        ;"UNIX" "ALIEN" "C-CALL"
        )
  (:export
   :create-tunnel :close-tunnel :tunnel-read :tunnel-write))

(in-package :tuntap)

;;; -----------------------------------------------------------
;;; System call interface type definitions (using CMUCL "Alien" FFI)

;; From <linux/if.h>

(defconstant +IFNAMSIZ+ 16)

;; Faked 16-byte type, just to pad the union in ifreq to the right size.
;(def-alien-type ifru-pad
;  (struct ifru-pad
;  (pad (array char #.+IFNAMSIZ+))))

(defcstruct ifru-pad
  (pad :char :count #.+IFNAMSIZ+))

;; From <linux/if.h>
#|(def-alien-type ifreq
  (struct ifreq
	  (name (array char 16))
	  (ifru (union ifr-ifru
		       (flags short)
		       (dummy ifru-pad)))))
|#
(defcunion ifr-ifru
  (flags :short)
  (dummy (:struct ifru-pad)))

(defcstruct ifreq
  (name :char :count 16)
	(ifru (:union ifr-ifru)))

;; From <linux/if_tun.h>

(defconstant +IFF-TUN+       #x0001)
(defconstant +IFF-TAP+       #x0002)
(defconstant +IFF-NO-PI+     #x1000)
(defconstant +IFF-ONE-QUEUE+ #x2000)

(defconstant +TUNSETIFF+     #x400454ca)

;;; -----------------------------------------------------------
;;; Lisp interface

(defun create-tunnel (type &optional name)
  "Open a tunnel and return the file descriptor."
  (let ((fd (osicat-posix:open "/dev/net/tun" osicat-posix:o-rdwr 0)))
     (if fd
         (init-tunnel fd type name)
         (error "Failed to open tunnel: ~s" (osicat-posix:strerror)))))
#|
(defun init-tunnel (fd type name)
  (let ((type-code (ecase type
		     (:tun +IFF-TUN+)
		     (:tap +IFF-TAP+))))
    (with-alien ((req (struct ifreq)))
      (if (null name)
          (setf (deref (slot req 'name) 0) 0)
          ;; yucksome.. copy name into ifreq's cstring
          (do ((idx 0 (1+ idx)))
              ((or (= idx (length name))
                   (= idx +IFNAMSIZ+))
               (setf (deref (slot req 'name) idx) 0))
            (setf (deref (slot req 'name) idx)
                  (char-code (aref name idx)))))
      (setf (slot (slot req 'ifru) 'flags)
	    (logior type-code +IFF-NO-PI+ +IFF-ONE-QUEUE+))
      (if (unix-ioctl fd +TUNSETIFF+ (alien-sap req))
	  fd
	  (error "TUNSETIFF ioctl failed")))))

(cffi:foreign-slot-value info ’(:struct system-info)
’number-of-processors)))
(let* ((name "mike")(name-len (+ (length name) 1)))
  (with-foreign-object (req '(:struct ifreq))
    (lisp-string-to-foreign name (cffi:foreign-slot-value req '(:struct ifreq) 'name) name-len)
    (print (cffi:foreign-string-to-lisp (cffi:foreign-slot-value req '(:struct ifreq) 'name)))
    )
  )
 
|#

(defun init-tunnel (fd type name)
  (let ((type-code (ecase type
                     (:tun +IFF-TUN+)
                     (:tap +IFF-TAP+)))
        (name-len (length name)))
    (when (> name-len 16) (error (format nil "Interface name:~a must not exceed 16 characters" name)))
    (with-foreign-object (req '(:struct ifreq))
      (if (null name)
          (setf (mem-ref (cffi:foreign-slot-value req '(:struct ifreq) 'name) :char 0) 0)
          (lisp-string-to-foreign name (cffi:foreign-slot-value req '(:struct ifreq) 'name) name-len))
      
      (setf (cffi:foreign-slot-value (cffi:foreign-slot-value req '(:struct ifreq) 'ifru)
                                     '(:union ifr-ifru) 'flags)
            (logior type-code +IFF-NO-PI+ +IFF-ONE-QUEUE+))
      (if (osicat-posix:ioctl fd +TUNSETIFF+ req)
          fd
          (error "TUNSETIFF ioctl failed"))
      )))


(defun vector-to-foreign-array (array)
  (let ((fa (cffi:foreign-alloc :unsigned-char :count (length array) :initial-element 0)))
    (dotimes (i (length array))
      (setf (mem-aref fa :unsigned-char i) (aref array i))
      )
    fa
    )
  )

(defun foreign-array-to-vector (fa len &optional array)
  (let ((buf (if array array (make-array (list len) :element-type '(unsigned-byte 8) :initial-element 0))))
    (dotimes (i len)
      (setf (aref buf i) (mem-aref fa :unsigned-char i) )
      )
    buf
    )
  )

(defun close-tunnel (fd)
  "Close the tunnel file descriptor, destroying the interface."
  (osicat-posix:close fd))

(defun tunnel-read (fd &optional
                         (buffer (make-array 2048
                                             :element-type '(unsigned-byte 8) :adjustable t)))
  "Read a frame from a tunnel file descriptor.
The frame is returned as a vector of bytes."
  #+cmucl
  (system:wait-until-fd-usable fd :input)
  (gc:with-gc-off
    (when (not (adjustable-array-p buffer)) (error "buffer must be adjustable array"))
    (let ((length (osicat-posix:read fd (vector-to-foreign-array buffer) (length buffer))))
      (and length (adjust-array buffer (list length))))))

(defun tunnel-write (fd frame)
  "Write a frame to the tunnel.
The frame can either be a string or a vector of 8-bit integers."
  (declare (type simple-array frame))
  (osicat-posix:write fd frame (length frame)))

#|
(defun tunnel-read (fd &optional
                    (buffer (make-array 2048
                                        :element-type '(unsigned-byte 8))))
  "Read a frame from a tunnel file descriptor.
The frame is returned as a vector of bytes."
  (system:wait-until-fd-usable fd :input)
  (system:without-gcing
   (let ((length (unix-read fd (system:vector-sap buffer) (length buffer))))
     (and length (adjust-array buffer (list length))))))

(defun tunnel-write (fd frame)
  "Write a frame to the tunnel.
The frame can either be a string or a vector of 8-bit integers."
  (declare (type simple-array frame))
  (unix-write-frame fd frame))

(defun unix-write-frame (fd vec)
  (unix-write fd vec 0 (length vec)))
|#
