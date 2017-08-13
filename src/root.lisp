;;; root.lisp -- superuser-when-it-suits-me privilege managemenet (for CMUCL)
;;; Written by Luke Gorrie <luke@bluetail.com>, July 2003.
;;
;; This package implements a convenient security loophole: it allows
;; you to become root temporarily, whenever you please.
;;
;; WARNING: Only use this package if you understand how it works,
;; and/or don't mind users of your machine becoming root at will! To
;; understand how it works, refer to Stevens' _Advanced Programming in
;; the Unix Environment_, or ask your local Unix guru.
;;
;; To setup:
;;
;;   Compile this package, and either add it to your image or load it
;;   in your init file.
;;
;;   Add the following line to your init file:
;;     #+root (root:condescend)
;;
;;   Make your 'lisp' process setuid-root:
;;     chown root `which lisp`
;;     chmod u+s `which lisp`
;;   (Alternatively, you could do this to a separate copy of the
;;   'lisp' program, called e.g. 'sulisp'.)
;;
;; Now your Lisp system will start as root, but quickly switch to your
;; real user. Whenever you want to run some code as root, you need
;; only write:
;;
;;   (root:as-superuser ...naughty code...)
;;
;; You can test by writing (list (root::geteuid)
;;                               (root:as-superuser (root::geteuid)))
;;
;; which should return (<your-real-uid> 0).

(defpackage :root
  (:use :common-lisp)
  (:export :condescend :as-superuser)
  )

(in-package :root)

(defun condescend ()
  "Switch down from the superuser, with the option to switch back.
\(Sets the effective user to the real user, and the real user to root.)
Returns T on success, or NIL if we weren't the superuser."
  (values (osicat-posix:setreuid 0 (osicat-posix:getuid))))

(defmacro as-superuser (&body forms)
  "Execute FORMS as the superuser."
  `(let ((old-euid (osicat-posix:geteuid)))
    (unwind-protect
         (if (osicat-posix:setreuid 0 0)
             (progn ,@forms)
             (error "Failed to become superuser"))
      (osicat-posix:setreuid 0 old-euid))))


(pushnew :root *features*)

