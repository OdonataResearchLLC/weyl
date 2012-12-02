;;; -*- Mode:Lisp; Package:CL-User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			    System Maintenance
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; maintenance.lisp,v 1.2 1994/08/04 14:29:22 rz Exp

(in-package #-ANSI-CL "USER" #+ANSI-CL "CL-USER")

(make::adjust-version-numbers Weyl "1.2")

#+Lucid
(defun dump-weyl (&optional (name "weyl"))
  (load-system 'weyl)
  (multiple-value-bind (seconds minutes hour date month year d-o-w d-s-t t-z)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds minutes hour d-o-w d-s-t t-z))
    (let ((file (make-pathname
		 :name (cond ((member :mips *features*) "weyl-mips")
			     ((member :sparc *features*) "weyl-sparc")
			     (t "Weyl-Unknown"))
		 :directory (pathname-directory *weyl-directory*)))
	  (archive (format nil
			   (cond ((member :mips *features*)
				  "~A/~A-mips-~D-~D-~D-~D")
				 ((member :sparc *features*)
				  "~A/~A-sparc-~D-~D-~D-~D")
				 (t "~A/~A-unknown-~D-~D-~D-~D"))
			   *weyl-archive-directory*
			   name month date year
			   (+ minutes (* 100 hour))))
	  (banner (weyl-banner)))
      (declare (special system::*enter-top-level-hook*))
      (when (probe-file file)
	(delete-file file))
      ;; Comment the following line to store binaries in the source directory.
      (user::shell (format nil "ln -s ~A ~A" archive file))
      (setq system::*enter-top-level-hook* 
	    #'(lambda ()
		(format t ";;; ~A~2%"  banner)
		(lucid::default-enter-top-level-hook)))
      (disksave file :full-gc t)
      (format t ";;; Weyl ~D.~D successfully dumped into ~A~%~
                 ;;; and link was created to it from ~A"
	      make::*weyl-major-version* make::*weyl-minor-version*
	      archive file))))

#+Lucid
(defun weyl-banner ()
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (format nil "Weyl Version ~D.~D, saved ~2D:~2D ~A, ~A ~D, ~D"
	    make::*weyl-major-version* make::*weyl-minor-version*
	    hour minute
	    (second (assoc day-of-week
			   '((0 "Monday") (1 "Tuesday") (2 "Wednesday")
			     (3 "Thursday") (4 "Friday") (5 "Saturday")
			     (6 "Sunday"))))
	    (second (assoc month
			   '((1 "January") (2 "February") (3 "March")
			     (4 "April") (5 "May") (6 "June") (7 "July")
			     (8 "August") (9 "September") (10 "October")
			     (11 "November") (12 "December"))))
	    date
	    year)))

#+MCL
(defun dump-weyl (&optional (name "Weyl"))
  (load-system 'weyl)
  (let ((file (make-pathname
               :name (format nil "~A ~Db~D" 
                             name
			     make::*weyl-major-version*
			     make::*weyl-minor-version*)
               :directory (pathname-directory (user-homedir-pathname)))))
    (when (probe-file file)
      (delete-file file))
    (format t ";;; Weyl ~D.~D successfully being dumped into ~A.~%"
	      make::*weyl-major-version* make::*weyl-minor-version*
	      file)
    (save-application  file :init-file "init")))


