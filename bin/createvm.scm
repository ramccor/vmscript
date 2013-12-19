(use-modules 
 (scsh scsh)
 (scsh syntax)
 (scsh run-extras)
 (srfi srfi-8)
 (rnrs lists)
 (ice-9 match)
 (ice-9 pretty-print)
 (srfi srfi-11))


(define guile-magic "#!/usr/bin/guile
!#
")
(define (modulesuse)
  "The trunk used to load necessary module in startvm.scm"
  '(use-modules  (scsh scsh)
		 (scsh syntax)
		 (scsh run-extras)
		 (srfi srfi-8)
		 (srfi srfi-11)
		 (ice-9 match)
		 (rnrs lists)
		 (ice-9 pretty-print))
  )
(define (xkvmf)
  "The trunk used to define procedure of kvm in generated startvm.scm"
  '(define (kvm xvmdef)
     (fold-right
      (lambda (x y)
	(let ((rdopt (lambda (v)
		       (string-join 
			(map (lambda (x)
			       (match x
				 ((i)
				  (format #f "~a" i))
				 ((i j)
				  (format #f "~a=~a" i j))))
			     v)
			","))))
	  (match x
	    ((p) 
	     (cons (format #f "-~a" p) y))
	    ((p v)
	     (cons (format #f "-~a" p)
		   (match v
		     ((? list? xx)
		      (cons (rdopt xx) y))
		     (_
		      (cons (format #f "~a" v) y))))))))
      '()
      xvmdef
      )))

(define (xvmdef hda-name macaddr memsize vmisopath)
  "the trunk used to define vm parameters in generated startvm.scm"
  `(define vmdef
     '((M pc)
;       (S)
       (cpu kvm64)
       (daemonize)
       (pidfile "kvm.pid")
       (drive ((file ,hda-name)
	       (index 0)
	       (if ide)
	       (media disk)))
       (net ((tap)
	     (ifname tap0)
	     (script /bin/true)))
       (net ((nic)
	     (macaddr ,macaddr)))
       (m ,memsize)
       (vnc "unix:vncsock,server")
       (chardev (
		 (socket)
		 (id mnt1)
		 (nowait)
		 (path mntrsock)
		 (server)))
       (mon ((chardev mnt1)
	     (mode readline)))
       (usb)
       (usbdevice tablet)
       (drive ((file ,vmisopath)
	       (index 1)
	       (if ide)
	       (media cdrom)))
       ))
  )


(define (optbuilder* ol)
  "usage: (optbuilder* '((info) (text \"hello world\")))"
  (let ((o->s 
	 (lambda (x)
	   (let* ((s (stringify (car x)))
		  (l? (> (string-length s) 1))
		  (p (if l?
			 "--"
			 "-"))
		  (m (if l?
			 "="
			 ""))
		  (ls (if (null? (cdr x))
			  (list p s)
			  (list p s m (stringify (cadr x))))))
	     (string-join ls "")))))
    (map o->s ol)))


(define (zenity zdef)
  `(zenity ,@(optbuilder* zdef)))

(setenv "WINDOWID" "0")

(run (,@
      (zenity 
       `((info) 
	 (text 
	  "Welcome Using Chaos Eternal's KVM creation utilities")
	 (title Welcome)))))

(random 255 (random-state-from-platform ))

(let ((repo (receive 
		(status p1 p2)
		(run/collecting 
		 (1 2) 
		 (,@(zenity 
		     '((file-selection)
		       (title "Choose Repository")
		       (directory)
		       (text 
			"Choose a directory to hold the repository")))))
	      (if (> status 0)
		  (throw 'user-cancel)
		  (car (port->string-list p1)))))
      (macaddr (string-join 
		(cons "00" 
		      (map 
		       (lambda (x) 
			 (format #f "~X" (random 255 ))) 
		       (iota 5))) 
		":")))
  (&& (test "!" -d ,repo)
      (begin (run (mkdir "-p" repo))
	     (run (mkdir ,(string-join (list repo "vde1") "/")))))
  (with-cwd 
   repo
   (let-values 
       (
	((vmname memsize disksize disktype)
	 [receive (status p1)
	     (run/collecting 
	      (1 2)
	      (,@(zenity 
		  '((forms)
		    (title "Create Virtual Machine")
		    (text "Input the following Definitions")
		    (add-entry "VM Name")
		    (add-entry "Memory Size")
		    (add-entry "Disk Size")
		    (add-list "Disk Type")
		    (list-values "qcow2|raw")
		    (column-values "QCow2")
		    (separator "\n")))))
	   (if (> status 0)
	       (throw 'user-cancel)
	       (apply values (port->string-list p1)))])
	((vmisopath)
	 [receive (status p1)
	     (run/collecting
	      (1 2)
	      (,@(zenity
		  '((file-selection)
		    (title "Choose a boot iso image, \
Cancel to create vm with out a boot iso")
		    ))))
	   (if (> status 0)
	       "/dev/null"
	       (values (car (port->string-list p1))))])
	)
     (let* ((vmpath (string-append repo "/" vmname))
	    (script-path (string-append vmpath "/startvm.scm"))
	    (hda-name "hda.img")
	    (hda-path (string-append vmpath "/" hda-name))
	    ;(script-port (open-file script-path "w"))
	    )
       (if (> (run (mkdir ,vmpath)) 0)
	   (throw `repo-exists))
       (run (qemu-img create -f ,disktype ,hda-path ,disksize))
       (with-output-to-file 
	   script-path
	 (lambda () 
	   (display guile-magic)
	   (pretty-print (modulesuse))
	   (pretty-print (xkvmf))
	   (pretty-print (xvmdef hda-name macaddr memsize vmisopath))
	   (pretty-print '(setenv "LD_PRELOAD"
				  "/usr/lib/vde2/libvdetap.so"))
	   (pretty-print '(setenv "tap0"
				  "../vde1"))
	   (pretty-print '(with-cwd 
			   (dirname 
			    (car (command-line)))
			   (let ()
			     (run (kvm ,@(kvm vmdef)))
			     (run (ssvncviewer ./vncsock))
			     )
			   )
			 )))
     ;(list repo vmname memsize disksize disktype vmisopath)
       ))))
