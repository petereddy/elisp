
(defvar mirrorcfg #s(hash-table))

(defun mirror-directories (directories)
	(mapcar (lambda (dir)
						(puthash dir (remove dir directories) mirrorcfg))
					directories))

;; (defun directory-of (filename)
;; 	(cond ((file-directory-p filename) filename)
;; 				((file-regular-p filename))))

(defun mirror-addq (object list)
	(if (memq object list) list
		(cons object list)))

(defun mirror-update-mirrored ()
	'())

(add-hook 'write-file-hooks #'mirror-update-mirrored)

;; (defun mirror-targets (file-or-dir-name)
;; 	(cond ((bufferp x) (mirror-mirrored-p (buffer-file-name x)))
;; 				((stringp x)
;; 				 (let (targets '())
;; 					 (maphash (lambda (dir dirs)
;; 											(cond ((file-in-directory-p x dir)
;; 														 (setq (mirror-addq x targ)

(defun mirror-targets (file-or-dir-name)
	(cond ((bufferp file-or-dir-name) (mirror-targets (buffer-file-name x)))
				((stringp file-or-dir-name)
				 (let (targets '())
					 (maphash (lambda (dir dirs)
											(cond ((file-in-directory-p file-or-dir-name dir)
														 (setq (mirror-addq dir targets)))))
										mirrorcfg))
				 targets)
				(t nil)))
														 
(mirror-targets "/a/b/c")

(file-truename (buffer-file-name))

(mirror-directories '("/a/b/c" "/d/e/f"))

(remove-hook 'write-file-hooks #'message)

mirrorcfg



(defun mirror-add-dirs (dir-list)
	

(buffer-file-name)

(provide 'pje-mirror)
