; This file is part of m3ide, a simple development environment for M3    
; Copyright (C) 1995 Michel Dagenais                                     
;                                                                        
; This library is free software; you can redistribute it and/or          
; modify it under the terms of the GNU Library General Public            
; License as published by the Free Software Foundation; either           
; version 2 of the License, or (at your option) any later version.       
;                                                                        
; This library is distributed in the hope that it will be useful,        
; but WITHOUT ANY WARRANTY; without even the implied warranty of         
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      
; Library General Public License for more details.                       
;                                                                        
; You should have received a copy of the GNU Library General Public      
; License along with this library; if not, write to the Free             
; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     
;                                                                        
; For more information on this program, contact Michel Dagenais at       
; dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique 
; P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           

;
; CHECK disable auto-save and undo on scratch buffers
;

(defvar w3-mode-hooks 'check-m3-w3-menu)
(defvar w3-reuse-buffers 'yes)

(defvar m3-change-builddir ()
  "Build into XXX-COV for coverage and XXX-PROF for profiling")

(require 'm3process)
(provide 'm3menu)

;
; If the user specifies a web browser in m3-fetch-url, use it.
; Otherwise use browse-url, or failing that w3-fetch.
;

(if (not (boundp 'm3-fetch-url-function))
  (progn
    (defvar m3-fetch-url-function 'm3-no-fetch
      "Function used to browse HTML documents e.g. m3-w3-fetch, 
       browse-url-netscape"
    )
    (if (fboundp 'browse-url-at-point) 
      (progn 
        (require 'browse-url)
        (setq m3-fetch-url-function browse-url-browser-function))
      (if (fboundp 'w3-fetch)
        (progn
          (require 'w3)
          (setq m3-fetch-url-function 'm3-w3-fetch))
      )
    )
  )
)

(defvar m3-w3-menu
  '("W3Edit"
    ["Edit corresponding file" m3-w3-edit t]
  )
)

(defvar m3-menu
  '("Modula-3"
    ("Help"
      ["PM3 introduction" m3-help-m3menu t]
      ["Language definition" m3-help-language t]
;      ["Libraries" m3-help-libraries t]
      ["Compiler and tools" m3-help-srcm3 t]
;      ["Building distributed Apps with M3" m3-help-bdam3 t]
    )
    ("Browse"
      ["List programs" m3-list-programs t]
      ["List libraries" m3-list-libraries t]
      ["List interfaces" m3-list-interfaces t]
      ["List implementations" m3-list-implementations t]
      ["List types" m3-list-types t]
      ( "List by prefix"
        ["List programs" m3-list-programs-prefix t]
        ["List libraries" m3-list-libraries-prefix t]
        ["List interfaces" m3-list-interfaces-prefix t]
        ["List .h interfaces" m3-list-h-interfaces-prefix t]
        ["List generic interfaces" m3-list-gen-interfaces-prefix t]
        ["List implementations" m3-list-implementations-prefix t]
        ["List .c implementations" m3-list-c-implementations-prefix t]
        ["List generic implementations" m3-list-gen-implementations-prefix t]
        ["List types" m3-list-types-prefix t]
      )
    )
    ("Build"
      ["Build" m3-build t]
      ["Ship" m3-ship t]
      ["Coverage analysis" m3-toggle-cov :style toggle :selected (m3-covp)]
      ["Profiling" m3-toggle-prof :style toggle :selected (m3-profp)]
    )
    ["Next error" next-error t]
    ("Run/debug"
      ["Run" m3-run-program t]
      ["Debug" m3-debug t]
      ["Show new" m3-toggle-shownew :style toggle :selected (m3-shownewp)]
      ["Show heap" m3-toggle-showheap :style toggle :selected (m3-showheapp)]
      ["Show thread" m3-toggle-showthread :style toggle 
          :selected (m3-showthreadp)]
      ["Run arguments" m3-read-run-arguments t]
    )
    ("Analyze"
      ["Coverage" m3-analyze-coverage t]
      ["Profiling" m3-analyze-profile t]
    )
    ("Forms VBT"
      ["Show Form" m3-show-form m3-is-form]
      ["Hide Form" m3-hide-form t]
    )
    ["Pretty print" m3-pprint m3-is-program]
    ("Ide parameters"
;      ["Packages path" m3-edit-packages-paths t]
      ["Restart ide" clean-m3-ide-process t]
      ["Restart m3browser" clean-m3-browser-process t]
    )
  )
)

(defvar m3-slash "/"
  "On unix the path separator is /, may be \ on some weird machines"
)

(if (file-exists-p "/usr/local/pm3/lib/m3/pkg")
  (defvar m3-www "file:/usr/local/pm3/lib/m3/pkg"
    "Path where modula-3 packages are located"
  )
  (defvar m3-www "file:/usr/lib/m3/pkg"
    "Path where modula-3 packages are located"
  )
)

(defvar m3-browser (concat "http://" (system-name) ":8000")
  "M3 browser to connect to"
)

(defvar m3-source-roots 
  (list "/usr/local/pm3/lib/m3/pkg" 
;;    (concat (getenv "HOME") "/src/m3")
  )
  "Roots for Modula-3 packages source code, served by m3browser"
)

(defvar m3-cov ()
  "Build to generate coverage information, run the corresponding executable"
)

(defun m3-covp () (if m3-cov t ()))

(defun m3-toggle-cov () (interactive)
  "Change the status of cov"
  (if m3-cov (setq m3-cov ()) (setq m3-cov t))
)

(defvar m3-prof ()
  "Build to generate profiling information, run the corresponding executable"
)

(defun m3-profp () (if m3-prof t ()))

(defun m3-toggle-prof () (interactive)
  "Change the status of prof"
  (if m3-prof (setq m3-prof ()) (setq m3-prof t))
)

(defvar m3-shownew ()
  "Show new allocations while running"
)

(defun m3-shownewp () (if m3-shownew t ()))

(defun m3-toggle-shownew () (interactive)
  "Change the status of shownew"
  (if m3-shownew (setq m3-shownew ()) (setq m3-shownew t))
)

(defvar m3-showheap ()
  "Show heap while running"
)

(defun m3-showheapp () (if m3-showheap t ()))

(defun m3-toggle-showheap () (interactive)
  "Change the status of showheap"
  (if m3-showheap (setq m3-showheap ()) (setq m3-showheap t))
)

(defvar m3-showthread ()
  "Show thread while running"
)

(defun m3-showthreadp () (if m3-showthread t ()))

(defun m3-toggle-showthread () (interactive)
  "Change the status of showthread"
  (if m3-showthread (setq m3-showthread ()) (setq m3-showthread t))
)

(defvar m3-run-arguments ""
  "Arguments when running a command"
)

(defun m3-read-run-arguments () (interactive)
  "Read and store the command line arguments"
  (setq m3-run-arguments 
    (read-string "Command line arguments: " m3-run-arguments)
  )
)

(defvar m3-w3-frame ()
  "Frame to show browsing"
)

(defun check-m3-menu ()
  "insure that the Modula-3 menu is on the current menu bar"
  (if (boundp 'current-menubar)
    (if (and current-menubar (not (assoc "Modula-3" current-menubar)))
     (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-menu nil "Modula-3" (cdr m3-menu))
      )
    )
    ;; Use easy menu in other cases (96-03-03/lenst)
    (if (fboundp 'easy-menu-define)
	(let ((map (current-local-map)))
	  (if (null map)
	      (progn (setq map (make-sparse-keymap))
		     (use-local-map map)))
	  (if (not (consp (lookup-key map [menu-bar Modula-3])))
	      (easy-menu-define m3-easy-menu map "The Modula 3 menu"
				m3-menu))))))


(defvar m3-w3-menu-set ()
  "Have we added to the W3 menu yet"
)

(defun check-m3-w3-menu ()
  "Add an edit menu to the w3 menubar"
  (if (boundp 'current-menubar)
    (if (not m3-w3-menu-set)
      (progn
        (setq w3-menu (cons m3-w3-menu w3-menu))
        (setq m3-w3-menu-set t)
      )
    )
  )
)

(defun get-buffer-create-m3 (b)
  "install the m3 menu as well"
  (let ((buffer (get-buffer-create b))
        (old-buffer (current-buffer))
       )
    (set-buffer buffer)
    (make-local-variable 'm3-is-form)
    (make-local-variable 'm3-is-program)
    (setq m3-is-form ())
    (setq m3-is-program ())
    (check-m3-menu)
    (set-buffer old-buffer)
    buffer
  )
)

(defun get-tmpbuf-create-m3 (b)
  "have no undo"
  (let ((buffer (get-buffer-create-m3 b)))
    (buffer-disable-undo buffer)
    buffer
  )
)

(defun m3-fetch-url (url)
  "Call the function stored in m3-fetch-url-function"
  (funcall m3-fetch-url-function url)
)

(defun m3-no-fetch (url)
  "Tell that no web browser is available. You should really get a version
   of emacs with w3.el or browse-url"
  (message "Sorry, no web browser is configured to show this information")
)

(defun m3-w3-fetch (url)
  "Follow the url in the m3-w3 frame."
  (interactive (list (w3-read-url-with-default)))
  (cond
    ( 
      (and (fboundp 'make-frame)
          (fboundp 'select-frame)
      )
      (progn
        (if (not (framep m3-w3-frame)) (setq m3-w3-frame (make-frame)))
        (select-frame m3-w3-frame)
        (w3-fetch url)
      )
    )
    (t (w3-fetch url))
  )
)

(defun m3-w3-edit () (interactive)
  "Do a find-file on the currently viewed html document if it is a file."
  (interactive)
  (cond
    (
      (and (or (null url-current-type) (eq url-current-type "file") t)
         (eq major-mode 'w3-mode)
      )
      (if w3-mutable-windows
        (find-file-other-window (m3-w3-current-file))
        (find-file (m3-w3-current-file))
      )
    )
    (t (message "Sorry, I can't get that file so you can alter it."))
  )
)

(defun m3-w3-current-file ()
  "Check if there is a base"
  (let ((html-source w3-current-source)
        (html-file url-current-file)
       )
    (if (string-match "<BASE HREF=\"[^/]*//[^/]*\\([^\"]*\\)\"" html-source)
      (setq html-file 
        (substring html-source (match-beginning 1) (match-end 1))
      )
    )
    html-file
  )
)
    
(defun m3-help-m3menu () (interactive)
  "Show the m3menu documentation"
  (m3-fetch-url (concat m3-www "/intro/src/index.html"))
)

(defun m3-help-language () (interactive)
  "Show the language definition documentation"
  (m3-fetch-url (concat m3-www "/modula3/src/m3defn/m3.html"))
)

(defun m3-help-libraries () (interactive)
  "Show the libraries documentation"
  (m3-fetch-url (concat m3-www "/libs/src/index.html"))
)

(defun m3-help-srcm3 () (interactive)
  "Show the SRC Modula-3 documentation"
  (m3-fetch-url (concat m3-www "/modula3/src/index.html"))
)

(defun m3-help-bdam3 () (interactive)
  "Show the Building distributed applications with Modula-3 book"
  (m3-fetch-url (concat m3-www "/modula3/src/index.html"))
)

(defun m3-build () (interactive)
  "Execute m3build and await errors"
  (m3-run (m3-add-builddir "m3build"))
)

(defun m3-add-builddir (command)
  "Add -b builddir as needed"
  (let ((new-command command)
        (builddir ())
       )
    (if (or m3-cov m3-prof)
      (if m3-change-builddir
        (progn 
          (if m3-cov
            (setq builddir (m3-ide-command-string "buildDir" (list "-COV")))
            (setq builddir (m3-ide-command-string "buildDir" (list "-PROF")))
          )
          (setq command (concat command " -b " builddir))
        )
        (if m3-cov
          (setq command (concat command " -DCOV"))
          (setq command (concat command " -DPROF"))
        )
      )
    )
    command
  )
)

(defun m3-run-program () (interactive)  
  "Run the program associated with this package"
  (let ((old-buffer (current-buffer))
        (old-window (selected-window))
        (m3-program-name ())
        (m3-program-dir ())
        (build-suffix ())
       )

    (setq build-suffix "")
    (if m3-change-builddir
      (if m3-cov
        (setq build-suffix "-COV")
        (if m3-prof
          (setq build-suffix "-PROF")
        )
      )
    )

    (setq m3-program-name 
      (m3-ide-command-string "pkgProgram" 
        (list (buffer-file-name) build-suffix)
      )
    )

    (setq m3-program-dir (m3-build-path (buffer-file-name) build-suffix))

    (if (or (= (length m3-program-name) 0) (= (length m3-program-dir) 0))
      (message (concat "File " (buffer-file-name)
        " is not in a program package")
      )
      (progn
        (shell)
        (if m3-shownew 
          (setq m3-program-name (concat m3-program-name " @M3shownew"))
        )
        (if m3-showheap
          (setq m3-program-name (concat m3-program-name " @M3showheap"))
        )
        (if m3-showthread 
          (setq m3-program-name (concat m3-program-name " @M3showthread"))
        )
        (if (> (length m3-run-arguments) 0)
          (setq m3-program-name (concat m3-program-name " " m3-run-arguments))
        )
        (goto-char (point-max))
        (insert (concat "cd " m3-program-dir))
        (comint-send-input)
        (insert m3-program-name)
      )
    )
  )
)

(defun m3-debug () (interactive)
  "Send the current buffer file name to the m3-ide which will return the
   input to send to gdb to initialize the paths, working directory and
   file to debug."
  (let ((old-gdb-name ())
        (m3-gdb-command "")
        (old-buffer (current-buffer))
        (old-window (selected-window))
        (old-file-name (buffer-file-name))
        (m3-program-name ())
        (m3-program-dir ())
        (build-suffix ())
       )

    ; The old "gdb.el" package looks in gdb-command-name for "m3gdb" while
    ; the newer "gud.el" expects "m3gdb" bundled in the argument.
    (if (boundp 'gdb-command-name)
      (progn 
        (setq old-gdb-name gdb-command-name)
        (setq gdb-command-name "m3gdb")
      )
      (setq m3-gdb-command "m3gdb ")
    )

    (setq build-suffix "")
    (if m3-change-builddir
      (if m3-cov
        (setq build-suffix "-COV")
        (if m3-prof
          (setq build-suffix "-PROF")
        )
      )
    )

    (setq m3-program-name 
      (m3-ide-command-string "pkgProgram" 
        (list (buffer-file-name) build-suffix)
      )
    )

    (setq m3-program-dir (m3-build-path (buffer-file-name) build-suffix))

    (if (or (= (length m3-program-name) 0) (= (length m3-program-dir) 0))
      (message (concat "File " (buffer-file-name)
        " is not in a program package")
      )
      (progn
        (setq m3-program-name (file-name-nondirectory m3-program-name))
        (gdb (concat m3-gdb-command m3-program-dir m3-slash m3-program-name))
        (if (boundp 'current-gdb-buffer)
          (set-buffer current-gdb-buffer)
          (set-buffer (concat "*gud-" m3-program-name "*"))
        )
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (delete-region (point) (point-max))
        (insert (m3-source-paths old-file-name build-suffix "dir " "\n"
            ":" ""))
        (comint-send-input)
      )
    )
    (if (boundp 'gdb-command-name) (setq gdb-command-name old-gdb-name))
  )
)

(defun m3-list-programs () (interactive)
  "List the programs through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/G"))
)

(defun m3-list-libraries () (interactive)
  "List the libraries through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/0"))
)

(defun m3-list-interfaces () (interactive)
  "List the interfaces through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/1"))
)

(defun m3-list-implementations () (interactive)
  "List the implementations through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/2"))
)

(defun m3-list-types () (interactive)
  "List the types through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/K"))
)

(defun m3-list-programs-prefix (prefix) 
  (interactive "sPrograms starting in: ")
  "List the programs through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/J" prefix))
)

(defun m3-list-libraries-prefix (prefix) 
  (interactive "sLibraries starting in: ")
  "List the libraries through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/9" prefix))
)

(defun m3-list-interfaces-prefix (prefix) 
  (interactive "sInterfaces starting in: ")
  "List the interfaces through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/B" prefix))
)

(defun m3-list-gen-interfaces-prefix (prefix) 
  (interactive "sInterfaces starting in: ")
  "List the interfaces through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/A" prefix))
)

(defun m3-list-h-interfaces-prefix (prefix) 
  (interactive "sInterfaces starting in: ")
  "List the interfaces through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/F" prefix))
)

(defun m3-list-implementations-prefix (prefix) 
  (interactive "sImplementations starting in: ")
  "List the implementations through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/D" prefix))
)

(defun m3-list-gen-implementations-prefix (prefix) 
  (interactive "sImplementations starting in: ")
  "List the implementations through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/C" prefix))
)

(defun m3-list-c-implementations-prefix (prefix) 
  (interactive "sImplementations starting in: ")
  "List the implementations through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/E" prefix))
)

(defun m3-list-types-prefix (prefix) (interactive "sTypes starting in: ")
  "List the types through the m3browser"
  (start-m3-browser)
  (m3-fetch-url (concat m3-browser "/M" prefix))
)

(defun m3-show-form () (interactive)
  "Send the current buffer (and associated file name) as a form to the 
   m3-ide and get back the errors if any."
  (let ((old-buffer (current-buffer))
        (old-window (selected-window))
       )
    (get-tmpbuf-create-m3 "*m3-tmp*")
    (m3-ide-command "showForm" "*m3-tmp*" 
      (list (buffer-file-name) (buffer-string))
    )
    (set-buffer "*m3-tmp*")
    (if (> (buffer-size) 1)
      (switch-to-buffer-other-window "*m3-tmp*")
    )
    (set-buffer old-buffer)    
    (select-window old-window)
  )
)

(defun m3-hide-form () (interactive)
  "Send an empty string as form to show"
  (get-tmpbuf-create-m3 "*m3-tmp*")
  (m3-ide-command "showForm" "*m3-tmp*" 
      (list "" "")
  )
)

(defun m3-pprint () (interactive)
  "Send the current buffer to the pretty printer and get the result back"
  (let ((old-buffer (current-buffer))
        (old-window (selected-window))
       )
    (m3-ide-command "pprint" (buffer-name) (list (buffer-string)))
    (set-buffer old-buffer)    
    (select-window old-window)
  )
)

;(defun m3-pprint () (interactive)
;  "Send the current buffer to the pretty printer and get back the
;   pretty printed version." 
;  (m3::pp-buffer)
;)

(defun m3-ship () (interactive)
  "Execute m3ship and await the errors"
  (m3-run (m3-add-builddir "m3ship"))
)

(defun m3-run (command)  
  "Run the specified command, usually m3build or m3ship, and await the
   errors"
  (let ((old-buffer (current-buffer))
        (old-buffer-file-name (buffer-file-name))
        (old-window (selected-window))
       )
    (get-tmpbuf-create-m3 "*m3-tmp*")
    (m3-ide-command "pkgM3makefile" "*m3-tmp*" (list old-buffer-file-name))
    (set-buffer "*m3-tmp*")
    (if (= (buffer-size) 0)
      (message (concat "File " old-buffer-file-name " is not in a package"))
      (progn
        (let ((m3makefile-name (buffer-string))
              (m3makefile-buffer (find-file-noselect (buffer-string)))
             )
          (set-buffer m3makefile-buffer)
          (if (= (buffer-size) 0)
            (message (concat "File " m3makefile-name " is empty"))
            (compile command)
          )
        )
      )
    )
    (set-buffer old-buffer)    
    (select-window old-window)
  )
)

(defun m3-analyze-coverage () (interactive)
  "Get the coverage analysis for the current file."
  (let ((old-buffer (current-buffer))
        (old-window (selected-window))
        (build-path ())
        (build-suffix "")
        (source-name (file-name-nondirectory (buffer-file-name)))
        (source-path (file-name-directory (buffer-file-name)))
       )
    (if m3-change-builddir (setq build-suffix "-COV"))
    (setq build-path (m3-build-path (buffer-file-name) build-suffix))

    (if (= (length build-path) 0)
      (message (concat "File " (buffer-file-name)
        " is not in a program package")
      )
      (progn
        (get-tmpbuf-create-m3 "*m3-coverage*")
        (set-buffer "*m3-coverage*")
        (erase-buffer)
        (setq default-directory (concat build-path m3-slash))
        (start-process "m3-coverage" "*m3-coverage*" "analyze_coverage"
          "-S" source-path "-l" source-name)
        (switch-to-buffer-other-window "*m3-coverage*")
        (set-buffer old-buffer)    
        (select-window old-window)
      )
    )
  )
)

(defun m3-analyze-profile () (interactive)
  "Get the profiling for the current package."
  (let ((old-buffer (current-buffer))
        (old-window (selected-window))
        (build-path ())
        (program-path ())
        (build-suffix "")
       )

    (if m3-change-builddir (setq build-suffix "-PROF"))
    (setq build-path (m3-build-path (buffer-file-name) build-suffix))
    (setq program-path (m3-package-program (buffer-file-name) build-suffix))

    (if (= (length build-path) 0)
      (message (concat "File " (buffer-file-name)
        " is not in a program package")
      )
      (progn
        (get-tmpbuf-create-m3 "*m3-profile*")
        (set-buffer "*m3-profile*")
        (erase-buffer)
        (setq default-directory (concat build-path m3-slash))
        (start-process "m3-profile" "*m3-profile*" "gprof" program-path)
        (switch-to-buffer-other-window "*m3-profile*")
        (set-buffer old-buffer)    
        (select-window old-window)
      )
    )
  )
)

(defun m3-ide-command-string (command args)  
  "send the specified ide command and return result in string"
  (let ((old-buffer (current-buffer))
        (result ())
       )
    (get-tmpbuf-create-m3 "*m3-tmp*")
    (m3-ide-command command "*m3-tmp*" args)
    (set-buffer "*m3-tmp*")
    (setq result (buffer-string))
    (set-buffer old-buffer)
    result
  )
)

(defun m3-package-prefix (file-name)
  "Return the directory for the package containing the file"
  (m3-ide-command-string "pkgPrefix" (list file-name))
)

(defun m3-package-m3makefile (file-name)
  "Return the m3makefile name for the package containing the file"
  (m3-ide-command-string "pkgM3makefile" (list file-name))
)

(defun m3-package-program (file-name build-suffix)
  "Return the program name for the package containing the file"
  (m3-ide-command-string "pkgProgram" (list file-name build-suffix))
)

(defun m3-build-dir (build-suffix)
  "Return the build directory"
  (m3-ide-command-string "buildDir" (list build-suffix))
)

(defun m3-build-path (file-name build-suffix)
  "Return the build directory path for the package containing the file"
  (m3-ide-command-string "buildPath" (list file-name build-suffix))
)

(defun m3-source-paths (file-name build-suffix command-prefix command-suffix
    path-prefix path-suffix)
  "Returns a large string of paths where source code may be found 
   for the program and its libraries. The path prefix and suffix are
   used to delimit the individual paths."
  (m3-ide-command-string "sourcePaths" 
    (list file-name build-suffix command-prefix command-suffix 
        path-prefix path-suffix)
  )
)

