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
; An asynchronous process is started to handle the ide commands.
; For each command, the command name and the textual arguments are sent.
; The result is received in a buffer.
;

(provide 'm3process)

(defun m3-ide-command (name output-buffer arglist)
  "This function receives the command name, the name of the buffer
  receiving the output and a list of textual arguments"

  (let ((tmplist arglist)
       )

    (wait-m3-ide-process-busy)
    (prepare-m3-buffers (get-buffer-create output-buffer)
      (get-tmpbuf-create-m3 "*m3-error*")
    )

    ; start the m3ide process if not already started and lock it.
    (start-m3-ide)
    (set-process-filter m3-ide-process 'receive-m3-ide-result)
    ;(sit-for 0.001)
    (process-send-string m3-ide-process (concat name "\n"))
    (while tmplist
      (process-send-string m3-ide-process 
        (concat (int-to-string (length (car tmplist))) "\n")
      )
      (process-send-string m3-ide-process (car tmplist))
      (setq tmplist (cdr tmplist))
    )

    ; a few newline insure that everything is sent. There does not
    ; seem to be a flush available for stuff sent to processes.
    (process-send-string m3-ide-process "\n\n\n\n\n")
    (wait-m3-ide-reply)
    (set-process-filter m3-ide-process ())
  )
)

(defun wait-m3-ide-reply ()
  "Wait until the filter sets m3-ide-busy to nil indicating that all the
   expected output was received."
  (while m3-ide-process-busy 
    (progn 
      (message "Receiving Modula-3 Ide result...")
      (sit-for 0.2)
    )
  )
)


(defun clean-m3-ide-process () (interactive)
  "Kill the current m3-ide process and clear the associated variables."
  (if m3-ide-process 
      (progn (set-process-filter m3-ide-process ())
	     (delete-process m3-ide-process))
  )
  (setq m3-ide-process ())
  (setq m3-ide-process-busy ())
)

;
; A number of global variables are used internally
;

(defvar m3-ide-process ()
  "Process started"
)

(defvar m3-ide-process-busy ()
  "A command is currently being processed by the Ide process"
)

(defvar m3-ide-buffer ()
  "Buffer where the result of the current Ide process command should go"
)

(defvar m3-ide-error-buffer ()
  "Buffer where errors for the current Ide process command should go"
)

(defvar m3-ide-error-received ()
  "The error portion of the answer from the Ide process was received"
)

(defvar m3-ide-length-received ()
  "Each answer comes with length newline answer (both error and real answer).
   This variable indicates if the answer was received"
)

(defvar m3-ide-length ()
  "Length of the answer being received"
)

(defvar m3-ide-received ()
  "How much of the expected answer has been received"
)

(defvar m3-ide-string-received ()
  "String where the beginning of the answer goes before the answer
   length is extracted"
)

;
; These functions are used internally.
;

(defun wait-m3-ide-process-busy ()
  (while m3-ide-process-busy 
    (progn 
      (message "Modula-3 Ide busy, waiting...")
      (sit-for 0.1)
    )
  )
)

(defun start-m3-ide ()
  "Check if the Modula-3 Ide process is running and start it if not.
   Wait for the current command to complete before returning and
   set the status as busy to prevent another invocation before the
   upcoming command completes. If the previous command did not reset
   m3-ide-process-busy properly, this function may wait indefinitely.
   It can be interrupted with ^G and the m3-ide-process-busy variable
   should be set to nil."

  ;; There is an m3-ide-process and a buffer attached to it and
  ;; process is running [added lenst 96-03-03]. Thus the process
  ;; should be alive.

  (if m3-ide-process 
    (if (and (eq (process-status m3-ide-process) 'run)
	     (buffer-name (process-buffer m3-ide-process)))
	
      ; wait until the previous command is completed
      (wait-m3-ide-process-busy)

      ; the process must be dead since it has no buffer attached
      ; set it to nil.
      (progn
        (set-process-filter m3-ide-process ())
        (delete-process m3-ide-process)
        (setq m3-ide-process ())
      )
    )
  )

  ; Lock the process to insure that only one command at a time is sent.
  (setq m3-ide-process-busy t)

  ; A new process must be started. By default uncaught output will go
  ; to *m3-ide* buffer.
  (if (not m3-ide-process)    
    (let ((old-buffer (current-buffer))
          (binary-process-input t)
          (binary-process-output t))
      (unwind-protect 
        (progn
          (set-buffer (get-buffer-create "*m3-ide*"))
          (erase-buffer)
          (setq m3-ide-process
            (start-process "m3-ide" (get-tmpbuf-create-m3 "*m3-ide*")
              "m3ide"
            )
          )
          (process-kill-without-query m3-ide-process)
          (while (< (buffer-size) 66)
            ;;(sit-for 0.1)
            (accept-process-output m3-ide-process)
            )
        )
        (set-buffer old-buffer)
      )
    )
  )
)

;
; The filter must receive its arguments and store its intermediate
; results into global variables since it can only receive pre-defined
; formal arguments from the process sending output back to emacs.
; The buffers to receive output and error, and variables to indicate
; what has been received so far are used.
;

(defun prepare-m3-buffers (text-buffer error-buffer)
  "Clear the text-buffer and store the text and error buffer to global
   variables for use by the filter"

  ; store the old buffer
  (let ((old-buffer (current-buffer)))

    ; erase the buffers and store them in the global variables.
    (setq m3-ide-buffer text-buffer)
    (set-buffer m3-ide-buffer)
    (erase-buffer)
    (setq m3-ide-error-buffer error-buffer)
    (set-buffer m3-ide-error-buffer)
    (erase-buffer)

    ; initialize other global variables.
    (setq m3-ide-error-received ())
    (setq m3-ide-length-received ())
    (setq m3-ide-string-received "")
    (set-buffer old-buffer)
  )
)

;
; The filter is called repeatedly receiving a chunk of the output each time.
;

(defun receive-m3-ide-result (proc string)
  "Accept the result from the Modula-3 Ide process"

  ; we are not within a command! no output should be received.
  (if (not m3-ide-process-busy)
    (if (> (length string) 0)
      (progn
        (message (concat "Received unsolicited output from ide " string))
      )
    )

    ; Everything received and unprocessed is kept there
    (setq m3-ide-string-received (concat m3-ide-string-received string))

    ; first we must receive the length argument.
    (if (not m3-ide-length-received)
      (progn
        ; the characters received are accumulated until the length is received.
        ; The length occupies 10 characters.
        (if (> (length m3-ide-string-received) 10)
          (progn

            ; the length is received and extracted. The remaining of the
            ; characters received are part of the value to receive.
            (setq m3-ide-length-received t)
            (let ((length-string (substring m3-ide-string-received 0 10))
                  (rest-string (substring m3-ide-string-received 11))
                 )
              (setq m3-ide-string-received rest-string)
              (setq m3-ide-length (string-to-int length-string))
              (setq m3-ide-received 0)

              ; process the remaining part of the characters received.
              (receive-m3-ide-string)
            )
          )
        )
      )
      ; The length was already received. This is part of the value to receive.
      (receive-m3-ide-string)
    )

    ; We are leaving while there are characters left!
    ; It must be the length for the next value
    (if (> (length m3-ide-string-received) 10) ; changed to 10 (/lenst)
      (receive-m3-ide-result () "")
    )
  )
)

(defun receive-m3-ide-string ()
  "Receive a string into the appropriate buffer"

  ; there is more than needed. The part required to complete the
  ; expected length is put into string. The rest is stored into
  ; the m3-ide-string-received variable.
  (let ((string ())
        (old-buffer (current-buffer))
        (new-buffer 
          (if m3-ide-error-received m3-ide-buffer m3-ide-error-buffer)
        )
       )
    (if (> (+ m3-ide-received (length m3-ide-string-received)) m3-ide-length)
      (progn
        (setq string
          (substring m3-ide-string-received 0 
            (- m3-ide-length m3-ide-received)
          )
        )
        (setq m3-ide-string-received 
          (substring m3-ide-string-received (- m3-ide-length m3-ide-received))
        )
      )
      (progn
        (setq string m3-ide-string-received)
        (setq m3-ide-string-received "")
      )
    )

    (unwind-protect			; Don't change buffers in a filter
	(progn
	  (set-buffer new-buffer)
	  (goto-char (point-max))
	  (insert string))
      (set-buffer old-buffer))


    ; accumulate the characters received.
    (setq m3-ide-received (+ m3-ide-received (length string)))

    ; everything was received for this value.
    (if (>= m3-ide-received m3-ide-length)
      (if m3-ide-error-received
        ; The error and ordinary values were received.
        (progn
          (set-process-filter m3-ide-process ())
          (setq m3-ide-process-busy ())
        )
        ; The error value was received and the ordinary value is next.
        (progn
          (setq m3-ide-error-received t)
          (setq m3-ide-length-received ())
        )
      )
    )
  )
)

(defvar m3-browser-process ())

(defun start-m3-browser ()
  "Check if the Modula-3 browser process is running and start it if not."
  (if m3-browser-process
    (if (not (and (eq (process-status m3-browser-process) 'run)
             (buffer-name (process-buffer m3-browser-process))))
      (progn
        (delete-process m3-browser-process)
        (setq m3-browser-process ())
      )
    )
  )
    
  (if (not m3-browser-process)
    (let ((old-buffer (current-buffer)))
      (setq m3-browser-process
        (eval
          (append 
            (list 'start-process "m3-browser" 
              (get-tmpbuf-create-m3 "*m3-browser*")
              "m3browser"
              "-port" "8000" "-mask" "32" "-notitle"
            )
            (let ((root-list ()))
              (mapcar 
                (lambda(x)
                  (setq root-list (append root-list (list "-root" x)))
                )
                m3-source-roots
              )
              root-list
            )
          )
        )
      )
      (process-kill-without-query m3-browser-process)
      (set-buffer "*m3-browser*")
      (while (< (buffer-size) 4)
        (sit-for 0.1)
      )
      (set-buffer old-buffer)
    )
  )
)

(defun clean-m3-browser-process () (interactive)
  "Kill the current m3-browser process and clear the associated variables."
  (if m3-browser-process 
    (delete-process m3-browser-process)
  )
  (setq m3-browser-process ())
)

;
; debugging aids
;

(defun m3-send-debug (string)
  "Write the string to the debug buffer"
  (let ((old-buffer (current-buffer))
        (new-buffer (get-tmpbuf-create-m3 "*m3-debug*"))
       )
    (set-buffer new-buffer)
    (insert string)
    (set-buffer old-buffer)
  )
)




