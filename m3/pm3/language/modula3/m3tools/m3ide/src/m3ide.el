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
; Load the needed lisp files
;

(require 'modula3)
(require 'm3menu)
(provide 'm3ide)

;
; For each file type, setup the relevant variables which will determine
; the menu items and commands available
;

(defun m3-ide-mode () (interactive)
  "start the modula-3 mode with the ide process and menus"
  (modula-3-mode)
  (make-local-variable 'm3-is-form)
  (make-local-variable 'm3-is-program)
  (setq m3-is-form ())
  (setq m3-is-program t)
  (check-m3-menu)
  ; insert a header if empty?
)

(defun formsvbt-ide-mode () (interactive)
  "start the modula-3 mode with the ide process and menus"
  (make-local-variable 'm3-is-form)
  (make-local-variable 'm3-is-program)
  (setq m3-is-form t)
  (setq m3-is-program ())
  (check-m3-menu)
  ; insert a header if empty?
)

(defun m3makefile-ide-mode () (interactive)
  "start the modula-3 mode with the ide process and menus"
  (make-local-variable 'm3-is-form)
  (make-local-variable 'm3-is-program)
  (setq m3-is-form ())
  (setq m3-is-program ())
  (check-m3-menu)
  ; insert a header if empty?
)


