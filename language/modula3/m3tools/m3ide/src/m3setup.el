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
; By loading this file, m3ide will be used automatically when files
; ending in .i3, .m3, .ig, .mg or .fv are used.
;

(autoload 'm3-ide-mode "m3ide")
(autoload 'formsvbt-ide-mode "m3ide")
(autoload 'm3makefile-ide-mode "m3ide")
(setq auto-mode-alist
  (append
    '(
      ("\\.ig$" . m3-ide-mode)
      ("\\.mg$" . m3-ide-mode)
      ("\\.i3$" . m3-ide-mode)
      ("\\.m3$" . m3-ide-mode)
      ("\\.fv$" . formsvbt-ide-mode)
      ("m3makefile$" . m3makefile-ide-mode)
    )
    auto-mode-alist
  )
)
