(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 23 14:29:02 PDT 1994 by najork                   *)
(*       Created on Thu Feb 10 11:51:21 PST 1994 by najork                   *)

(* A "PerspCameraGO.T" is a perspective camera, i.e.\ a camera which shows 
   a scene from a certain point (and depicts perspective relations naturally).
   The volume of space observed by such a camera forms an (infinitely high) 
   pyramid. The shape of the pyramid is determined by the following parameters:
   \begin{itemize}
   \item The position of the camera (which determines the tip of the pyramid)
   \item A point on the central viewing axis (which determines the vertical
         axis of the pyramid)
   \item The up-vector (which rotates the pyramid around its vertical axis)
   \item The field of vision (the angle between the ``up'' and the ``down''
         wall of the pyramid)
   \item The aspect ratio (the ratio between height and width of any
         cross-section of the pyramid)
   \end{itemize}

   The figure below illustrates the role of the parameters:
   
   \begin{center}
   \begin{tabular}{c}
   \psfig{figure=images/PerspCamera.ps,width=4in,silent=} 
   \end{tabular}
   \end{center}
*)

INTERFACE PerspCameraGO;

IMPORT CameraGO, Point3, RealProp;

TYPE
  T <: Public;
  Public = CameraGO.T OBJECT 
  METHODS
    init () : T;
  END;
(* "pc.init()" initializes a new perspective camera and returns it. *)

PROCEDURE New (from, to, up : Point3.T; fovy : REAL) : T;
(* "New(from,to,up,fovy)" creates a new perspective camera "cam" and 
   returns it. It also attaches the following properties to "cam":
   \begin{verbatim}
      (CameraGO.From,PointProp.NewConst(from))
      (CameraGO.To,PointProp.NewConst(to))
      (CameraGO.Up,PointProp.NewConst(up))
      (CameraGO.Aspect,RealProp.NewConst(1.0))
      (Fovy,RealProp.NewConst(fovy))
   \end{verbatim}
*)

VAR
  Fovy : RealProp.Name;
(* In addition to the properties observed by all \type{GO}{T}'s and 
   \type{CameraGO}{T}'s, there is one additional property that is observed 
   by "PerspCameraGO.T"'s. 
   "Fovy" is the name of a property that determines the field-of-vision,
   i.e.\ the angle (in degree radians) between the ``up'' and the ``down''
   wall of the pyramid; it associates with a property value of type 
   \type{RealProp}{Val}. The angle between ``left'' and ``right'' wall is 
   determined by the aspect ratio. *)

END PerspCameraGO.
