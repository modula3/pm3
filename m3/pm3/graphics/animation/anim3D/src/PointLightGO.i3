(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Sep 29 17:21:43 PDT 1994 by najork                   *)
(*       Created on Fri Feb  4 15:25:53 PST 1994 by najork                   *)


(* A "PointLightGO.T" is a geometric object that describes a point-shaped 
   light source which is emitting light uniformly into all directions. The
   amount of light that falls onto another geometric object depends on the 
   distance between this object and the light source. This phenomenon is 
   referred to as <EM>attenuation</EM>. 

   The underlying graphics system (PEX or OpenGL) uses the following formula
   to compute the attenuation: Let<BR>
   $L_p$ : Position of the light source<BR>
   $O_p$ : position of (some part of) an object<BR>
   $C_0$ : Constant attenuation coefficieny<BR>
   $C_1$ : Linear attenuation coefficieny<BR>
   Then the overall color attenuation factor $L_a$ for the light hitting the
   object at point $O_p$ is:
    \frac1C_0 + C_1 | O_p - L_p |  

   This attenuation model is supported by both PEX and OpenGL (in hardware). 
   Note, however, that it does not capure real-world attenuation, which
   behaves like this:
    L_a = \frac1| O_p - L_p |^2  

   OpenGL supports a third, quadratic, attenuation coefficient; PEX, however,
   does not. As lighting computation is (and, for efficiency reasons, must be)
   supported by the graphics hardware, and as right now PEX is the only API 
   to our hardware, we are stuck with its simplistic model.
*)

INTERFACE PointLightGO;

IMPORT Color, GO, LightGO, PointProp, Point3, RealProp;

TYPE 
  T <: Public;
  Public = LightGO.T OBJECT
  METHODS
    init () : T;
  END;
(* "l.init()" initializes a new point light source "l" and returns it. *)

PROCEDURE New (c : Color.T; origin : Point3.T; att0, att1 : REAL) : T;
(* "New(c,origin,att0,att1)" initializes a new point light source "l" 
   and returns it. It also attaches the following properties to "l":
   <PRE>
      (LightGO.Colour,ColourProp.NewConst(c)) 
      (LightGO.Switch,BooleanProp.NewConst(TRUE))
      (Origin,PointProp.NewConst(dir))
      (Attenuation0,RealProp.NewConst(att0))
      (Attenuation1,RealProp.NewConst(att1))
   </PRE>
*)


VAR
  Origin       : PointProp.Name;
  Attenuation0 : RealProp.Name;
  Attenuation1 : RealProp.Name;
(* In addition to the properties observed by all <TT>GO.T</TT>'s and 
   <TT>LightGO.T</TT>'s, there are three additional properties that are 
   observed by "PointLightGO.T"'s. 
   "Origin" is the name of a property that determines the origin 
   of the light emitted by the light sources; it associates with a property 
   value of type <TT>PointProp.Val</TT>. 
   "Attenuation0" is the name of a property that determines the attenuation 
   coefficient $C_0$; it associates with a property value of type 
   <TT>RealProp.Val</TT>. 
   "Attenuation1" is the name of a property that determines the attenuation 
   coefficient $C_1$; it associates with a property value of type 
   <TT>RealProp.Val</TT>. 
*)


PROCEDURE SetOrigin (o : GO.T; origin : Point3.T);
(* The expression "SetOrigin(o,origin)" is equivalent to 
   "o.setProp(Origin.bind(PointProp.NewConst(origin)))". *)

PROCEDURE SetAttenuation0 (o : GO.T; att : REAL);
(* The expression "SetAttenuation0(o,att)" is equivalent to 
   "o.setProp(Attenuation0.bind(RealProp.NewConst(att)))". *)

PROCEDURE SetAttenuation1 (o : GO.T; att : REAL);
(* The expression "SetAttenuation1(o,att)" is equivalent to 
   "o.setProp(Attenuation1.bind(RealProp.NewConst(att)))". *)

END PointLightGO.
