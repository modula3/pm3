(* 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon Jan 30 15:33:55 1995
 * Last Modified By: Douglas H. Quebbeman
 * Last Modified On: Sun May 26 15:26:00 2002
 * Update Count    : 85
 * 
 *)

INTERFACE Utermio;

FROM Ctypes IMPORT char, int;
FROM Utypes IMPORT u_short, u_char, u_int;

CONST
  (* Taken from /usr/include/sys/termio.h and related files. *)
  CBAUD         = 8_0010017;
  BAUDBITS      = 8_0010017;
  B0      	= 8_0000000;
  B50     	= 8_0000001;
  B75     	= 8_0000002;
  B110    	= 8_0000003;
  B134    	= 8_0000004;
  B150    	= 8_0000005;
  B200    	= 8_0000006;
  B300    	= 8_0000007;
  B600    	= 8_0000010;
  B1200   	= 8_0000011;
  B1800   	= 8_0000012;
  B2400   	= 8_0000013;
  B4800   	= 8_0000014;
  B9600   	= 8_0000015;
  B14400        = -1;			(* unsupported should be marked thus *)
  B19200  	= 8_0000016;
  B38400  	= 8_0000017;
  B57600  	= 8_0010001;
  B115200 	= 8_0010002;
  B230400 	= 8_0010003;
  B460800 	= 8_0010004;

  CSIZE         = 8_00000060;
  CS5           = 8_00000000;
  CS6           = 8_00000020;
  CS7           = 8_00000040;
  CS8		= 8_00000060;

  PARITYBITS    = 8_00001400;
  PARNONE       = 8_00000000;
  PAREVEN	= 8_00000400;
  PARODD	= 8_00001400;

  (* Input Modes *)
  IGNBRK	= 8_00000001;
  BRKINT        = 8_00000002;
  IGNPAR	= 8_00000004;
  PARMRK        = 8_00000010;
  INPCK         = 8_00000020;
  ISTRIP        = 8_00000040;
  INLCR         = 8_00000100;
  IGNCR         = 8_00000200;
  ICRNL         = 8_00000400;

  (* line discipline modes *)
  ISIG          = 8_00000001;
  ICANON        = 8_00000002;
  ECHO          = 8_00000010;
  ECHOE         = 8_00000020;
  ECHOK         = 8_00000040;
  ECHONL        = 8_00000100;
  NOFLSH        = 8_00000200;
  TOSTOP        = 8_00000400;

  (* control modes *)
  CSTOPB	= 8_00000100;
  CREAD		= 8_00000200;
  HUPCL         = 8_00002000;
  CLOCAL	= 8_00004000;

  NCC		= 8;
  NCCS		= 32;

  (* control characters *)
  VINTR         = 0;
  VQUIT         = 1;
  VERASE        = 2;
  VKILL         = 3;
  VEOF          = 4;
  VEOL          = 5;
  VEOL2         = 6;
  VMIN          = 4;
  VTIME         = 5;
  VSWTCH        = 7;
  VSTART        = 8;
  VSTOP         = 9;
  VSUSP         = 10;
  VDSUSP        = 11;
  VREPRINT      = 12;
  VDISCARD      = 13;
  VWERASE       = 14;
  VLNEXT        = 15;

  TCIFLUSH	= 0;  (* flush data received but not read *)
  TCOFLUSH	= 1;  (* flush data written but not transmitted *)
  TCIOFLUSH	= 2;  (* flush both data both input and output queues *)

  TCSANOW   = 0;
  TCSADRAIN = 1;
  TCSAFLUSH = 2;

TYPE
  struct_winsize = RECORD
	ws_row, ws_col:       u_short; 	(* Window charact. size *)
	ws_xpixel, ws_ypixel: u_short;	(* Window pixel size	*)
  END;

  struct_termio = RECORD
	c_iflag:   u_short := 0;	 (* input modes *)
	c_oflag:   u_short := 0;	 (* output modes *)
	c_cflag:   u_short := 0;	 (* control modes *)
	c_lflag:   u_short := 0;	 (* line discipline modes *)
	c_line:    char    := VAL(0, char); (* line discipline *)
	c_cc := ARRAY [0..NCC-1] OF u_char{0,..}; (* control chars *)
  END;

  lines = int;

  speed_t  = u_int;
  cc_t     = u_char;
  tcflag_t = u_int;

  struct_termios = RECORD
	c_iflag:   tcflag_t := 0;	 (* input modes *)
	c_oflag:   tcflag_t := 0;	 (* output modes *)
	c_cflag:   tcflag_t := 0;	 (* control modes *)
	c_lflag:   tcflag_t := 0;	 (* local mode flags *)
        c_line:    cc_t;                 (* line discipline modes *)
	c_cc := ARRAY [0..NCCS-1] OF cc_t{0,..}; (* control chars *)
	c_ispeed:  speed_t  := 0;	 (* input speed *)
	c_ospeed:  speed_t  := 0;	 (* output speed *)
  END;

<*EXTERNAL*> 
PROCEDURE tcgetattr (fildes: int; 
                     termios_p: UNTRACED REF struct_termios): int; 

<*EXTERNAL*> 
PROCEDURE tcsetattr(fildes, optional_actions: int;
                    termios_p: UNTRACED REF struct_termios) : int;

<*EXTERNAL*> PROCEDURE tcsendbreak(fildes, duration: int): int;

<*EXTERNAL*> PROCEDURE tcdrain(fildes: int): int;

<*EXTERNAL*> PROCEDURE tcflush(fildes, queue_selector: int): int;

<*EXTERNAL*> PROCEDURE  tcflow(fildes, action: int): int;

<*EXTERNAL*> 
PROCEDURE cfgetospeed(termios_p: UNTRACED REF struct_termios): speed_t;

<*EXTERNAL*> 
PROCEDURE cfsetospeed(termios_p: UNTRACED REF struct_termios;
                      speed: speed_t): int;

<*EXTERNAL*> 
PROCEDURE cfgetispeed(termios_p: UNTRACED REF struct_termios): speed_t;

<*EXTERNAL*> 
PROCEDURE cfsetispeed(termios_p: UNTRACED REF struct_termios;
                      speed: speed_t): int;

END Utermio.


