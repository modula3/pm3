(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
SAFE MODULE test;

IMPORT Text, Wr;
FROM Stdio IMPORT stdout;

PROCEDURE P( wr: Wr.T );
VAR
    s: ARRAY [0..19] OF CHAR;
    t: Text.T;
BEGIN
    PRINTF( wr, "Hello world.\n" );
    PRINTF( wr, "Hello %% world.\n" );

    PRINTF( wr, "d = |%d|\n", 105 );
    PRINTF( wr, "4d = |%4d|\n", 105 );
    PRINTF( wr, "05d = |%05d|\n", 105 );
    PRINTF( wr, "-05d = |%-05d|\n", 105 );
    PRINTF( wr, "-5d = |%-5d|\n", 105 );
    PRINTF( wr, "2d = |%2d|\n", 105 );

    PRINTF( wr, "u = |%u|\n", 4294967295 );
    PRINTF( wr, "2u = |%2u|\n", 4294967295 );
    PRINTF( wr, "12u = |%12u|\n", 4294967295 );
    PRINTF( wr, "-12u = |%-12u|\n", 4294967295 );
    PRINTF( wr, "012u = |%012u|\n", 4294967295 );
    PRINTF( wr, "-012u = |%-012u|\n", 4294967295 );

    PRINTF( wr, "o = |%o|\n", 4294967295 );
    PRINTF( wr, "2o = |%2o|\n", 4294967295 );
    PRINTF( wr, "12o = |%12o|\n", 4294967295 );
    PRINTF( wr, "-12o = |%-12o|\n", 4294967295 );
    PRINTF( wr, "012o = |%012o|\n", 4294967295 );
    PRINTF( wr, "-012o = |%-012o|\n", 4294967295 );
    PRINTF( wr, "-12#o = |%-12#o|\n", 4294967295 );

    PRINTF( wr, "x = |%x|\n", 4294967295 );
    PRINTF( wr, "2x = |%2x|\n", 4294967295 );
    PRINTF( wr, "12x = |%12x|\n", 4294967295 );
    PRINTF( wr, "-12x = |%-12x|\n", 4294967295 );
    PRINTF( wr, "012x = |%012x|\n", 4294967295 );
    PRINTF( wr, "-012x = |%-012x|\n", 4294967295 );
    PRINTF( wr, "-12#x = |%-12#x|\n", 4294967295 );

    PRINTF( wr, "c = |%c|\n", '@' );
    PRINTF( wr, "4c = |%4c|\n", '@' );
    PRINTF( wr, "-4c = |%-4c|\n", '@' );

    s[ 0 ] := 'a';
    s[ 1 ] := 'b';
    s[ 2 ] := 'c';
    s[ 3 ] := 'd';
    s[ 4 ] := '\000';

    PRINTF( wr, "s = |%s|\n", s );
    PRINTF( wr, "2s = |%2s|\n", s );
    PRINTF( wr, "6s = |%6s|\n", s );
    PRINTF( wr, "-6s = |%-6s|\n", s );
    PRINTF( wr, "6.10s = |%6.10s|\n", s );
    PRINTF( wr, "6.3s = |%6.3s|\n", s );
    PRINTF( wr, "-6.3s = |%-6.3s|\n", s );

    t := "abcd";
    PRINTF( wr, "t = |%t|\n", t );
    PRINTF( wr, "2t = |%2t|\n", t );
    PRINTF( wr, "6t = |%6t|\n", t );
    PRINTF( wr, "-6t = |%-6t|\n", t );
    PRINTF( wr, "6.10t = |%6.10t|\n", t );
    PRINTF( wr, "6.3t = |%6.3t|\n", t );
    PRINTF( wr, "-6.3t = |%-6.3t|\n", t );
    PRINTF( wr, "6.3t = |%6.3t|\n", t );
    
    PRINTF( wr, "-*.*t = |%-*.*t|\n", 6, 3, t ); 

    PRINTF( wr, "lf = |%lf|\n", 3.123456789876543D0 );
    PRINTF( wr, "3lf = |%3lf|\n", 3.123456789876543D0 );
    PRINTF( wr, "25lf = |%25lf|\n", 3.123456789876543D0 );
    PRINTF( wr, "-25lf = |%-25lf|\n", 3.123456789876543D0 );
    PRINTF( wr, "025lf = |%025lf|\n", 3.123456789876543D0 );
    PRINTF( wr, "25.5lf = |%25.5lf|\n", 3.123456789876543D0 );
    PRINTF( wr, "25.10lf = |%25.10lf|\n", 3.123456789876543D0 );
    
    PRINTF( wr, "f = |%f|\n", 3.123456789876543 );
    PRINTF( wr, "3f = |%3f|\n", 3.123456789876543 );
    PRINTF( wr, "25f = |%25f|\n", 3.123456789876543 );
    PRINTF( wr, "-25f = |%-25f|\n", 3.123456789876543 );
    PRINTF( wr, "025f = |%025f|\n", 3.123456789876543 );
    PRINTF( wr, "25.5f = |%25.5f|\n", 3.123456789876543 );
    PRINTF( wr, "25.10f = |%25.10f|\n", 3.123456789876543 );

    PRINTF( wr, "le = |%le|\n", 3.123456789876543D0 );
    PRINTF( wr, "3le = |%3le|\n", 3.123456789876543D0 );
    PRINTF( wr, "25le = |%25le|\n", 3.123456789876543D0 );
    PRINTF( wr, "-25le = |%-25le|\n", 3.123456789876543D0 );
    PRINTF( wr, "025le = |%025le|\n", 3.123456789876543D0 );
    PRINTF( wr, "25.5le = |%25.5le|\n", 3.123456789876543D0 );
    PRINTF( wr, "25.10le = |%25.10le|\n", 3.123456789876543D0 );
    
    PRINTF( wr, "e = |%e|\n", 3.123456789876543 );
    PRINTF( wr, "3e = |%3e|\n", 3.123456789876543 );
    PRINTF( wr, "25e = |%25e|\n", 3.123456789876543 );
    PRINTF( wr, "-25e = |%-25e|\n", 3.123456789876543 );
    PRINTF( wr, "025e = |%025e|\n", 3.123456789876543 );
    PRINTF( wr, "25.5e = |%25.5e|\n", 3.123456789876543 );
    PRINTF( wr, "25.10e = |%25.10e|\n", 3.123456789876543 );

    PRINTF( wr, "lg = |%lg|\n", 3.123456789876543D0 );
    PRINTF( wr, "3lg = |%3lg|\n", 3.123456789876543D0 );
    PRINTF( wr, "25lg = |%25lg|\n", 3.123456789876543D0 );
    PRINTF( wr, "-25lg = |%-25lg|\n", 3.123456789876543D0 );
    PRINTF( wr, "025lg = |%025lg|\n", 3.123456789876543D0 );
    PRINTF( wr, "25.5lg = |%25.5lg|\n", 3.123456789876543D0 );
    PRINTF( wr, "25.10lg = |%25.10lg|\n", 3.123456789876543D0 );
    
    PRINTF( wr, "g = |%g|\n", 3.123456789876543 );
    PRINTF( wr, "3g = |%3g|\n", 3.123456789876543 );
    PRINTF( wr, "25g = |%25g|\n", 3.123456789876543 );
    PRINTF( wr, "-25g = |%-25g|\n", 3.123456789876543 );
    PRINTF( wr, "025g = |%025g|\n", 3.123456789876543 );
    PRINTF( wr, "25.5g = |%25.5g|\n", 3.123456789876543 );
    PRINTF( wr, "25.10g = |%25.10g|\n", 3.123456789876543 );

    PRINTF( wr, "lg = |%lg|\n", 3.123456789876D-15 );
    PRINTF( wr, "3lg = |%3lg|\n", 3.123456789876D-15 );
    PRINTF( wr, "25lg = |%25lg|\n", 3.123456789876D-15 );
    PRINTF( wr, "-25lg = |%-25lg|\n", 3.123456789876D-15 );
    PRINTF( wr, "025lg = |%025lg|\n", 3.123456789876D-15 );
    PRINTF( wr, "25.5lg = |%25.5lg|\n", 3.123456789876D-15 );
    PRINTF( wr, "25.10lg = |%25.10lg|\n", 3.123456789876D-15 );
    
    PRINTF( wr, "g = |%g|\n", 3.123456789876E-15 );
    PRINTF( wr, "3g = |%3g|\n", 3.123456789876E-15 );
    PRINTF( wr, "25g = |%25g|\n", 3.123456789876E-15 );
    PRINTF( wr, "-25g = |%-25g|\n", 3.123456789876E-15 );
    PRINTF( wr, "025g = |%025g|\n", 3.123456789876E-15 );
    PRINTF( wr, "25.5g = |%25.5g|\n", 3.123456789876E-15 );
    PRINTF( wr, "25.10g = |%25.10g|\n", 3.123456789876E-15 );

    PRINTF( wr, "#g = |%#g|\n", 2.0 );
    PRINTF( wr, "#g = |%#g|\n", 2.1 );
    END P;

PROCEDURE Main();
VAR
    wr:   Wr.T;
    text: Text.T;
BEGIN
(*    P( stdout ); *)

    wr := Wr.New();
    P( wr );
    text := Wr.ToText( wr );
    ASSERT( Text.Equal( text, "\
Hello world.\n\
Hello % world.\n\
d = |105|\n\
4d = | 105|\n\
05d = |00105|\n\
-05d = |10500|\n\
-5d = |105  |\n\
2d = |105|\n\
u = |4294967295|\n\
2u = |4294967295|\n\
12u = |  4294967295|\n\
-12u = |4294967295  |\n\
012u = |004294967295|\n\
-012u = |429496729500|\n\
o = |37777777777|\n\
2o = |37777777777|\n\
12o = | 37777777777|\n\
-12o = |37777777777 |\n\
012o = |037777777777|\n\
-012o = |377777777770|\n\
-12#o = |037777777777|\n\
x = |ffffffff|\n\
2x = |ffffffff|\n\
12x = |    ffffffff|\n\
-12x = |ffffffff    |\n\
012x = |0000ffffffff|\n\
-012x = |ffffffff0000|\n\
-12#x = |0xffffffff  |\n\
c = |@|\n\
4c = |   @|\n\
-4c = |@   |\n\
s = |abcd|\n\
2s = |abcd|\n\
6s = |  abcd|\n\
-6s = |abcd  |\n\
6.10s = |  abcd|\n\
6.3s = |   abc|\n\
-6.3s = |abc   |\n\
t = |abcd|\n\
2t = |abcd|\n\
6t = |  abcd|\n\
-6t = |abcd  |\n\
6.10t = |  abcd|\n\
6.3t = |   abc|\n\
-6.3t = |abc   |\n\
6.3t = |   abc|\n\
-*.*t = |abc   |\n\
lf = |3.123457|\n\
3lf = |3.123457|\n\
25lf = |                 3.123457|\n\
-25lf = |3.123457                 |\n\
025lf = |000000000000000003.123457|\n\
25.5lf = |                  3.12346|\n\
25.10lf = |             3.1234567899|\n\
f = |3.123457|\n\
3f = |3.123457|\n\
25f = |                 3.123457|\n\
-25f = |3.123457                 |\n\
025f = |000000000000000003.123457|\n\
25.5f = |                  3.12346|\n\
25.10f = |             3.1234567165|\n\
le = |3.123457e+00|\n\
3le = |3.123457e+00|\n\
25le = |             3.123457e+00|\n\
-25le = |3.123457e+00             |\n\
025le = |00000000000003.123457e+00|\n\
25.5le = |              3.12346e+00|\n\
25.10le = |         3.1234567899e+00|\n\
e = |3.123457e+00|\n\
3e = |3.123457e+00|\n\
25e = |             3.123457e+00|\n\
-25e = |3.123457e+00             |\n\
025e = |00000000000003.123457e+00|\n\
25.5e = |              3.12346e+00|\n\
25.10e = |         3.1234567165e+00|\n\
lg = |3.12345|\n\
3lg = |3.12345|\n\
25lg = |                  3.12345|\n\
-25lg = |3.12345                  |\n\
025lg = |0000000000000000003.12345|\n\
25.5lg = |                   3.1234|\n\
25.10lg = |              3.123456789|\n\
g = |3.12345|\n\
3g = |3.12345|\n\
25g = |                  3.12345|\n\
-25g = |3.12345                  |\n\
025g = |0000000000000000003.12345|\n\
25.5g = |                   3.1234|\n\
25.10g = |              3.123456716|\n\
lg = |3.123457e-15|\n\
3lg = |3.123457e-15|\n\
25lg = |             3.123457e-15|\n\
-25lg = |3.123457e-15             |\n\
025lg = |00000000000003.123457e-15|\n\
25.5lg = |              3.12346e-15|\n\
25.10lg = |         3.1234567899e-15|\n\
g = |3.123457e-15|\n\
3g = |3.123457e-15|\n\
25g = |             3.123457e-15|\n\
-25g = |3.123457e-15             |\n\
025g = |00000000000003.123457e-15|\n\
25.5g = |              3.12346e-15|\n\
25.10g = |         3.1234568629e-15|\n\
#g = |2.00000|\n\
#g = |2.10000|\n\
" ) );
    END Main;
    
BEGIN
    Main();
    END test.
