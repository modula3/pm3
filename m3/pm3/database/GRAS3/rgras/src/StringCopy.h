/* Headerdatei fuer StringCopy. */
/* Automatisch generiert */
/* Typedeklarationen stehen in C_Global.h in Abschnitt StringCopy */

#if !defined (C_StringCopy_h_)
#define C_StringCopy_h_
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <ansi.h>
#include <string.h>
#include <stdio.h>
#include <rgglobal.h>
#include <CTypesForM3.h>

void *
CopyAttributeStoT(char * s, CARDINAL len);

void
CopyTtoS(void * t, char * s);

void *
CopyStoT(char * s);
