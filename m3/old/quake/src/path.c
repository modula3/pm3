/*
 * Copyright (C) 1993, Digital Equipment Corporation
 * All rights reserved.
 * See the file COPYRIGHT for a full description. 
 *
 * by Stephen Harrison
 *
 * Last modified on Mon Aug 12 15:27:21 PDT 1996 by najork   
 *      modified on Thu Oct 13 08:13:14 PDT 1994 by kalsow   
 *      modified on Mon Jan 31 09:19:02 1994 by harrison 
 *      modified on Wed Apr 21 18:03:34 PDT 1993 by mjordan 
 */

#include "quake.h"

Boolean Path_IsAbsolute(path)
String path;
{
    if (path->body == NULL)  return FALSE;
    if (*(path->body) == DIR_SEPARATOR)  return TRUE;
    if (strchr (path->body, VOL_SEPARATOR) != NULL) return TRUE;
    return FALSE;
}

char *Path_LastSep (s)
char *s;
{
  char *last = NULL;
  if (s != NULL) {
    for (;  *s;  s++) {
      if ((*s == DIR_SEPARATOR) || (*s == VOL_SEPARATOR)) {
        last = s;
      }
    }
  }
  return last;
}

String Path_ExtractFile(anything)
String anything;
{
    char *last_sep = Path_LastSep (anything->body);

    if (last_sep == NULL) return anything;
    return String_New(last_sep + 1);
}

String Path_ExtractPath(anything)
String anything;
{
    char *last_sep = Path_LastSep (anything->body);

    if (last_sep == NULL) return String_New("");

    {
	int length = last_sep - anything->body;
	char path[MAXPATHLEN];

	Utils_CopyMemory(anything->body, path, length);
	path[length] = '\0';

	return String_New(path);
    }
}

static void Normalize(src, dst)
char *src;
char *dst;
{
    char *stack[100], *first = dst;
    int sp = 0;
    Boolean full = FALSE;
    
    if (*src == '\0' || (*src == '.' && src[1] == '\0'))
	*dst++ = '.';
    else {
	if (*src == DIR_SEPARATOR) {
	    *dst++ = *src++;
#ifdef	TARGET_NT386
            // server specific names start "\\"
#else
	    while (*src == DIR_SEPARATOR) src++;
#endif
	    full = TRUE;
	}
	first = dst;
	while (*src) {
	    /* ./  foobar/. */
	    if (*src == '.' && (src[1] == '\0' || src[1] == DIR_SEPARATOR)) {
		src++;
		while (*src == DIR_SEPARATOR) src++;
		continue;
	    }
	    if (*src == '.' && src[1] == '.') {   /* ../ foobar/.. */
		if (src[2] == '\0' || src[2] == DIR_SEPARATOR) {
		    src += 2;
		    if (sp)
			dst = stack[--sp];
		    else if (!full) {
			*dst++ = '.';
			*dst++ = '.';
			if (*src == DIR_SEPARATOR) *dst++ = *src++;
		    }
		    while (*src == DIR_SEPARATOR) src++;
		    continue;
		}
	    }
	    stack[sp++] = dst;
	    while (*src != '\0' && *src != DIR_SEPARATOR) *dst++ = *src++;
	    if (*src == DIR_SEPARATOR) *dst++ = *src++;
	    while (*src == DIR_SEPARATOR) src++;
	}
    }

    /* strip trailing directory separators */
    while (dst > first && dst[-1] == DIR_SEPARATOR) dst--;

    /* make sure we didn't clobber memory... */
    assert(dst - first <= MAXPATHLEN);
    *dst = '\0';
}

String Path_Normalize(unfixed_path)
String unfixed_path;
{
    char normalized[MAXPATHLEN + 1];

    Normalize(unfixed_path->body, normalized);

    if (strcmp(unfixed_path->body, normalized) == 0)
	return unfixed_path;

    return String_New(normalized);
}

String Path_MakeRelative(current_path, unfixed_path)
String current_path;
String unfixed_path;
{
    if (current_path != NULL &&
	strlen(current_path->body) > 0) {
	char *current = current_path->body;
	char *unfixed = unfixed_path->body;
	int current_len = strlen(current);

	if (strncmp(current, unfixed, current_len) == 0) {
	    if (unfixed[current_len] == DIR_SEPARATOR)
	        return String_New(unfixed + current_len + 1);

	    /* Special case for current == root. */
	    if ((*current == DIR_SEPARATOR) && (current[1] == '\0'))
	        return String_New(unfixed + 1);
	}
    }

    return unfixed_path;
}

