(* Copyright 1996-1998 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

(* The "Uglob" interface provides low-level access to the Unix "fnmatch"
   library function. *)

INTERFACE Uglob;

FROM Ctypes IMPORT int, const_char_star;

CONST
  (* "fnmatch" flags. *)
  FNM_NOESCAPE    = 16_0001;  (* Disable backslash escaping. *)
  FNM_PATHNAME    = 16_0002;  (* Slash must be matched by slash. *)
  FNM_PERIOD      = 16_0004;  (* Leading period must be matched by period. *)

  (* The following flags are non-POSIX. *)
  FNM_LEADING_DIR = 16_0008;  (* OK if pattern matches leading path prefix. *)
  FNM_CASEFOLD    = 16_0010;  (* Ignore case. *)
  FNM_PREFIX_DIRS = 16_0020;  (* Directory prefixes of pattern match too. *)

<*EXTERNAL*>
PROCEDURE fnmatch(pattern: const_char_star;
                  string: const_char_star;
		  flags: int): int;

END Uglob.
