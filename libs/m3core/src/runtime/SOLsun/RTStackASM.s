#define _ASM
#include <sys/asm_linkage.h>

	ENTRY(RTStack__CurFrame)
	ta ST_FLUSH_WINDOWS
	st %o7, [%o0]
	st %fp, [%o0 + 4]
	st %sp, [%o0 + 8]
	retl
	nop
