#define _ASM
#include <sys/asm_linkage.h>

	ENTRY(RTStack__Flush)
	ta ST_FLUSH_WINDOWS
	retl
	nop
	SET_SIZE(RTStack__Flush)
