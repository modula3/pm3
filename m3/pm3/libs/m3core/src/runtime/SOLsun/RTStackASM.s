	.section ".text"
	.align 4
	.global RTStack__Flush
	.type RTStack__Flush, #function
RTStack__Flush: 
	ta 0x03
	retl
	nop
	.size RTStack__Flush, (.-RTStack__Flush)
