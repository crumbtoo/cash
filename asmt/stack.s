.global main
main:
	sub sp, sp, #16		// sp = sp - 16

	str r0, [sp, #0]	// *(sp + 0) = r0
	str r1, [sp, #4]	// *(sp + 4) = r1
	str r2, [sp, #8]	// *(sp + 4) = r2

	add sp, sp, #16		// sp = sp + 16
	bx lr

