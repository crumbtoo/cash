.global main
main:
	// push ip to maintain 2-word alignment
	push {ip, lr}

	// allocate one word (4 bytes)
	mov r0, #4
	bl malloc

	// store 42 in the allocated word
	mov r3, #42
	str r3, [r0]

	// dealloc
	bl free

	pop {ip, pc}
	bx lr

