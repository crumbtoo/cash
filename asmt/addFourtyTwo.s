.text
	.global main
	main:
		push {ip, lr}

		mov r0, #0			// r0 = 0
		bl addFourtyTwo		// lr = pc; pc addFourtyTwo
		sub r0, r0, #3		// r0 = r0 - 3

		pop {ip, lr}
		bx lr				// return r0

	addFourtyTwo:
		add r0, r0, #42		// r0 = r0 + 42
		bx lr				// pc = lr

