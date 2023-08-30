.data
	fmt: .string "%s %c %c\n"
	hello: .string "doge"

.text
	.global main
	main:
		push {ip, lr}

		// load the address `fmt` into r0
		ldr r0, =fmt			// r0 = fmt

		// load the address `hello` into r1
		ldr r1, =hello			// r1 = hello

		// load the address `hello` into r2, then dereference it
		ldr r2, =hello			// r2 = hello
		ldr r2, [r2]			// r2 = *r2

		// load the address `hello` into r3, then
		// dereference with an (immediate) offset.
		// offset may be negative.
		ldr r3, =hello			// r3 = hello
		ldr r3, [r3, #2]		// r3 = *(r3 + 8)

		bl printf				// printf(r0, r1, r2)

		pop {ip, lr}
		bx lr					// return

