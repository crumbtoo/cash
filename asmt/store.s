// rw data
.data
	overwrite: .string "donk"
	.balign 4
	storage: .string "doge\n"

// ro data
.text
	.global main
	main:
		push {ip, lr}

		// print `storage`, which starts as "doge\n"
		ldr r0, =storage
		bl printf

		// set r2 to the address `storage`
		ldr r2, =storage		// r2 = storage

		// set r1 to the address `overwrite`, then to the
		// word pointed to by `overwrite`
		ldr r1, =overwrite		// r1 = overwrite
		ldr r1, [r1]			// r1 = *r1

		// store the word r1 in the address pointed to by r2
		str r1, [r2]			// *r2 = r1

		// print `storage`, which is now "donk\n"
		ldr r0, =storage
		bl printf

		pop {ip, lr}
		bx lr

