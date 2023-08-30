.data
	hello: .string "hello worms\n"
	
.text
	.global main
	main:
		push {ip, lr}

		ldr r0, =hello		// r0 = hello
		bl printf			// printf(r0)

		mov r0, #41			// r0 = 41
		add r0, r0, #1		// r0 = r0 + 1

		pop {ip, lr}
		bx lr
