// note that our string is declared in .text
hello:
	// our hello string is 13 bytes, which pushes `main` to an unaligned address
	// this directive adds padding to align `main` to an alligned 32-bit word
	// .string "hello worms\n"
	// .balign 4

	// we can also pad our string with null bytes to align it
	.string "hello worms\n\0\0\0"

.global main
main:
	push {ip, lr}

	ldr r0, =hello
	bl printf

	mov r0, #41
	add r0, r0, #1

	pop {ip, lr}
	bx lr

