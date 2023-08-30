.global main
main:
	// inserts a literal word
	.word 0xe3a0002a // same as `mov r0, #42`
	bx lr

