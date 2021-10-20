	la		$s0, hello_str

loop:
	lbu		$a0, 0($s0)
	beq		$a0, $zero, done
	li		$v0, 11
	syscall
	addi	$s0, $s0, 1
	j		loop
done:

	li		$t0, 0xfffffff
idle:
	addiu	$t0, $t0, 0x-1
	bne		$t0, $zero, idle

	li		$v0, 10
	syscall

hello_str:
	.asciiz	"Hello, world!\n"
