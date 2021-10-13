	li		$1, hello_str
	li		$3, 1000

loop:
	lbu		$2, 0($1)
	beq		$2, $0, done
	sb		$2, 0($3)
	addi	$1, $1, 1
	addi	$3, $3, 1
	j		loop
done:

idle:
	j	idle

hello_str:
	.asciz	"Hello, world!"
