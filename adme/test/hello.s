	li		t0, hello_str
	li		t1, 8192

loop:
	lbu		t2, 0(t0)
	beq		t2, zero, done
	sb		t2, 0(t1)
	addi	t0, t0, 1
	j		loop
done:

idle:
	j	idle

hello_str:
	.asciz	"Hello, world!"
