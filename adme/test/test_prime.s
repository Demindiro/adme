_start:
	# Read prime
	li		$v0, 5		# READ_INT
	syscall
	move	$s0, $v0

	# Setup loop
	li		$s1, 2
loop:
	divu	$s0, $s1
	mfhi	$t0
	beq		$t0, $zero, not_prime
	# Increment, then check if we should begin next iteration
	addi	$s1, $s1, 1
	blt		$s1, $s0, loop
	
	# It is prime
	la		$a0, prime_str
	j		print

	# It is not prime
not_prime:
	la		$a0, not_prime_str
	
print:	
	# Print
	li		$v0, 4		# PRINT_STRING
	syscall
	
	# Exit
	li		$v0, 10		# EXIT
	syscall
	
prime_str:
	.asciiz	"Prime\n"
not_prime_str:
	.asciiz	"Not prime\n"
