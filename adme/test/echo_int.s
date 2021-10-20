_start:
	# Read integer from input
	li		$v0, 5		# READ_INT
	syscall
	move	$s0, $v0

	# Write sentence with integer to output
	la		$a0, str_a
	li		$v0, 4		# PRINT_STRING
	syscall
	move	$a0, $s0
	li		$v0, 1		# PRINT_INT
	syscall
	la		$a0, str_b
	li		$v0, 4		# PRINT_STRING
	syscall
	
	# Exit
	li		$v0, 10			# EXIT
	syscall
	
str_a:
	.asciiz "You entered "
str_b:
	.asciiz "\n"
