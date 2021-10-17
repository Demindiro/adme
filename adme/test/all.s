# https://uweb.engr.arizona.edu/~ece369/Resources/spim/MIPSReference.pdf

add		$0, $0, $0
addu	$0, $0, $0
addi	$1, $0, 1
addiu	$0, $0, 0
and		$0, $0, $0
andi	$0, $0, 0
div		$0, $1
divu	$0, $1
mult	$0, $0
multu	$0, $0
nor		$0, $0, $0
or		$0, $0, $0
ori		$0, $0, 0
sll		$0, $0, 0
sllv	$0, $0, $0
sra		$0, $0, 0
srav	$0, $0, $0
srl		$0, $0, 0
srlv	$0, $0, $0
sub		$0, $0, $0
subu	$0, $0, $0
xor		$0, $0, $0
xori	$0, $0, 0

lhi		$0, 0
llo		$0, 0

slt		$0, $0, $0
sltu	$0, $0, $0
slti	$0, $0, 0
sltiu	$0, $0, 0

beq		$0, $1, end
bgtz	$0, end
blez	$1, end
bne		$0, $0, end

lb		$0, 0($0)
lbu		$0, 0($0)
lh		$0, 0($0)
lhu		$0, 0($0)
lw		$0, 0($0)

sb		$0, 0($0)
sh		$0, 0($0)
sw		$0, 0($0)

mfhi	$0
mflo	$0
mthi	$0
mtlo	$0

#trap	0

end:
	j	end


.ascii	"Hello, "
.asciiz	"world!\n"
.word	0x12345678
.half	0x9abc
.byte	0xde
