module Block_mod
	implicit none
	type, public :: Block
		real :: data, grad
		contains
			procedure, pass(this) :: add => addition_definition
	end type Block
	contains
		function construct_block(variable) result(output)
			type(Block) :: output
			real :: variable
			output%data = variable
			output%grad = 0
		end function construct_block

		function addition_definition(this, other) result(output)
			class(Block), intent(in) :: this, other
			type(Block) :: output
			output = construct_block(this%data + other%data)
		end function addition_definition

end module Block_mod



module reference_mod
	use Block_mod
	implicit none
	! the Q contains references to objects taking part in certain operations
	type, public :: reference
		type(Block), pointer :: this_ptr, other_ptr
		character(len=3) :: operation
	end type reference
	contains
		function construct_reference(this, other, operation) result(output)
			type(reference) :: output
			type(Block), target :: this, other
			type(Block), pointer :: this_ptr, other_ptr
			character(len=3) :: operation
			allocate(this_ptr)
			allocate(other_ptr)
			allocate(output%this_ptr)
			allocate(output%other_ptr)
			this_ptr=>this
			other_ptr=>other
			output%this_ptr = this_ptr
			output%other_ptr = other_ptr
			output%operation = operation
		end function

end module reference_mod



module  FTL !we need a queue for the differentiation grpah
	use reference_mod
	implicit none

	type, public :: queue
		type(reference), dimension(:), allocatable :: list
		contains
			procedure, pass(this) :: append => append_definition
			procedure, pass(this) :: print => print_definition
	end type queue


	contains
		subroutine append_definition(this, first, second, operation)
			class(queue) :: this
			type(reference), dimension(:), allocatable :: queue_cpy
			type(reference) :: item
			type(Block) :: first, second
			character(len=3) :: operation


			print *, "debug"
			item = construct_reference(first, second, operation) 
			print *, "debug"
			if (.not. allocated(this%list)) then ! if the Q is empty we create it
				allocate(this%list(0)) ! THE PROBLEM IS HERE!!!
			end if
			print *, "debug"
			
			allocate(queue_cpy(size(this%list)+1)) ! we allocate a copy that's larger with 1 element and copy the contents of the origina queue 
			queue_cpy(1:size(this%list)) = this%list
			queue_cpy(size(this%list)+1) = item

			deallocate(this%list) ! deallocate the original queue
			allocate(this%list(size(queue_cpy))) ! realloacate 
			this%list = queue_cpy
			this%list(size(this%list)) = item
		end subroutine append_definition

		subroutine print_definition(this)
			class(queue) :: this
			integer :: n, i
			n = size(this%list)
			print *, n
			do i=1, n
				print *, this%list(i)%this_ptr, this%list(i)%other_ptr, this%list(i)%operation
			end do
		end subroutine print_definition
end module FTL



program main
	use FTL
	use Block_mod
	implicit none
	type(Block) :: a, b, c
	type(queue) :: graf
	!apparently problem is that i m using the reference type for array elements



	a = construct_block(3.0)
	b = construct_block(4.0)
	call graf%append(a, b, 'add')
	call graf%append(a, b, 'add')
	call graf%print()
	! call graf%append(a, b, 'add')

	! call graf%print()
	! type(queue) :: q
	! type(Block) :: a
	! call q%append(3.0)
	! call q%append(4.0)
	! call q%print()
	! call q%print
	! type(Block) :: A, B, C
	! A = construct(30.0)
	! B = construct(40.0)
	! C = A%add(B)
	! print *, C%data
end program main
