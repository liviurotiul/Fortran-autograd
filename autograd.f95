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


		subroutine addition_definition(this, first_o, second_o)
			class(Block) :: this
			type(Block) :: first_o, second_o
			this%data = first_o%data + second_o%data
		end subroutine


end module Block_mod



module reference_mod
	use Block_mod
	implicit none
	! the Q contains references to objects taking part in certain operations
	type, public :: reference
		type(Block), pointer :: this_ptr, other_ptr, result_ptr
		character(len=3) :: operation
	end type reference
	contains
		function construct_reference(this, other, result, operation) result(output)
			type(reference) :: output
			type(Block), target :: this, other, result
			type(Block), pointer :: this_ptr, other_ptr, result_ptr
			character(len=3) :: operation
			allocate(this_ptr)
			allocate(other_ptr)
			allocate(result_ptr)
			allocate(output%this_ptr)
			allocate(output%other_ptr)
			allocate(output%result_ptr)
			this_ptr=>this
			other_ptr=>other
			result_ptr=>result
			output%this_ptr = this_ptr
			output%other_ptr = other_ptr
			output%result_ptr = result_ptr
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
			! procedure, pass(this) :: backward => backward_definition
	end type queue


	contains
		subroutine append_definition(this, first, second, result, operation)
			class(queue) :: this
			type(reference), dimension(:), allocatable :: queue_cpy
			type(reference) :: item
			type(Block) :: first, second, result
			character(len=3) :: operation


			print *, "debug"
			item = construct_reference(first, second, result, operation) 
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
				print *, this%list(i)%this_ptr, this%list(i)%other_ptr, this%list(i)%result_ptr, this%list(i)%operation
			end do
			print *, ''
		end subroutine print_definition

		subroutine backward_definition(this) 
			class(queue) :: this
			integer :: n, i
			n = size(this%list)
			!the last one has the gradient of one because is equal to dy/dy
			this%list(n)%this_ptr%grad = 1.0
			do i=n-1, 1
				this%list(i)%this_ptr%grad = this%list(i)%this_ptr%grad + this%list(i+1)%this_ptr%grad
				this%list(i)%other_ptr%grad = this%list(i)%other_ptr%grad + this%list(i+1)%other_ptr%grad
			end do
		end subroutine

end module FTL



program main
	use FTL
	use Block_mod
	implicit none
	type(Block) :: a, b, c
	type(queue) :: graf
	!apparently problem is that i m using the reference type for array elements

	a = construct_block(0.0)
	b = construct_block(0.0)
	c = construct_block(0.0)

	call graf%append(a, b, c, 'add')

	call c%add(a, b)
	call graf%print()
	print *, c

end program main
