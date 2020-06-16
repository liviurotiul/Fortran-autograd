module Block_mod
	implicit none
	type, public :: Block
		real :: data, grad
		contains
			procedure, pass(this) :: add => addition_definition
			procedure, pass(this) :: pass => pass_data
			procedure, pass(this) :: pass_grad => pass_gradient
	end type Block
	contains
		function construct_block(variable) result(output)
			type(Block) :: output
			real :: variable
			output%data = variable
			output%grad = 0.0
		end function construct_block


		subroutine addition_definition(this, first_o, second_o)
			class(Block) :: this
			type(Block) :: first_o, second_o
			this%data = first_o%data + second_o%data
		end subroutine

		subroutine pass_data(this, data_in)
			class(Block) :: this
			real :: data_in
			this%data = data_in
		end subroutine pass_data

		subroutine pass_gradient(this, grad)
			class(Block) :: this
			real :: grad
			this%grad = grad
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
			character(len=3) :: operation
			allocate(output%this_ptr)
			allocate(output%other_ptr)
			allocate(output%result_ptr)
			output%this_ptr => this
			output%other_ptr => other
			output%result_ptr => result
			output%operation = operation
		end function

		! function construct_reference() result(output)
		! 	type(reference) :: output
		! 	type(Block), target :: this, other, result
		! 	type(Block), pointer :: this_ptr, other_ptr, result_ptr
		! 	character(len=3) :: operation
		! 	operation = "add"
		! 	this = construct_block(0.0)
		! 	other = construct_block(0.0)
		! 	result = construct_block(0.0)
		! 	allocate(this_ptr)
		! 	allocate(other_ptr)
		! 	allocate(result_ptr)
		! 	allocate(output%this_ptr)
		! 	allocate(output%other_ptr)
		! 	allocate(output%result_ptr)
		! 	this_ptr=>this
		! 	other_ptr=>other
		! 	result_ptr=>result
		! 	output%this_ptr = this_ptr
		! 	output%other_ptr = other_ptr
		! 	output%result_ptr = result_ptr
		! 	output%operation = operation
		! end function

		! subroutine pass_data_init(this_object, this, other, result, operation)
		! 	class(reference) :: this_object
		! 	type(Block), target :: this, other, result
		! 	type(Block), pointer :: this_ptr, other_ptr, result_ptr
		! 	character(len=3) :: operation
		! 	allocate(this_ptr)
		! 	allocate(other_ptr)
		! 	allocate(result_ptr)
		! 	allocate(this_object%this_ptr)
		! 	allocate(this_object%other_ptr)
		! 	allocate(this_object%result_ptr)
		! 	this_ptr=>this
		! 	other_ptr=>other
		! 	result_ptr=>result
		! 	this_object%this_ptr = this_ptr
		! 	this_object%other_ptr = other_ptr
		! 	this_object%result_ptr = result_ptr
		! 	this_object%operation = operation
		! end subroutine

end module reference_mod



module  FTL !we need a queue for the differentiation grpah
	use reference_mod
	implicit none

	type, public :: queue
		type(reference), dimension(:), allocatable :: list
		contains
			procedure, pass(this) :: append => append_definition
			procedure, pass(this) :: print => print_definition
			procedure, pass(this) :: backward => backward_definition
	end type queue


	contains
		subroutine append_definition(this, first, second, result, operation)
			class(queue) :: this
			type(reference), dimension(:), allocatable :: queue_cpy
			type(reference) :: item
			type(Block) :: first, second, result
			character(len=3) :: operation


			item = construct_reference(first, second, result, operation) 
			item%this_ptr%data = 1000.0
			print *, item%this_ptr, first
			if (.not. allocated(this%list)) then ! if the Q is empty we create it
				allocate(this%list(0)) ! THE PROBLEM IS HERE!!!
			end if

			
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
			! print *, n
			do i=1, n
			print *,"_____value_____", "______grad________", "________value______", "_______grad_____","________value_____", "________grad__"
			print *, this%list(i)%this_ptr, this%list(i)%operation, this%list(i)%other_ptr, "=" ,this%list(i)%result_ptr
			end do
			print *, ''
		end subroutine print_definition

		subroutine backward_definition(this) 
			class(queue) :: this
			integer :: n, i, index
			n = size(this%list)
			!the last one has the gradient of one because is equal to dy/dy
			call this%list(n)%result_ptr%pass_grad(1.0)
			do i=1, n
				index = n-i+1
				print *, index

				call this%list(index)%this_ptr%pass_grad(this%list(index)%this_ptr%grad + this%list(index)%result_ptr%grad)
				call this%list(index)%other_ptr%pass_grad(this%list(index)%other_ptr%grad + this%list(index)%result_ptr%grad)
			end do
		end subroutine

end module FTL



program main
	use FTL
	use Block_mod
	implicit none
	type(Block) :: a, b, c, d, e 
	type(queue) :: graf
	!apparently problem is that i m using the reference type for array elements

	a = construct_block(0.0)
	b = construct_block(0.0)
	c = construct_block(0.0)
	d = construct_block(0.0)
	e = construct_block(0.0)

	call a%pass(3.0)
	call b%pass(4.0)
	call e%pass(3.0)
	call c%add(a, b)
	call d%add(c, e)
	call graf%append(a, b, c, 'add')
	call graf%append(c, e, d, 'add')
	print *, a ,b, c, e
	call graf%backward()
	call graf%print()
end program main
