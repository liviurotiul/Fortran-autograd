module Autograd
	implicit none


	! i need this type to store the children 
	type, public :: block_ptr
		type(Block), pointer :: p
	end type block_ptr

	abstract interface
		subroutine generic_back_subr(this_ptr, other_ptr)
			type(Block), pointer :: this_ptr, other_ptr
		end subroutine
	end interface

	type, public :: Block
		real :: data, grad
		type(block_ptr), dimension(:), allocatable :: children
		procedure(generic_back_subr), pointer :: backward_
		contains
			procedure, pass(this) :: construct_ => constructor_definition !in place constructor
			procedure, pass(this) :: add => addition_definition
			! procedure, pass(this) :: backward_ => backward_definition
	end type Block


	contains

		function construct(data) result(output)
			type(Block) :: output
			real :: data
			output%data = data
			output%grad = 0
		end function construct

		subroutine constructor_definition(this, data)
			class(Block) :: this
			real :: data
			this%data = data
			this%grad = 0
		end subroutine constructor_definition

		function addition_definition(this, other) result(output)
			type(Block), intent(in) :: this
			type(Block) :: output, other
			output = construct(this%data + other%data)
			! contains
			! 	subroutine backward_()
			! 		this%grad = this%grad + output%grad
			! 		other%grad = other%grad + output%grad
			! 	end subroutine
			! output%backward_ => backward_

		end function addition_definition

end module Autograd



program main
	use Autograd
	implicit none
	type(Block) :: A, B, C
	A = construct(30.0)
	B = construct(40.0)
	C = A%add(B)
	print *, C%data
end program main
