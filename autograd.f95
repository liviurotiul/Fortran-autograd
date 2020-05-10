module Autograd
	implicit none

	type, public :: Block
		real :: data, grad
		contains
			procedure, pass(this) :: construct_ => constructor_definition !in place constructor
			procedure, pass(this) :: add => addition_definition
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
			class(Block), intent(in) :: this, other
			type(Block) :: output
			output = construct(this%data + other%data)
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
