module Autograd
	implicit none

	type, public :: Block
		real :: data, grad
		contains
			procedure, pass(this) :: construct => constructor_definition
			procedure, pass(this) :: add => addition_definition
	end type Block

	contains

		subroutine constructor_definition(this, data)
			class(Block) :: this
			real :: data, other
			this%data = data
			this%grad = 0
		end subroutine constructor_definition

		function addition_definition(this, other) result(output)
			class(Block), intent(in) :: this, other
			type(Block) :: output
			call output%construct(this%data + other%data)
		end function addition_definition

end module Autograd



program main
	use Autograd
	implicit none
	type(Block) :: A, B, C
	call A%construct(3.0)
	call B%construct(4.0)
	C = A%add(B)
	C = C%add(B)
	print *, C%data
end program main
