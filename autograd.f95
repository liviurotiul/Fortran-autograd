module  FTL
	implicit none
	type, public :: queue
		real, dimension(:), allocatable :: list
		contains
			procedure, pass(this) :: append => append_definition
	end type queue
	contains
		subroutine append_definition(this, data)
			class(queue) :: this
			real, allocatable, dimension(:) :: new
			real :: data
			allocate(new(size(this%list) + 1))
			new(1:size(this%list)) = this%list(1:size(this%list))
			this%list = new
			this%list(size(this%list)+1) = data
		end subroutine append_definition
end module FTL





! module Autograd
! 	implicit none
! 	type, public :: Block
! 		real :: data, grad
! 		contains
! 			procedure, pass(this) :: construct_ => constructor_definition !in place constructor
! 			procedure, pass(this) :: add => addition_definition
! 	end type Block

! 	abstract interface
! 		subroutine generic_back_subr(this_ptr, other_ptr)
! 			class(Block), pointer :: this_ptr, other_ptr
! 		end subroutine
! 	end interface
	

! 	contains

! 		function construct(data) result(output)
! 			type(Block) :: output
! 			real :: data
! 			output%data = data
! 			output%grad = 0
! 		end function construct

! 		subroutine constructor_definition(this, data)
! 			class(Block) :: this
! 			real :: data
! 			this%data = data
! 			this%grad = 0
! 		end subroutine constructor_definition

! 		function addition_definition(this, other) result(output)
! 			type(Block), intent(in) :: this
! 			type(Block) :: output, other
! 			output = construct(this%data + other%data)
! 			! contains
! 			! 	subroutine backward_()
! 			! 		this%grad = this%grad + output%grad
! 			! 		other%grad = other%grad + output%grad
! 			! 	end subroutine
! 			! output%backward_ => backward_

! 		end function addition_definition

! end module Autograd



program main
	use FTL
	implicit none
	type(queue) :: q
	call q%append(3.0)
	! type(Block) :: A, B, C
	! A = construct(30.0)
	! B = construct(40.0)
	! C = A%add(B)
	! print *, C%data
end program main
