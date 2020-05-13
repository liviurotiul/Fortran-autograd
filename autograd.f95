module Autograd
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

end module Autograd

module  FTL !we need a queue for the differentiation grpah
	use Autograd
	implicit none
	! the Q contains references to objects taking part in certain operations
	type, public :: reference
		type(Block), pointer :: this_ptr, other_ptr
		character(len = 20) :: operation
	end type reference

	type, public :: queue
		type(reference), dimension(:), allocatable :: list
		contains
			procedure, pass(this) :: append => append_definition
			procedure, pass(this) :: print => print_definition
	end type queue


	contains
		function construct_reference(this_ptr, other_ptr, operation) result(output)
			type(reference) :: output
			type(Block), pointer :: this_ptr, other_ptr
			character(len = 20) :: operation
			output%this_ptr = this_ptr
			output%other_ptr = other_ptr
			output%operation = operation
		end function

		subroutine append_definition(this, item)
			class(queue) :: this
			type(reference), allocatable, dimension(:) :: new
			type(reference) :: item
			if (.not. allocated(this%list)) then
				allocate(this%list(0))
			end if
			allocate(new(size(this%list)+1))
			new(1:size(this%list)) = this%list
			new(size(this%list)+1) = item
			deallocate(this%list)
			allocate(this%list(size(new)))
			this%list = new
		end subroutine append_definition

		subroutine print_definition(this)
			class(queue) :: this
			print *, "this%list"
		end subroutine print_definition
end module FTL

program main
	use FTL
	use Autograd
	implicit none
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
