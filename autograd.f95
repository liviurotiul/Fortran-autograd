module Block_mod
	implicit none
	type, public :: Block
		real :: data, grad
		contains
			procedure, pass(this) :: pass_block_data => pass_block_data_definition
			procedure, pass(this) :: pass_block_grad => pass_block_grad_definition
			procedure, pass(this) :: subtract => substraction_definition
			procedure, pass(this) :: add => addition_definition
			procedure, pass(this) :: multiply => multiply_definition
			procedure, pass(this) :: divide => divide_definition
			end type Block
	contains
		! constructor__________________________________________
		function construct_block(variable) result(output)
			type(Block) :: output
			real :: variable
			output%data = variable
			output%grad = 0.0
		end function construct_block
		
		! opperations __________________________________________
		subroutine addition_definition(this, operand1, operand2)
			class(Block) :: this
			type(Block) :: operand1, operand2
			this%data = operand1%data + operand2%data
		end subroutine

		subroutine substraction_definition(this, operand1, operand2)
			class(Block) :: this
			type(Block) :: operand1, operand2
			this%data = operand1%data - operand2%data
		end subroutine

		subroutine multiply_definition(this, operand1, operand2)
			class(Block) :: this
			type(Block) :: operand1, operand2
			this%data = operand1%data * operand2%data
		end subroutine

		subroutine divide_definition(this, operand1, operand2)
			class(Block) :: this
			type(Block) :: operand1, operand2
			this%data = operand1%data / operand2%data
		end subroutine

		subroutine pass_block_data_definition(this, data_in)
			class(Block) :: this
			real :: data_in
			this%data = data_in
		end subroutine pass_block_data_definition

		

		subroutine pass_block_grad_definition(this, grad)
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
		type(Block), pointer :: operand1_ptr, operand2_ptr, result_ptr
		character(len=3) :: operation
	end type reference
	contains
		function construct_reference(operand1, operand2, result, operation) result(output)
			type(reference) :: output
			type(Block), target :: operand1, operand2, result
			character(len=3) :: operation
			allocate(output%operand1_ptr)
			allocate(output%operand2_ptr)
			allocate(output%result_ptr)
			output%operand1_ptr => operand1
			output%operand2_ptr => operand2
			output%result_ptr => result
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
			procedure, pass(this) :: backward => backward_definition
	end type queue


	contains
		subroutine append_definition(this, operand1, operand2, result, operation)
			class(queue) :: this
			type(reference), dimension(:), allocatable :: queue_cpy
			type(reference) :: item
			type(Block) :: operand1, operand2, result
			character(len=3) :: operation


			item = construct_reference(operand1, operand2, result, operation) 

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
			print *, this%list(i)%operand1_ptr, this%list(i)%operation, this%list(i)%operand2_ptr, "=" ,this%list(i)%result_ptr
			end do
			print *, ''
		end subroutine print_definition

		subroutine backward_definition(this) 
			class(queue) :: this
			integer :: n, i, index
			n = size(this%list)
			!the last one has the gradient of one because is equal to dy/dy
			call this%list(n)%result_ptr%pass_block_grad(1.0)
			do i=1, n
				index = n-i+1
				print *, index
				if (this%list(index)%operation == 'add') then
					call this%list(index)%operand1_ptr%pass_block_grad(this%list(index)%operand1_ptr%grad + this%list(index)%result_ptr%grad)
					call this%list(index)%operand2_ptr%pass_block_grad(this%list(index)%operand2_ptr%grad + this%list(index)%result_ptr%grad)

				else if (this%list(index)%operation == 'sub') then
					call this%list(index)%operand1_ptr%pass_block_grad(this%list(index)%operand1_ptr%grad + this%list(index)%result_ptr%grad)
					call this%list(index)%operand2_ptr%pass_block_grad(this%list(index)%operand2_ptr%grad - this%list(index)%result_ptr%grad)

				else if (this%list(index)%operation == 'mul') then
					call this%list(index)%operand1_ptr%pass_block_grad(this%list(index)%operand1_ptr%grad + &
																		(this%list(index)%result_ptr%grad * &
																		this%list(index)%operand2_ptr%data))

					call this%list(index)%operand2_ptr%pass_block_grad(this%list(index)%operand2_ptr%grad + &
																		(this%list(index)%result_ptr%grad * &
																		this%list(index)%operand1_ptr%data))

				else if (this%list(index)%operation == 'div') then
					call this%list(index)%operand1_ptr%pass_block_grad(this%list(index)%operand1_ptr%grad + &
																		this%list(index)%result_ptr%grad / &
																		this%list(index)%operand2_ptr%data )

					call this%list(index)%operand2_ptr%pass_block_grad(this%list(index)%operand2_ptr%grad + &
																		(this%list(index)%result_ptr%grad / &
																		(-1)*this%list(index)%operand1_ptr%data))
				end if
			end do
		end subroutine

end module FTL



program main
	use FTL
	use Block_mod
	implicit none
	type(Block) :: a, b, c, d, e, f, g
	type(queue) :: graf


	a = construct_block(0.0)
	b = construct_block(0.0)
	c = construct_block(0.0)
	d = construct_block(0.0)
	e = construct_block(0.0)
	f = construct_block(0.0)
	g = construct_block(0.0)

	call a%pass_block_data(3.0)
	call b%pass_block_data(4.0)
	call c%pass_block_data(5.0)
	call d%pass_block_data(6.0)

	call e%multiply(a, b)
	call f%divide(c, d)
	call g%add(e, f)

	call graf%append(a, b, e, 'mul')
	call graf%append(c, d, f, 'div')
	call graf%append(e, f, g, 'add')

	call graf%backward()
	call graf%print()
end program main
