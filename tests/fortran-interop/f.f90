MODULE m

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  integer, dimension(15) :: test_arr=1
  integer, target :: reduce_res
  integer, pointer :: p_reduce_res
  !$omp threadprivate(reduce_res)
TYPE, BIND(C) :: TaskHandle
INTEGER(C_INT) :: task_id
END TYPE TaskHandle

  ! Define interface of C function.
  INTERFACE
    INTEGER(KIND=C_INT) FUNCTION call_it (func, arg) BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING
      import TaskHandle
      TYPE(C_FUNPTR), INTENT(IN), VALUE :: func
      Type(TaskHandle), INTENT(IN), VALUE :: arg
    END FUNCTION call_it
  END INTERFACE
  INTERFACE
    SUBROUTINE run(func) BIND(C)
    use, INTRINSIC :: iso_c_binding
    type(c_funptr), intent(in), value :: func
    END SUBROUTINE run
  END INTERFACE

CONTAINS

  ! Define procedure passed to C function.
  ! It must be interoperable!
  type(TaskHandle) FUNCTION double_it (arg) BIND(C)
          type(TaskHandle), INTENT(IN), VALUE :: arg
    double_it%task_id = 2*arg%task_id
  END FUNCTION double_it
  subroutine print_x(x) 
    use omp_lib
    integer :: x
    print*,"X is: ",x
    reduce_res = reduce_res + x
    print*,"reduce_res is", reduce_res
  endsubroutine print_x
  subroutine reduce_intermediates()
    
  endsubroutine reduce_intermediates
  subroutine hello_ints(x,y) BIND(C)
    use ISO_C_BINDING, only : c_int
    !integer(kind=c_int) :: x,y
    integer, value :: x,y
    call print_x(x)
    call print_x(y)
    p_reduce_res = p_reduce_res + reduce_res
  end subroutine hello_ints

  ! Call C function.
  SUBROUTINE foobar ()
    use omp_lib
    type(TaskHandle) :: task_handle
    INTEGER(KIND=C_INT) :: i


    ! Use it.
    DO i = 1_C_INT, 10_C_INT
      task_handle%task_id = i
      PRINT *, call_it (c_funloc(double_it), task_handle)

    END DO
    p_reduce_res => reduce_res
    call run(c_funloc(hello_ints))
    print*,"reduce res = ",reduce_res
  END SUBROUTINE foobar

END MODULE m
program main
        use m
        call foobar()
endprogram
