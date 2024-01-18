MODULE m

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  integer, allocatable, dimension(:) :: test_arr
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
    SUBROUTINE run(calc_func, reduce_func, array, n) BIND(C)
    use, INTRINSIC :: iso_c_binding
    type(c_funptr), intent(in), value :: calc_func
    type (c_funptr), intent(in), value :: reduce_func
    integer, value :: n
    integer, dimension(n) :: array
    END SUBROUTINE run
  END INTERFACE

CONTAINS

  ! Define procedure passed to C function.
  ! It must be interoperable!
  type(TaskHandle) FUNCTION double_it (arg) BIND(C)
          type(TaskHandle), INTENT(IN), VALUE :: arg
    double_it%task_id = 2*arg%task_id
  END FUNCTION double_it
  subroutine reduce_intermediates() BIND(C)
    p_reduce_res = p_reduce_res + reduce_res
  endsubroutine reduce_intermediates
  subroutine hello_ints(start_c,end, array) BIND(C)
    use ISO_C_BINDING, only : c_int
    integer, dimension(15) :: array
    integer, value :: start_c,end
    integer :: start
    start = start_c+1
    !if(start == 1) then
    !  call sleep(10)
    !endif
    print*,"range: ",start,"-",end
    print*,"arr: ",array
    reduce_res = reduce_res + sum(test_arr(start:end))
  end subroutine hello_ints

  ! Call C function.
  SUBROUTINE foobar ()
    use omp_lib
    type(TaskHandle) :: task_handle
    INTEGER(KIND=C_INT) :: i
    allocate(test_arr(15))
    test_arr=1

    ! Use it.
    DO i = 1_C_INT, 10_C_INT
      task_handle%task_id = i
      PRINT *, call_it (c_funloc(double_it), task_handle)

    END DO
    p_reduce_res => reduce_res
    do i = 1,15
      test_arr(i) = i
    enddo
    call run(c_funloc(hello_ints), c_funloc(reduce_intermediates), test_arr, 15)
    print*,"reduce res = ",reduce_res
  END SUBROUTINE foobar

END MODULE m
program main
        use m
        call foobar()
endprogram
