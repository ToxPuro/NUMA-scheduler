MODULE m

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  integer, target :: reduce_res
  integer, pointer :: p_reduce_res
  !$omp threadprivate(reduce_res)
  integer, allocatable, target, dimension(:,:) :: test_arr
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
  interface
      subroutine make_threadpool(num_threads) BIND(C)
        integer, value :: num_threads
      endsubroutine make_threadpool
  end interface
  INTERFACE
    SUBROUTINE run(calc_func, reduce_func, clean_func, array, n) BIND(C)
    use, INTRINSIC :: iso_c_binding
    type(c_funptr), intent(in), value :: calc_func, reduce_func, clean_func
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
  subroutine hello_ints(start,end, start_y, end_y, array) BIND(C)
    use ISO_C_BINDING, only : c_int
    integer, dimension(15,15) :: array
    integer, value :: start,end,start_y, end_y
    !if(start == 1) then
    !  call sleep(10)
    !endif
    print*,"x range: ",start,"-",end
    print*,"arr: ",array
    print*,"y range:",start_y,"-",end_y
    reduce_res = reduce_res + sum(array(start:end,start_y:end_y))
  end subroutine hello_ints
  subroutine init_func() BIND(C)
    integer :: i,j
    allocate(test_arr(15,15))
    do i=1,15
      do j=1,15
        test_arr(i,j) = i+(j-1)*15
      enddo
    enddo
  endsubroutine init_func
  subroutine clean_func() BIND(C)
    deallocate(test_arr)
  endsubroutine clean_func

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
    call init_func()
    call make_threadpool(3)
    call run(c_funloc(hello_ints), c_funloc(reduce_intermediates), c_funloc(clean_func), test_arr, 15)
    print*,"reduce res = ",reduce_res
    print*,test_arr
    print*,allocated(test_arr)
    !deallocate(test_arr)
    print*,test_arr
    !print*,allocated(test_arr)
  END SUBROUTINE foobar

END MODULE m
program main
        use m
        call foobar()
endprogram
