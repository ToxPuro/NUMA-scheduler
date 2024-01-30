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
  type(TaskHandle) :: empty_handle = TaskHandle(task_id=-1)
  integer, parameter :: default_task_type=0, async_task_type=1, critical_task_type=2
  integer, parameter :: depend_on_all=0, depend_on_single=1

  ! Define interface of C function.
  interface
      subroutine make_threadpool(num_threads) BIND(C)
        integer, value :: num_threads
      endsubroutine make_threadpool
  end interface
  interface
      subroutine wait_all_thread_pool() BIND(C)
      endsubroutine wait_all_thread_pool
  end interface
  interface
      subroutine free_thread_pool() BIND(C)
      endsubroutine free_thread_pool 
  end interface
  interface
      type(TaskHandle) function &
      push_void_func(func, prerequisite, num_of_subtasks, task_type, priority, dependency_int) BIND(C)
        use, INTRINSIC :: iso_c_binding
        import TaskHandle
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
      end function
  end interface
  interface
    subroutine hi_from_c() BIND(C)
    endsubroutine
  end interface
  interface
      type(TaskHandle) function push_1d_func_with_arr_int &
      (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int,start, end,array, n) BIND(C)
        use, INTRINSIC :: iso_c_binding
        import TaskHandle
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: start,end 
        integer, value :: n
        integer, dimension(n) :: array
      end function
  end interface
  interface
      type(TaskHandle) function push_2d_func_with_arr_int &
      (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int,&
      x_start,x_end,y_start,y_end,array,x_length, y_length) BIND(C)
        use, INTRINSIC :: iso_c_binding
        import TaskHandle
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: x_start,x_end, y_start, y_end
        integer, value :: x_length, y_length 
        integer, dimension(x_length, y_length) :: array
      end function
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
    use omp_lib
    integer, dimension(15,15) :: array
    integer, value :: start,end,start_y, end_y
    integer :: x_index, y_index
    !if(start == 1) then
    !  call sleep(10)
    !endif
    !$omp parallel num_threads(3)
    print*,"thread num= ",omp_get_thread_num()
    !$omp do
    do x_index=start,end
      do y_index = start_y, end_y
        reduce_res = reduce_res + array(x_index,y_index)
      enddo
    enddo
    !$omp end do
    !$omp critical
    p_reduce_res = p_reduce_res + reduce_res
    !$omp end critical
    !$omp end parallel
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
  subroutine hello_func() BIND(C)
    print*,"Hi from Fortran"
  endsubroutine

  SUBROUTINE foobar ()
    use omp_lib
    type(TaskHandle) :: task_handle
    INTEGER(KIND=C_INT) :: i
    integer, parameter :: num_threads=1

    ! Use it.
    p_reduce_res => reduce_res
    call init_func()
    call make_threadpool(num_threads)
    !call run(c_funloc(hello_ints), c_funloc(reduce_intermediates), c_funloc(clean_func), test_arr, 15)
    !task_handle = push_void_func(c_funloc(hello_func), empty_handle, 3, default_task_type, 1, depend_on_all)
    task_handle = push_2d_func_with_arr_int(c_funloc(hello_ints), empty_handle, num_threads, &
                  default_task_type, 1, depend_on_all, 0, 15, 0, 15, test_arr, 15, 15)
    !task_handle = push_void_func(c_funloc(reduce_intermediates), task_handle, num_threads, critical_task_type, 1, depend_on_single)
    task_handle = push_void_func(c_funloc(clean_func), task_handle, 1, critical_task_type, 1, depend_on_all)
    call wait_all_thread_pool()
    print*,"reduce res = ",reduce_res
    print*,allocated(test_arr)
    !deallocate(test_arr)
    !print*,test_arr
    call free_thread_pool()
  END SUBROUTINE foobar

END MODULE m
program main
        use m
        call foobar()
endprogram
