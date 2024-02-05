module mt
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

TYPE, BIND(C) :: TaskHandle
INTEGER(C_INT) :: task_id
END TYPE TaskHandle
type(TaskHandle) :: empty_handle = TaskHandle(task_id=-1)
integer, parameter :: default_task_type=0, async_task_type=1, critical_task_type=2
integer, parameter :: depend_on_all=0, depend_on_single=1
interface push_task
        module procedure push_void_func_wrapper
        module procedure push_2d_func_with_arr_int_wrapper
        module procedure push_4d_array_task_wrapper_single
        module procedure push_4d_array_task_wrapper_double
end interface
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
  interface
      type(TaskHandle) function push_4d_array_task_single&
      (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int,&
      array,x_length, y_length, z_length, w_length) BIND(C)
        use, INTRINSIC :: iso_c_binding
        import TaskHandle
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: x_length, y_length, z_length, w_length
        real(4), dimension(x_length, y_length, z_length, w_length) :: array
      end function
  end interface
  interface
      type(TaskHandle) function push_4d_array_task_double&
      (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int,&
      array,x_length, y_length, z_length, w_length) BIND(C)
        use, INTRINSIC :: iso_c_binding
        import TaskHandle
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: x_length, y_length, z_length, w_length
        real(8), dimension(x_length, y_length, z_length, w_length) :: array
      end function
  end interface


contains
        type(TaskHandle) function push_void_func_wrapper(func, prerequisite, num_of_subtasks, task_type, priority, dependency_int)
        use, INTRINSIC :: iso_c_binding
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int

        push_void_func_wrapper = push_void_func(func, prerequisite, num_of_subtasks, task_type, priority, dependency_int)
        endfunction

      type(TaskHandle) function push_2d_func_with_arr_int_wrapper&
       (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int&
       ,x_start,x_end,y_start,y_end,array,x_length, y_length) 
        use, INTRINSIC :: iso_c_binding
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: x_start,x_end, y_start, y_end
        integer, value :: x_length, y_length 
        integer, dimension(x_length, y_length) :: array
        push_2d_func_with_arr_int_wrapper=push_2d_func_with_arr_int (func, prerequisite, num_of_subtasks, &
                task_type, priority, dependency_int,x_start,x_end,y_start,y_end,array,x_length, y_length)
      end function

      type(TaskHandle) function push_4d_array_task_wrapper_single&
      (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int,&
      array,x_length, y_length, z_length, w_length) BIND(C)
        use, INTRINSIC :: iso_c_binding
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: x_length, y_length, z_length, w_length
        real, dimension(x_length, y_length, z_length, w_length) :: array
        push_4d_array_task_wrapper_single = push_4d_array_task_single(func, prerequisite,&
               num_of_subtasks, task_type, priority,dependency_int,&
               array,x_length, y_length, z_length, w_length)
      end function

      type(TaskHandle) function push_4d_array_task_wrapper_double&
      (func, prerequisite, num_of_subtasks, task_type, priority, dependency_int,&
      array,x_length, y_length, z_length, w_length) BIND(C)
        use, INTRINSIC :: iso_c_binding
        type(c_funptr), value :: func
        type(TaskHandle), value :: prerequisite
        integer, value :: num_of_subtasks
        integer, value :: task_type
        integer, value :: priority
        integer, value :: dependency_int
        integer, value :: x_length, y_length, z_length, w_length
        real(8), dimension(x_length, y_length, z_length, w_length) :: array
        push_4d_array_task_wrapper_double = &
        push_4d_array_task_double(func, prerequisite, &
        num_of_subtasks, task_type, priority,dependency_int,&
        array,x_length, y_length, z_length, w_length)
      end function
endmodule
