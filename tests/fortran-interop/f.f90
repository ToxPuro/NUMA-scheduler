MODULE m

  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

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

CONTAINS

  ! Define procedure passed to C function.
  ! It must be interoperable!
  type(TaskHandle) FUNCTION double_it (arg) BIND(C)
          type(TaskHandle), INTENT(IN), VALUE :: arg
    double_it%task_id = 2*arg%task_id
  END FUNCTION double_it

  ! Call C function.
  SUBROUTINE foobar ()
    type(TaskHandle) :: task_handle
    INTEGER(KIND=C_INT) :: i


    ! Use it.
    DO i = 1_C_INT, 10_C_INT
      task_handle%task_id = i
      PRINT *, call_it (c_funloc(double_it), task_handle)
    END DO
  END SUBROUTINE foobar

END MODULE m
program main
        use m
        call foobar()
endprogram
