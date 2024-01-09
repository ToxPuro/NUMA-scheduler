#include <stdio.h>
typedef struct
{
  const int task_id;
} TaskHandle;

TaskHandle
call_it (TaskHandle (*func)(TaskHandle), TaskHandle arg)
{
  printf("Hi from c: %d\n", arg.task_id);
  return func (arg);
}
