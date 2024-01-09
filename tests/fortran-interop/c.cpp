#include <stdio.h>
#include <memory>
#include <scheduler.h>
extern "C"{

TaskHandle
call_it (TaskHandle (*func)(TaskHandle), TaskHandle arg)
{
  printf("Hi from c: %d\n", arg.task_id);
  return func (arg);
}
void run(void (*func)(const int a, const int b))
{
  printf("RUNNING \n");
	const auto processor_count = std::thread::hardware_concurrency();
  ThreadPool pool(processor_count-1);
	printf("processor count: %d\n",processor_count);
  for(int i=0;i<1;++i)
  {
    TaskHandle task_handle = pool.Push(func,3,0,10,1,{},Critical); 
  }
  pool.WaitAll();
  pool.StopProcessing();
}
}
