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
void
hi_func()
{
  printf("Hi from hi func\n");
}
std::function<void(const int a, const int b)>
convert(std::function<void(const int a, const int b, int* arr)> lambda, int* array)
{
  return [=](const int x, const int y)
  {
    lambda(x,y,array);
  };
}
void run(void (*calc_func)(const int a, const int b, int* arr), void (*reduce_func)(void), int* array)
{
  printf("RUNNING \n");
  printf("Arr values:\n");
  for(int i=0;i<15;i++)
    printf("%d,",array[i]);
  printf("\n");
	const auto processor_count = std::thread::hardware_concurrency();
  ThreadPool pool(processor_count-1);
	printf("processor count: %d\n",processor_count);
  for(int i=0;i<1;++i)
  {
    TaskHandle task_handle = pool.Push(convert(calc_func, array),3,0,15,1,{},Default); 
    pool.Push(reduce_func,3,0,10,1,{task_handle},Critical,Single);
    pool.Push(hi_func, 3, 0,1,0);
  }
  pool.WaitAll();
  pool.ReLaunchAll();
  pool.WaitAll();
  pool.StopProcessing();
}
}
