#include <scheduler.h>
#include <memory>
void
print_difference(const int a, const int b)
{
  printf("a,b\t%d,%d\n",a,b);
}
int main()
{
	const auto processor_count = std::thread::hardware_concurrency();
  ThreadPool pool(processor_count-1);
	printf("processor count: %d\n",processor_count);
  for(int i=0;i<1;++i)
  {
    TaskHandle task_handle = pool.Push((SingleDimensionalFunc){print_difference,{0,10}},3,1,{},Async);
  }
  pool.WaitAll();
  pool.StopProcessing();
  return 0;
}
