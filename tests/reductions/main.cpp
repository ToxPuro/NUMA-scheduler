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
  std::vector<TaskHandle> handles;
  for(int i=0;i<1;++i)
  {
    TaskHandle task_handle = pool.Push(print_difference,3,0,10,1,{},Async);
    handles.push_back(task_handle);
  }
  pool.WaitAll();
  pool.StopProcessing();
  return 0;
}
