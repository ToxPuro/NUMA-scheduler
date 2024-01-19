#include <stdio.h>
#include <memory>
#include <scheduler.h>
static TaskHandle EmptyTaskHandle{-1};
static ThreadPool* pool;
std::function<void(const int a, const int b)>
convert(std::function<void(const int a, const int b, int* arg)> lambda, int* arg)
{
  return [=](const int x, const int y)
  {
    lambda(x,y,arg);
  };
}
template <typename T>
std::function<void(const int a, const int b, const int c, const int d)>
convert(std::function<void(const int a, const int b, const int c, const int d, int* arg)> lambda, T arg)
{
  return [=](const int x, const int y, const int z, const int w)
  {
    lambda(x,y,z,w,arg);
  };
}
template <typename T>
std::function<void(const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end)>
convert(std::function<void(const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end, T arg)> lambda, T arg)
{
  return [=](const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end)
  {
    lambda(x_start,x_end,y_start,y_end,z_start,z_end,arg);
  };
}
template <typename T>
std::function<void(void)>
convert(std::function<void(T x, T y)> lambda, T x, T y)
{
  return[=]()
  {
    lambda(x,y);
  };
}
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
void make_threadpool(const int num_threads)
{
  pool = new ThreadPool(num_threads);
}
void
wait_all_thread_pool()
{
  pool->WaitAll();
}
void
free_thread_pool()
{
  pool->StopProcessing();
  free(pool);
}
void
hi_from_c()
{
  printf("Hi from c\n");
}
TaskHandle
push_void_func(void (*func)(void), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int)
{
  TaskType type = static_cast<TaskType>(task_type);
  DependencyType dependency_type = static_cast<DependencyType>(dependency_int);
  if(prerequisite == EmptyTaskHandle)
    return pool->Push(func,num_of_subtasks,priority, {}, type);
  return pool->Push(func, num_of_subtasks, priority, {prerequisite}, type,dependency_type);
}
TaskHandle
push_1d_func_with_arr_int(void (*func)(const int start, const int end, int* arr), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, const int start, const int end, int* array, const int n)
{
  TaskType type = static_cast<TaskType>(task_type);
  DependencyType dependency_type = static_cast<DependencyType>(dependency_int);
  SingleDimensionalFunc lambda = {convert(func, array), {static_cast<size_t>(start), static_cast<size_t>(end)}};
  if(prerequisite == EmptyTaskHandle)
    return pool->Push(lambda,num_of_subtasks,priority, {}, type);
  return pool->Push(lambda, num_of_subtasks, priority, {prerequisite}, type,dependency_type);
}
TaskHandle
push_2d_func_with_arr_int(void (*func)(const int x_start, const int x_end, const int y_start, const int y_end, int* arr), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, const int x_start, const int x_end, const int y_start, const int y_end, int* array, const int x_length, const int y_length)
{
  TaskType type = static_cast<TaskType>(task_type);
  DependencyType dependency_type = static_cast<DependencyType>(dependency_int);
  TwoDimensionalFunc lambda = {convert(func, array), {static_cast<size_t>(x_start), static_cast<size_t>(x_end)}, {static_cast<size_t>(x_start), static_cast<size_t>(x_end)}};
  if(prerequisite == EmptyTaskHandle)
    return pool->Push(lambda,num_of_subtasks,priority, {}, type);
  return pool->Push(lambda, num_of_subtasks, priority, {prerequisite}, type,dependency_type);
}
void run(void (*calc_func)(const int a, const int b, const int c, const int d, int* arr), void (*reduce_func)(void), void(*clean_func)(void), int* array)
{
  int* my_array = array;
  for(int i=0;i<15;i++)
    my_array[i]=i+1;
  printf("RUNNING \n");
  printf("Arr values:\n");
  for(int i=0;i<15;i++)
    printf("%d,",array[i]);
  printf("\n");
	const auto processor_count = std::thread::hardware_concurrency();
	printf("processor count: %d\n",processor_count);
  for(int i=0;i<1;++i)
  {
    TaskHandle task_handle = pool->Push((TwoDimensionalFunc){convert(calc_func, my_array), {0,15}, {0,15}},3,1,{},Default); 
    TaskHandle reduce_handle = pool->Push(reduce_func,3,1,{task_handle},Critical,Single);
    TaskHandle hi_handle = pool->Push(hi_func, 3,0);
    pool->Push(clean_func,1,1,{reduce_handle},Critical, All);
  }
  // free(my_array);
  // pool.ReLaunchAll();
  // pool.WaitAll();
  // free(array);
  //deallocate_func(array);
}
}
