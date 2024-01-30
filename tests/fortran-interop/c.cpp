/***
 * Not Needed for the moment since we can make the calls directly from Fortran
***/
// #include <stdio.h>
// #include <memory>
// #include <scheduler.h>
// #include <complex.h>
// static TaskHandle EmptyTaskHandle{-1};
// static ThreadPool* pool;

// extern "C"{

// TaskHandle
// call_it (TaskHandle (*func)(TaskHandle), TaskHandle arg)
// {
//   printf("Hi from c: %d\n", arg.task_id);
//   return func (arg);
// }
// void
// hi_func()
// {
//   printf("Hi from hi func\n");
// }
// void
// hi_from_c()
// {
//   printf("Hi from c\n");
// }
// void 
// run(void (*calc_func)(const int a, const int b, const int c, const int d, int* arr), void (*reduce_func)(void), void(*clean_func)(void), int* array)
// {
//   int* my_array = array;
//   for(int i=0;i<15;i++)
//     my_array[i]=i+1;
//   printf("RUNNING \n");
//   printf("Arr values:\n");
//   for(int i=0;i<15;i++)
//     printf("%d,",array[i]);
//   printf("\n");
// 	const auto processor_count = std::thread::hardware_concurrency();
// 	printf("processor count: %d\n",processor_count);
//   for(int i=0;i<1;++i)
//   {
//     TaskHandle task_handle = pool->Push((TwoDimensionalFunc){convert(calc_func, my_array), {0,15}, {0,15}},3,1,{},Default); 
//     TaskHandle reduce_handle = pool->Push(reduce_func,3,1,{task_handle},Critical,Single);
//     TaskHandle hi_handle = pool->Push(hi_func, 3,0);
//     pool->Push(clean_func,1,1,{reduce_handle},Critical, All);
//   }
//   // free(my_array);
//   // pool.ReLaunchAll();
//   // pool.WaitAll();
//   // free(array);
//   //deallocate_func(array);
// }
}
