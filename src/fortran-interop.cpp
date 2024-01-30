#include <stdio.h>
#include <memory>
#include <scheduler.h>
#include <complex.h>
static TaskHandle EmptyTaskHandle{-1};
static ThreadPool* pool;
typedef float NsReal;

//TODO: replace with correct dims
const int nx_start = 0;
const int ny_start = 0;
const int nz_start = 0;

const int nx_end = 1;
const int ny_end = 1;
const int nz_end = 1;

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
std::function<void()>
convert(std::function<void(NsReal* first, const int first_x, const int first_y, const int first_z, NsReal* second, const int second_x, const int second_y, const int second_z)> lambda, NsReal* first, const int first_x, const int first_y, const int first_z, NsReal* second, const int second_x, const int second_y, const int second_z)
{
  return[=]()
  {
    lambda(first, first_x, first_y, first_z, second, second_x, second_y, second_z);
  };
}

template <typename F>
TaskHandle
push_func(F func, TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int)
{
  TaskType type = static_cast<TaskType>(task_type);
  DependencyType dependency_type = static_cast<DependencyType>(dependency_int);
  if(prerequisite == EmptyTaskHandle)
    return pool->Push(func,num_of_subtasks,priority, {}, type);
  return pool->Push(func, num_of_subtasks, priority, {prerequisite}, type,dependency_type);
}
extern "C" {
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
TaskHandle
push_void_func(void (*func)(void), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int)
{
  return push_func(func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_1d_func_with_arr_int(void (*func)(const int start, const int end, int* arr), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, const int start, const int end, int* array, const int n)
{
  SingleDimensionalFunc lambda = {convert(func, array), {static_cast<size_t>(start), static_cast<size_t>(end)}};
  return push_func(lambda, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_2d_func_with_arr_int(void (*func)(const int x_start, const int x_end, const int y_start, const int y_end, int* arr), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, const int x_start, const int x_end, const int y_start, const int y_end, int* array, const int x_length, const int y_length)
{
  TwoDimensionalFunc lambda = {convert(func, array), {static_cast<size_t>(x_start), static_cast<size_t>(x_end)}, {static_cast<size_t>(x_start), static_cast<size_t>(x_end)}};
  return push_func(lambda, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_2d_3d_func_with_arrays_real(void (*func)(NsReal* first, const int first_x, const int first_y, const int first_z, NsReal* second, const int second_x, const int second_y, const int second_z), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, NsReal* first, const int first_x, const int first_y, const int first_z, NsReal* second, const int second_x, const int second_y, const int second_z)
{
  std::function<void()> lambda = convert(func, first, first_x, first_y, first_z, second, second_x, second_y, second_z);
  return push_func(lambda, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_yx_fourier(void (*transform)(const int y_start, const int y_end, const int x_start, const int x_end, NsReal* p_re, NsReal* p_im, const bool normalize, const bool inverse), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, const int y_range, const int x_range, NsReal* p_re, NsReal* p_im, const int p_x, const int p_y, const int p_z, const bool normalize, const bool inverse)
{
  std::function<void(const int y_start, const int y_end, const int x_start, const int x_end)> lambda
    = [=](const int y_start, const int y_end, const int x_start, const int x_end)
          {
            transform(y_start, y_end, x_start, x_end, p_re, p_im, normalize, inverse);
          };
  TwoDimensionalFunc res_func = {lambda, {0, static_cast<size_t>(y_range)}, {0, static_cast<size_t>(x_range)}};
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_3d_power_map_func(void (*map_func)(const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end, NsReal* dst, NsReal* src, const int src_index, const int vector_index), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, NsReal* dst, NsReal* src, const int src_index, const int vector_index) 
{
  std::function<void(const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end)> lambda
    = [=](const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end)
    {
      map_func(x_start, x_end, y_start, y_end, z_start, z_end, dst, src, src_index, vector_index);
    };
  ThreeDimensionalFunc res_func = {lambda, {nx_start,nx_end}, {ny_start,ny_end}, {nz_start,nz_end}};
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_powerscl_calc_data(void (*func)(NsReal* a_re, NsReal* a_im, NsReal* f, const int sp, const int iapn_index, const bool lsqrt), TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, NsReal* a_re, NsReal* a_im, NsReal* f, int sp, int iapn_index, bool lsqrt)
{
  std::function<void()> res_func = [=](){
    func(a_re,a_im,f,sp,iapn_index,lsqrt);
  };
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);

}
TaskHandle
push_powerscl_calc_spectra(void (*func)(NsReal* spectrum, NsReal* hor_spectrum, NsReal* ver_spectrum, NsReal* a_re, NsReal* a_im, int sp),TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, NsReal* spectrum, NsReal* hor_spectrum, NsReal* ver_spectrum, NsReal* a_re, NsReal* a_im, int sp)
{
  std::function<void()> res_func = [=]()
  {
    func(spectrum,hor_spectrum,ver_spectrum,a_re,a_im,sp);
  };
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_powerscl_output_results(void (*func)(NsReal* spectrum, NsReal* hor_spectrum, NsReal* ver_spectrum, NsReal* a_re, NsReal* a_im, const int sp),TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, NsReal* spectrum, NsReal* hor_spectrum, NsReal* ver_spectrum, NsReal* a_re, NsReal* a_im, const int sp)
{
  std::function<void()> res_func = [=]()
  {
    func(spectrum,hor_spectrum,ver_spectrum,a_re,a_im,sp);
  };
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_4d_array_task_single(void (*func)(float* array),TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, float* array, const int x_length, const int y_length, const int z_length, const int w_length)
{
  std::function<void()> res_func = [=]()
  {
    func(array);
  };
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
TaskHandle
push_4d_array_task_double(void (*func)(double* array),TaskHandle prerequisite, const int num_of_subtasks, const int task_type, const int priority, const int dependency_int, double* array, const int x_length, const int y_length, const int z_length, const int w_length)
{
  std::function<void()> res_func = [=]()
  {
    func(array);
  };
  return push_func(res_func, prerequisite, num_of_subtasks, task_type, priority, dependency_int);
}
}
