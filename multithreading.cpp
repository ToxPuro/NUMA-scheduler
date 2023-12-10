#include <thread>
#include <stdio.h>
#include <functional>
#include <vector>
#include <atomic>
#include <algorithm>
#include <multithreading.h>
#include <iostream>
#include <chrono>
std::vector<TaskBounds> get_group_intervals(const size_t num_of_intervals, const TaskBounds task_bounds) {
   std::vector<TaskBounds> res;
   for(int i=0;i<num_of_intervals;i++)
      res.emplace_back((TaskBounds){
        i * task_bounds.length() / num_of_intervals,
        (i+ 1) * task_bounds.length() / num_of_intervals
      });
    return res;
}
Task::Task(const std::function<void(const int start, const int end)> lambda, const TaskBounds task_bounds, const int num_of_subtasks, const std::vector<Task*> dependencies): 
  m_lambda(lambda), m_latch(num_of_subtasks),  m_taskbounds(task_bounds), m_num_of_subtasks(num_of_subtasks), m_dependencies(dependencies){}
bool Task::HasFinished()
{
  return m_latch.try_wait();
}
bool Task::PrerequisitesDone()
{
  if(m_dependencies.empty())
    return true;
  return std::all_of(m_dependencies.begin(), m_dependencies.end(), [](Task* task){
    return task->HasFinished();
  });
}
bool
Task::Decrement()
{
  m_latch.count_down();
  return true;
}
std::vector<SubTask>
GenerateSubTasks(Task* task)
{
  std::vector<SubTask> res;
  for(TaskBounds taskbounds : get_group_intervals(task->m_num_of_subtasks, task->m_taskbounds))
  {

    res.emplace_back((SubTask){
      task,
      [=](){
        task->m_lambda(taskbounds.start,taskbounds.end);
        task->Decrement();
      }
    });
  }
  return res;
}
ThreadPool::ThreadPool(const int num_of_threads): 
  m_threads(num_of_threads), m_num_threads(num_of_threads), m_tasks({}), m_global_taskqueue(){
    for(int i=0;i<num_of_threads;i++)
      m_core_taskqueues.emplace_back(std::make_unique<TsMinVector<std::pair<size_t, SubTask>>>());
  };
TaskHandle
ThreadPool::Push(const std::function<void(const int start, const int end)> lambda, const int num_of_subtasks, const size_t start, const size_t end, const size_t priority, std::vector<TaskHandle> dependency_handles){

  std::vector<Task*> dependencies;
  for(TaskHandle handle : dependency_handles)
    dependencies.emplace_back(m_tasks[handle.task_id]);

  m_tasks.emplace_back(new Task(
    lambda, 
    (TaskBounds){start, end},
    num_of_subtasks,
    dependencies
  ));

  std::vector<std::pair<size_t, SubTask>> subtasks_with_priority;
  for(SubTask subtask : GenerateSubTasks(m_tasks[m_tasks.size()-1]))
    subtasks_with_priority.emplace_back(std::pair<size_t, SubTask>(priority, subtask));
  for(int i=0;i<subtasks_with_priority.size();++i)
    m_core_taskqueues[i]->push_elems(subtasks_with_priority);
  return (TaskHandle){static_cast<int>(m_tasks.size()-1)};
}
bool
ThreadPool::Test(const TaskHandle& task_handle)
{
  return m_tasks[task_handle.task_id]->HasFinished();
}
void
ThreadPool::Wait(const TaskHandle& task_handle)
{
  while(!m_tasks[task_handle.task_id]->HasFinished());
}
void
ThreadPool::WaitAll()
{
  for(auto& task : m_tasks)
    while(!task->HasFinished());
}
void
ThreadPool::ReLaunch(const TaskHandle& task_handle)
{
}
void
ThreadPool::ProcessTasks()
{

  while(m_keep_processing)
  {
    int i = 0;
    std::optional<std::pair<size_t,SubTask>> possible_pair 
      = m_core_taskqueues[i]->pop_first([](std::pair<size_t,SubTask> task){return task.second.IsReady();});
    if(possible_pair.has_value()) 
      possible_pair.value().second.Execute();
    //if no core specific tasks then look into the global queue
    else{
      std::optional<std::pair<size_t,SubTask>> possible_pair 
        = m_global_taskqueue.pop_first([](std::pair<size_t,SubTask> task){return task.second.IsReady();});
      if(possible_pair.has_value()) possible_pair.value().second.Execute();
    }
  }
}
bool
BindThreadToCore(std::thread& t, const int core_id)
{
    // Credit: https://eli.thegreenplace.net/2016/c11-threads-affinity-and-hyperthreading/
    // Create a cpu_set_t object representing a set of CPUs. Clear it and mark
    // only CPU core_id as set.
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(core_id, &cpuset);
    const int rc = pthread_setaffinity_np(t.native_handle(),
                                    sizeof(cpu_set_t), &cpuset);
    return static_cast<bool>(1-rc);
}
void
ThreadPool::StartProcessing()
{
  printf("starting with threads: %d\n", m_num_threads);
  m_keep_processing=true;
  for(int i = 0;i<m_num_threads;++i){
    m_threads[i] = std::thread(
      [=]{
        this->ProcessTasks();
      }
    );
    BindThreadToCore(m_threads[i], i);
  }

    
}
void
ThreadPool::StopProcessing()
{
  m_keep_processing = false;
  printf("STOPPED PROCESSING\n");
  for(auto& t : m_threads)
    t.join();
}
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
  for(int i=0;i<processor_count;++i)
  {
    TaskHandle task_handle = pool.Push(print_difference,2,1,100,1);
    handles.push_back(task_handle);
  }
  // std::vector<Task*> dependencies;
  // Task task(print_difference, (TaskBounds){1,99}, 2);
  // std::vector<SubTask> subtasks = GenerateSubTasks(&task);
  // // task.m_lambda(1,2)
  // printf("\n\n");
  // printf("Is ready: %d\n",subtasks[0].IsReady());
  // printf("%d\n",subtasks.size());
  // subtasks[0].Execute();
  // subtasks[1].Execute();
  // subtasks.erase(subtasks.begin());
  // task.PrerequisitesDone();
  // pool.ExecuteAllTasks();
  // pool.ProcessTasks();
  // pool.StartProcessing();

  // Task* a = &task;

  // a->Decrement();
  pool.StartProcessing();
  // pool.ProcessTasks();
  // for(auto handle : handles)
  //   pool.Wait(handle);
  pool.WaitAll();
  pool.StopProcessing();
  return 0;
}
