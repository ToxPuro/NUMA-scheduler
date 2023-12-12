#include <thread>
#include <stdio.h>
#include <atomic>
#include <algorithm>
#include <scheduler.h>


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
std::function<void()>
MakeAsync(std::function<void()> lambda, const int core_id)
{
  return [=](){
    auto t = std::thread(lambda);
    BindThreadToCore(t, core_id);
    t.detach();
  };
}
ThreadPool::ThreadPool(const int num_of_threads): 
  m_tasks({}), m_global_taskqueue(){
    for(int i=0;i<num_of_threads;i++)
      m_workers.emplace_back(std::make_unique<ThreadWorker>(i));
  };
TaskHandle
ThreadPool::Push(const std::function<void(const int start, const int end)> lambda, const int num_of_subtasks, const size_t start, const size_t end, const size_t priority, std::vector<TaskHandle> dependency_handles, const bool is_async){
  std::vector<Task*> dependencies;
  for(TaskHandle handle : dependency_handles)
    dependencies.emplace_back(m_tasks[handle.task_id]);
  auto task_bounds = (TaskBounds){start,end};
  auto task = new Task( lambda, task_bounds, num_of_subtasks, dependencies);
  m_tasks.push_back(task);

  for(int i=0;i<num_of_subtasks;i++)
  {
    const auto& [start, end]= task_bounds.GetInterval(i, num_of_subtasks);
    auto func = [=](){
        task->m_lambda(start,end);
        task->Decrement();
    };
    m_workers[i % m_workers.size()]->m_taskqueue.push_elems({
      std::pair<size_t, SubTask>(
        priority,
        (SubTask){
          task,
          is_async ? MakeAsync(func, m_workers[i % m_workers.size()] -> m_core_num) : func
        }
      )
    });
  }
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
ThreadPool::ProcessTasks(ThreadWorker& worker)
{

  while(m_keep_processing)
  {
    std::optional<std::pair<size_t,SubTask>> possible_pair 
      = worker.m_taskqueue.pop_first([](std::pair<size_t,SubTask> task){return task.second.IsReady();});
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
void
ThreadWorker::Launch(std::function<void(ThreadWorker&)> lambda)
{
  m_thread = std::thread([=]{ lambda(*this);});
  BindThreadToCore(m_thread, m_core_num);
}
void
ThreadPool::StartProcessing()
{
  printf("starting with threads: %ld\n", m_workers.size());
  m_keep_processing=true;
  for(int i = 0;i<m_workers.size();++i){
    m_workers[i]->Launch([&](ThreadWorker& x){ this ->ProcessTasks(x);});
  }

    
}
void
ThreadPool::StopProcessing()
{
  m_keep_processing = false;
  printf("STOPPED PROCESSING\n");
  for(auto& worker : m_workers)
    worker->m_thread.join();
}
