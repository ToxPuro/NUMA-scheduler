#include <thread>
#include <stdio.h>
#include <atomic>
#include <algorithm>
#include <scheduler.h>

bool
always_true()
{
  return true;
}
void
NsTask::make_subtasks(SingleDimensionalFunc function)
{
  for(int i=0;i<num_of_subtasks;i++){
    const auto& [start, end]= function.task_bounds.GetInterval(i, num_of_subtasks);
    const int x = function.IsFortranFunc ? start+1: start;
      m_subtasks_lambdas.push_back(
        [=](){
          function.lambda(x,end);
          subtasks_done[i]=true;
          Decrement();
        }
      );
    }
}
void
NsTask::make_subtasks(TwoDimensionalFunc function)
{
  for(int i=0;i<num_of_subtasks;i++){
    const auto& [x_start_, x_end]= function.x_task_bounds.GetInterval(i, num_of_subtasks);
    const int x_start = function.IsFortranFunc ? x_start_+1 : x_start_;
    const int y_start = function.IsFortranFunc ? function.y_task_bounds.start+1 : function.y_task_bounds.start;
      m_subtasks_lambdas.push_back(
        [=](){
          function.lambda(x_start,x_end,y_start,function.y_task_bounds.end);
          subtasks_done[i]=true;
          Decrement();
        }
      );
    }
}
void
NsTask::make_subtasks(const std::function<void()> lambda)
{
  for(int i=0;i<num_of_subtasks;i++){
      m_subtasks_lambdas.push_back(
        [=](){
          lambda();
          subtasks_done[i]=true;
          Decrement();
        }
      );
    }
}
bool NsTask::HasFinished()
{
  return m_latch.try_wait();
}
bool NsTask::PrerequisitesDone(const int subtask_id)
{
  if(m_dependencies.empty())
    return true;
  switch (m_dependency_type)
  {
  case All:
    return std::all_of(m_dependencies.begin(), m_dependencies.end(), [](NsTask* task){
      return task->HasFinished();
    });
  case Single:
    return std::all_of(m_dependencies.begin(), m_dependencies.end(), [=](NsTask* task){
      return task->subtasks_done[subtask_id];
    });
  default:
    return false;
  }
}
void
NsTask::Wait()
{
  while(!m_latch.try_wait()){}
  return;
}
bool
NsTask::Decrement()
{
  m_latch.count_down();
  return true;
}
void
NsTask::Reset()
{
  m_latch.reset();
  for(int i=0;i<subtasks_done.size();i++)
    subtasks_done[i]=false;
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
NsTask::MakeAsync(std::function<void()> lambda, const int core_id)
{
  return [=](){
    printf("LaunchAsync\n");
    auto t = std::thread(lambda);
    BindThreadToCore(t, core_id);
    t.detach();
    // t.detach();
    // this->m_threads_to_wait_on.push_back(std::move(t));
    // printf("threads: %ld", this->m_threads_to_wait_on.size());
    // this->m_threads_to_wait_on.emplace_back(std::thread(lambda));
    // BindThreadToCore(this->m_threads_to_wait_on[m_threads_to_wait_on.size()-1], core_id);
  };
}
std::function<void()>
NsTask::MakeCritical(std::function<void()> lambda)
{
  return [=](){
    std::lock_guard<std::mutex> critical_section_lock{critical_section_mutex};
    lambda();
  };
}
ThreadPool::ThreadPool(const int num_of_threads): 
  m_tasks({}), m_global_taskqueue(){
    for(int i=0;i<num_of_threads;i++)
      m_workers.emplace_back(std::make_unique<ThreadWorker>(i));
    StartProcessing();
  };
void
ThreadPool::PushSubtasks(NsTask* task)
{
  for(int i=0;i<task->num_of_subtasks;i++)
  {
    auto func = task->m_subtasks_lambdas[i];
    m_workers[i % m_workers.size()]->m_taskqueue.push_elems({
      std::pair<int, SubTask>(
        task->priority,
        (SubTask){
          task->type == Async? 
                    task->MakeAsync(func, m_workers[i % m_workers.size()] -> m_core_num): 
          task->type == Critical?
                    task->MakeCritical(func):
                    func,
          [=](){
            return task->PrerequisitesDone(i);
          }
        }
      )
    });
  }
  m_new_tasks_cv.notify_all();
}
bool
ThreadPool::Test(const TaskHandle& task_handle)
{
  return m_tasks[task_handle.task_id]->HasFinished();
}
void
ThreadPool::Wait(const TaskHandle& task_handle)
{
  m_tasks[task_handle.task_id]->Wait();
}
void
ThreadPool::WaitAll()
{
  for(auto task : m_tasks)
    task->Wait();
}
void
ThreadPool::ReLaunchAll()
{
  for(auto task : m_tasks)
  {
    task->Reset();
    PushSubtasks(task);
  }
}
void
ThreadPool::ProcessTasks(ThreadWorker& worker)
{
  while(m_keep_processing)
  {
    if(worker.m_taskqueue.empty() && m_global_taskqueue.empty())
    {
      std::unique_lock lock(worker.m_mutex);
      m_new_tasks_cv.wait(lock, [&]{ return !( worker.m_taskqueue.empty() && m_global_taskqueue.empty()) || !m_keep_processing;});
    }
    std::optional<std::pair<int,SubTask>> possible_pair 
      = worker.m_taskqueue.pop_first([](std::pair<int,SubTask> task){return task.second.IsReady();});
    if(possible_pair.has_value()) 
      possible_pair.value().second.Execute();
    //if no core specific tasks then look into the global queue
    else{
      std::optional<std::pair<int,SubTask>> possible_pair 
        = m_global_taskqueue.pop_first([](std::pair<int,SubTask> task){return task.second.IsReady();});
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
  printf("starting with %ld threads\n", m_workers.size());
  m_keep_processing=true;
  for(int i = 0;i<m_workers.size();++i){
    m_workers[i]->Launch([&](ThreadWorker& x){ this ->ProcessTasks(x);});
  }
}
void
ThreadPool::StopProcessing()
{
  m_keep_processing = false;
  m_new_tasks_cv.notify_all();
  printf("STOPPED PROCESSING\n");
  for(auto& worker : m_workers)
    worker->m_thread.join();
  
}

