#pragma once
#include <cstddef>
#include <vector>
#include <functional>
#include <queue>
#include <mutex>
#include <optional>
#include <mpi.h>
typedef struct 
{
  const size_t start;
  const size_t end;
  size_t length() const{
    return end-start;
  }
} TaskBounds;
typedef struct 
{
  const int task_id;
} TaskHandle;

class Latch{
  Latch(const Latch&) = delete;
  Latch(Latch&&) = delete;
  private:
    std::atomic<int> counter;
  public:
    Latch(const int starting_value): counter(starting_value){}
    void
    count_down(){
      counter.fetch_sub(1, std::memory_order_release);
    }
    bool
    try_wait()
    {
      return counter.load(std::memory_order_acquire)==0;
    }
    void
    wait()
    {
      while(!try_wait());
    }
};

class Task {
  Task(const Task&) = delete;
  private:
    const std::vector<Task*> m_dependencies;
    Latch m_latch;
    std::vector<MPI_Request> m_requests_to_wait_on;
  public:
    const TaskBounds m_taskbounds;
    const int m_num_of_subtasks;
    const std::function<void(const int start, const int end)> m_lambda;
    Task(const std::function<void(const int start, const int end)> lambda, const TaskBounds task_bounds, const int num_of_subtasks=1, const std::vector<Task*> dependencies=std::vector<Task*>());
    bool HasFinished();
    bool PrerequisitesDone();
    bool Decrement();
};

typedef struct{
  Task* task;
  std::function<void()> lambda;
// SubTask::SubTask(const SubTask& other):
//   m_lambda(other.m_lambda), m_task(other.m_task){}
// SubTask::SubTask(SubTask&& other):
//   m_lambda(other.m_lambda), m_task(other.m_task){}
  bool
  IsReady(){
    // return true;
    return task->PrerequisitesDone();
  }
  void
  Execute()
  {
    lambda();
  }
} SubTask;

template <class T>
class TsMinVector{
  TsMinVector(const TsMinVector&) = delete;
  private:
    std::mutex m_queue_mutex;

  public:
    std::vector<T> m_vector;
    TsMinVector(): m_vector({}), m_queue_mutex(){}
    bool
    push_elems(std::vector<T> const& vals);
    typename std::optional<T>
    pop_first(std::function<bool(const T&)> condition);
    size_t
    size()
    {
      return m_vector.size();
    }
};
template <class T>
bool
TsMinVector<T>::push_elems(std::vector<T> const& vals)
{
    std::lock_guard<std::mutex> queue_lock{m_queue_mutex};
    for(auto val : vals)
      m_vector.push_back(val);
    // std::sort(m_vector.begin(),m_vector.end());
    return true;
}
template <class T>
std::optional<T>
TsMinVector<T>::pop_first(std::function<bool(const T&)> condition)
{
  std::lock_guard<std::mutex> queue_lock{m_queue_mutex};
  for (auto it = m_vector.begin(); it != m_vector.end();  ++it )
  {
       auto value = *it;
       if(condition(value))
       {
         m_vector.erase(it);
         return std::make_optional<T>(value);
       }
  }
  return std::nullopt;
}
class ThreadPool {
  ThreadPool(const ThreadPool&) = delete;
	private:
    const int m_num_threads;
    volatile bool m_keep_processing;
    std::vector<std::thread> m_threads;
    std::vector<Task*> m_tasks;
    //We use minvector instead of minheap since we want to iterate over the elements
    //Could change this in the future but at least std::queue can't be iterated over
    std::vector<std::unique_ptr<TsMinVector<std::pair<size_t, SubTask>>>> m_core_taskqueues;
    TsMinVector<std::pair<size_t, SubTask>> m_global_taskqueue;
	public:
		ThreadPool(const int num_of_threads);
    TaskHandle 
    Push(const std::function<void(const int start, const int end)> lambda, const int num_of_subtasks, const size_t start, const size_t end, const size_t priority, std::vector<TaskHandle> dependency_handles=std::vector<TaskHandle>());
    bool
    Test(const TaskHandle& task_handle); 
    void
    Wait(const TaskHandle& task_handle);
    void
    WaitAll();
    void
    ReLaunch(const TaskHandle& task_handle);
    void
    ProcessTasks();
    void
    StartProcessing();
    void
    StopProcessing();
    void
    ExecuteAllTasks();
};