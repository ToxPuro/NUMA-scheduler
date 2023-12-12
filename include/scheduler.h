#pragma once
#include <cstddef>
#include <queue>
#include <mutex>
#include <optional>
#include <task.h>
#include <thread>
typedef struct 
{
  const int task_id;
} TaskHandle;


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
    std::sort(m_vector.begin(),m_vector.end());
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
class ThreadWorker{
  public:
    TsMinVector<std::pair<size_t, SubTask>> m_taskqueue;
    std::mutex m_mutex;
    std::thread m_thread;
};
class ThreadPool {
  ThreadPool(const ThreadPool&) = delete;
	private:
    volatile bool m_keep_processing;
    std::vector<Task*> m_tasks;
    std::vector<std::unique_ptr<ThreadWorker>> m_workers;
    //We use minvector instead of minheap since we want to iterate over the elements
    //Could change this in the future but at least std::queue can't be iterated over
    TsMinVector<std::pair<size_t, SubTask>> m_global_taskqueue;
    void
    ProcessTasks(const int i);
	public:
		ThreadPool(const int num_of_threads);
    TaskHandle 
    Push(const std::function<void(const int start, const int end)> lambda, const int num_of_subtasks, const size_t start, const size_t end, const size_t priority, std::vector<TaskHandle> dependency_handles=std::vector<TaskHandle>(), const bool is_async=false);
    bool
    Test(const TaskHandle& task_handle); 
    void
    Wait(const TaskHandle& task_handle);
    void
    WaitAll();
    void
    ReLaunch(const TaskHandle& task_handle);
    void
    StartProcessing();
    void
    StopProcessing();
    void
    ExecuteAllTasks();
};