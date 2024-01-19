#pragma once
#include <cstddef>
#include <queue>
#include <mutex>
#include <optional>
#include <task.h>
#include <thread>
#include <condition_variable>
struct TaskHandle
{
  const int task_id;
  bool operator==(const TaskHandle& other)
  {
    return task_id == other.task_id;
  }
};


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
    bool 
    empty()
    {
      return m_vector.empty();
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
    TsMinVector<std::pair<int, SubTask>> m_taskqueue;
    std::mutex m_mutex;
    std::thread m_thread;
    const int m_core_num;
    ThreadWorker(const int core_num):
      m_core_num(core_num){}
    void
    Launch(std::function<void(ThreadWorker&)> lambda);

};
class ThreadPool {
  ThreadPool(const ThreadPool&) = delete;
	private:
    volatile bool m_keep_processing;
    std::vector<NsTask*> m_tasks;
    std::vector<std::unique_ptr<ThreadWorker>> m_workers;
    std::condition_variable m_new_tasks_cv;
    //We use minvector instead of minheap since we want to iterate over the elements
    //Could change this in the future but at least std::queue can't be iterated over
    TsMinVector<std::pair<int, SubTask>> m_global_taskqueue;
    void
    ProcessTasks(ThreadWorker& worker);
    void
    StartProcessing();
    void
    PushSubtasks(NsTask* task);
	public:
		ThreadPool(const int num_of_threads);
    template <typename F>
    TaskHandle 
    Push(const F lambda, const int num_of_subtasks, const int priority, std::vector<TaskHandle> dependency_handles=std::vector<TaskHandle>(), TaskType type=Default, DependencyType dependency_type=All){
      std::vector<NsTask*> dependencies;
      for(TaskHandle handle : dependency_handles)
        dependencies.emplace_back(m_tasks[handle.task_id]);
      auto task = new NsTask( lambda, num_of_subtasks, dependencies, dependency_type, priority, type);
      m_tasks.push_back(task);
      PushSubtasks(task);
      return (TaskHandle){static_cast<int>(m_tasks.size()-1)};
    }
    bool
    Test(const TaskHandle& task_handle); 
    void
    Wait(const TaskHandle& task_handle);
    void
    WaitAll();
    void
    ReLaunchAll();
    void
    StopProcessing();
    void
    ExecuteAllTasks();
};
