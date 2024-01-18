#pragma once
#include <functional>
#include <vector>
#include <latch.h>
#include <thread>
#include <mpi.h>
typedef enum 
{
  Default,
  Async,
  Critical
} TaskType;
typedef enum
{
  All,
  Single
} DependencyType;

typedef struct TaskBounds
{
  const size_t start;
  const size_t end;
  size_t length() const{
    return end-start;
  }
  TaskBounds
  GetInterval(const int interval_num, const int num_of_intervals) const
  {

    return (TaskBounds){
      interval_num * length() / num_of_intervals,
      interval_num == num_of_intervals? 
        length():
        (interval_num + 1) * length() / num_of_intervals
    };
  }
} TaskBounds;

class NsTask {
  NsTask(const NsTask&) = delete;
  private:
    const std::vector<NsTask*> m_dependencies;
    Latch m_latch;
    DependencyType m_dependency_type;
    void
    make_subtasks(const std::function<void(const int start, const int end)> lambda, const TaskBounds task_bounds);
    void
    make_subtasks(const std::function<void()> lambda, const TaskBounds task_bounds);
    //Constant
  public:
    std::vector<std::function<void()>> m_subtasks_lambdas;
    const int priority;
    std::vector<bool> subtasks_done;
    std::mutex critical_section_mutex;
    TaskType type;
  public:
    const int num_of_subtasks;
    template <typename F>
    NsTask(const F lambda, const TaskBounds task_bounds, const int num_of_subtasks=1, const std::vector<NsTask*> dependencies=std::vector<NsTask*>(), DependencyType dependency_type=All, const int priority=0, TaskType=Default):
      m_latch(num_of_subtasks), num_of_subtasks(num_of_subtasks), m_dependencies(dependencies), m_dependency_type(dependency_type), subtasks_done(num_of_subtasks, false), priority(priority), type(type)
    {
      make_subtasks(lambda, task_bounds);
    }
    // NsTask(const std::function<void()> lambda, const TaskBounds task_bounds, const int num_of_subtasks=1, const std::vector<NsTask*> dependencies=std::vector<NsTask*>(), DependencyType dependency_type=All, const int priority=0, TaskType=Default);
    bool HasFinished();
    bool PrerequisitesDone(const int subtask_id);
    bool Decrement();
    std::function<void()>
    MakeAsync(std::function<void()> lambda, const int core_id);
    std::function<void()>
    MakeCritical(std::function<void()> lambda);
    void
    Wait();
    void
    Reset();
};

typedef struct SubTask{
  std::function<void()> lambda;
  std::function<bool()> ready_check;
  //Order does not matter
  bool operator<(const SubTask& other) const
  {
    return true;
  }
  bool
  IsReady(){
    return ready_check();
  }
  void
  Execute()
  {
    lambda();
  }
} SubTask;