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
  GetInterval(const int interval_num, const int num_of_intervals)
  {

    return (TaskBounds){
      interval_num * length() / num_of_intervals,
      interval_num == num_of_intervals? 
        length():
        (interval_num + 1) * length() / num_of_intervals
    };
  }
} TaskBounds;

class Task {
  Task(const Task&) = delete;
  private:
    const std::vector<Task*> m_dependencies;
    Latch m_latch;
    DependencyType m_dependency_type;
  public:
    std::vector<bool> subtasks_done;
    std::mutex critical_section_mutex;
  public:
    const TaskBounds m_taskbounds;
    const int m_num_of_subtasks;
    const std::function<void(const int start, const int end)> m_lambda;
    Task(const std::function<void(const int start, const int end)> lambda, const TaskBounds task_bounds, const int num_of_subtasks=1, const std::vector<Task*> dependencies=std::vector<Task*>(), DependencyType depency_type=All);
    bool HasFinished();
    bool PrerequisitesDone(const int subtask_id);
    bool Decrement();
    std::function<void()>
    MakeAsync(std::function<void()> lambda, const int core_id);
    std::function<void()>
    MakeCritical(std::function<void()> lambda);
    void
    Wait();
};

typedef struct SubTask{
  Task* task;
  int id;
  std::function<void()> lambda;
  //Order does not matter
  bool operator<(const SubTask& other) const
  {
    return true;
  }
  bool
  IsReady(){
    return task->PrerequisitesDone(id);
  }
  void
  Execute()
  {
    lambda();
  }
} SubTask;