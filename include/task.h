#pragma once
#include <functional>
#include <vector>
#include <latch.h>
#include <thread>
#include <mpi.h>
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

typedef struct SingleDimensionalFunc
{
  const std::function<void(const int a, const int b)> lambda;
  const TaskBounds task_bounds;
  const bool IsFortranFunc =true;
} SingleDimensionalFunc;
typedef struct TwoDimensionalFunc
{
  const std::function<void(const int a, const int b, const int c, const int d)> lambda;
  const TaskBounds x_task_bounds;
  const TaskBounds y_task_bounds;
  const bool IsFortranFunc =true;
} TwoDimensionalFunc;
typedef struct ThreeDimensionalFunc
{
  const std::function<void(const int x_start, const int x_end, const int y_start, const int y_end, const int z_start, const int z_end)> lambda;
  const TaskBounds x_task_bounds;
  const TaskBounds y_task_bounds;
  const TaskBounds z_task_bounds;
  const bool IsFortranFunc =true;
} ThreeDimensionalFunc;
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


class NsTask {
  NsTask(const NsTask&) = delete;
  private:
    const std::vector<NsTask*> m_dependencies;
    Latch m_latch;
    DependencyType m_dependency_type;
    void
    make_subtasks(SingleDimensionalFunc lambda);
    void
    make_subtasks(TwoDimensionalFunc lambda);
    void
    make_subtasks(ThreeDimensionalFunc lambda);
    void
    make_subtasks(const std::function<void()> lambda);
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
    NsTask(const F lambda, const int num_of_subtasks=1, const std::vector<NsTask*> dependencies=std::vector<NsTask*>(), DependencyType dependency_type=All, const int priority=0, TaskType=Default):
      m_latch(num_of_subtasks), num_of_subtasks(num_of_subtasks), m_dependencies(dependencies), m_dependency_type(dependency_type), subtasks_done(num_of_subtasks, false), priority(priority), type(type)
    {
      make_subtasks(lambda);
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