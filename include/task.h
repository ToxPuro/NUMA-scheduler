#pragma once
#include <functional>
#include <vector>
#include <latch.h>
#include <mpi.h>
typedef struct 
{
  const size_t start;
  const size_t end;
  size_t length() const{
    return end-start;
  }
} TaskBounds;

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

typedef struct SubTask{
  Task* task;
  std::function<void()> lambda;
  //Order does not matter
  bool operator<(const SubTask& other) const
  {
    return true;
  }
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