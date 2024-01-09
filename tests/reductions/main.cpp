#include <scheduler.h>
#include <memory>
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
  for(int i=0;i<1;++i)
  {
    TaskHandle task_handle = pool.Push(print_difference,3,1,100,1,{},Async);
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
  // pool.ProcessTasks();
  // for(auto handle : handles)
  //   pool.Wait(handle);
  pool.WaitAll();
  pool.StopProcessing();
  return 0;
}
