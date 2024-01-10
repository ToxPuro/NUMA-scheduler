#pragma once
#include <atomic>
class Latch{
  Latch(const Latch&) = delete;
  Latch(Latch&&) = delete;
  private:
    std::atomic<int> counter;
    const int original_value;
  public:
    Latch(const int starting_value): counter(starting_value), original_value(starting_value){}
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
    void
    reset()
    {
      wait();
      counter = original_value;
    }
};