#include <iostream>
#include <vector>
#include <string>
#include <chrono>
#include <thread>
#include "unistd.h"

class Base {
public:
  int baseInt=3;
  std::string baseString="base";
  Base() {
  }
  virtual ~Base() {
  }
};
class Sub : public Base {
public:
  int subInt=4;
  std::string subString="sub";
  Sub* member=nullptr;
  Sub() : Base() {
  }
  virtual ~Sub() {
  }
  void setMember(Sub* sub) {
    member = sub;
  }
};

void func(int param) {
  int intVar=2;
  char* charPtr="test string";
  std::string str="test std string";
  std::vector<int> vectorInts={0,1,2,3};
  Base* b = new Base();
  Base b2 = {};
  Base* b3 = new Sub();
  Sub* sub = new Sub();
  sub->setMember(sub);
  Sub sub2 = {};
  // TODO map, set
  // TODO object ptr, object ref
  // TODO unique ptr
  std::cout << str << std::endl;
  std::cout << getpid() << std::endl;
  size_t i=0;
  while(true) {
    i = i + 1;
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    i = i - 1;
    sub->setMember(sub);
    sub2.setMember(sub);
  }
}

int main() {
  int var=3;
  func(var);
  return 0;
}
