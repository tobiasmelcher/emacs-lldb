#include <iostream>
#include <vector>
#include <string>
#include <chrono>
#include <thread>
#include "unistd.h"
#include <unordered_map>
#include <memory>

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
  //  map
  std::unordered_map<int,Base*> map;
  map.insert({1,new Sub()});
  map.insert({2,new Base()});
  // TODO set
  // TODO object ptr, object ref
  // unique ptr
  std::unique_ptr<Sub> unique_sub = std::make_unique<Sub>();
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
