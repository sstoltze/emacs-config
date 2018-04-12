#include <iostream>
#include <iomanip>
using namespace std;

int main() {
  cout << setfill(' ') << setw(17);
  cout << "Hello World!" << endl;
  cout << setfill('-') << setw(15) << left;
  cout << "Hello World!" << endl;
  return 0;
}
