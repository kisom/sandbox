#include <iostream>
#include <ods/array.h>
#include <ods/array_stack.h>
using namespace std;
using namespace ods;

int
main(void)
{
	Array<int>	a(2);
	Array<int>	b(5);;
	cout << "a[0] " << a[0] << endl;
	
	b = a;
	cout << "b[0] " << b[0] << endl;

	ArrayStack<int>	as(5);
	for (int i = 0; i < 3; i++) {
		as.set(i, i+1);
	}
	as.add(1, 42);
	cout << "as[0] " << as.get(0) << endl;
	cout << "as[1] " << as.get(1) << endl;
	as.remove(0);
	cout << "as[0] " << as.get(0) << endl;

	cout << "size: " << as.size() << ", cap: " << as.cap() << endl;
	as.add(0, 47);
	as.add(0, 11);
	as.add(0, 17);
	as.add(0, 1);
	cout << "size: " << as.size() << ", cap: " << as.cap() << endl;
}
