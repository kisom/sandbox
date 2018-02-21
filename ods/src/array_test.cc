#include <cassert>
#include <iostream>
#include <ods/array.h>
#include <ods/array_stack.h>
#include <ods/array_queue.h>
#include <ods/array_deque.h>
using namespace std;
using namespace ods;

int
main(void)
{
	Array<int>	a(2);
	Array<int>	b(5);

	cout << "=== Array ===" << endl;
	cout << "a[0] " << a[0] << endl;
	
	b = a;
	cout << "b[0] " << b[0] << endl;

	cout << "=== ArrayStack ===" << endl;
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

	cout << "=== ArrayQueue ===" << endl;
	ArrayQueue<int> 	aq(5);
	cout << "size: " << aq.size() << ", cap: " << aq.cap() << endl;
	for (int i = 0; i < 100; i++) {
		aq.add(i);
	}
	cout << "size: " << aq.size() << ", cap: " << aq.cap() << endl;
	for (int i = 0; i < 100; i++) {
		aq.remove();
	}
	cout << "size: " << aq.size() << ", cap: " << aq.cap() << endl;

	cout << "=== ArrayDeque ===" << endl;
	ArrayDeque<int>	ad(1);
	for (int i = 0; i < 5; i++) {
		ad.add(0, 4-i);
	}
	cout << "size: " << ad.size() << ", cap: " << ad.cap() << endl;

	for (int i = 0; i < 5; i++) {
		ad.add(ad.size(), i);
	}
	cout << "size: " << ad.size() << ", cap: " << ad.cap() << endl;

	for (int i = 0; i < ad.size(); i++) {
		cout << i << "\t" << ad.get(i) << endl;
	}

	auto x = ad.remove(1);
	assert(x == 1);
	x = ad.remove(7);
	assert(x == 3);
	cout << "size: " << ad.size() << ", cap: " << ad.cap() << endl;

	for (int i = 0; i < ad.size(); i++) {
		cout << i << "\t" << ad.get(i) << endl;
	}
	ad.remove(2);
	ad.remove(3);
	ad.remove(4);
	cout << "size: " << ad.size() << ", cap: " << ad.cap() << endl;
}
