#include <cassert>
#include <iostream>
#include <ods/simplist.h>
#include <ods/simpuset.h>
#include <ods/simpsset.h>
#include <ods/linked_list.h>
using namespace std;


static void
check_simplist(void)
{
	ods::SimpList<int>	sl;

	sl.add(0, 1);
	sl.add(1, 2);
	sl.add(2, 3);
	assert(sl.size() == 3);

	sl.add(0, 4);
	sl.add(1, 5);
	sl.add(2, 6);
	assert(sl.size() == 6);
	
	int	expected[6] = {4, 5, 6, 1, 2, 3};
	for (size_t i = 0; i < 6; i++) {
		assert(sl.get(i) == expected[i]);
	}
	
	bool caught = false;
	try {
		sl.add(8, 8);
	} catch (std::invalid_argument) {
		caught = true;
	}
	assert(caught);
	
	assert(sl.get(2) == 6);
	sl.remove(2);
	assert(sl.get(2) == 1);
	assert(sl.size() == 5);
	sl.set(2, 6);
	assert(sl.get(2) == 6);
	
	// expected = {1, 2, 3, 4, 5, 6};
	for (size_t i = 0; i < 6; i++) {
		expected[i] = i+1;
	}

	for (size_t i = 0; i < 5; i++) {
		sl.set(i, i+1);
	}
	sl.add(5, 6);
	
	for (size_t i = 0; i < 6; i++) {
		assert(sl.get(i) == expected[i]);
	}
}


static void
check_simpuset(void)
{
	ods::SimpUSet<int>	us;
	
	assert(us.add(1));
	assert(us.size() == 1);
	assert(us.find(1));
	assert(!us.add(1));
	assert(us.size() == 1);
	assert(us.add(2));
	assert(us.find(2));
	assert(us.add(3));
	assert(us.size() == 3);
	assert(us.find(3));
	
	auto removed = us.remove(2);
	assert(removed == 2);
	assert(us.size() == 2);
	assert(!us.find(2));
}


static void
check_simpsset(void)
{
	ods::SimpSSet<int>	ss;
	
	assert(ss.add(2));
	assert(ss.size() == 1);
	assert(ss.find(2));
	assert(!ss.add(2));
	assert(ss.size() == 1);
	assert(ss.add(1));
	assert(ss.find(1));
	assert(ss.size() == 2);
	assert(ss.add(3));
	assert(ss.size() == 3);
	assert(ss.find(3));
	
	auto removed = ss.remove(2);
	assert(removed == 2);
	assert(ss.size() == 2);
	assert(!ss.find(2));
}

void check_linkedlist(void);

void
check_linkedlist()
{
	ods::LinkedList<int>	ll;
	
	ll.add(0, 1);
	assert(ll.size() == 1);
	assert(ll.get(0) == 1);
	ll.add(0, 2);
	assert(ll.size() == 2);
	assert(ll.get(0) == 2);
	ll.add(2, 4);
	assert(ll.get(2) == 4);
	assert(ll.size() == 3);
	
	ll.set(1, 5);
	assert(ll.get(1) == 5);
	
	ll.remove(1);
	assert(ll.size() == 2);
	assert(ll.get(1) == 4);
	ll.remove(1);
	assert(ll.size() == 1);
	ll.remove(0);
	assert(ll.size() == 0);
	
	ll.add(0, 1);
}


int
main(void)
{
	check_simplist();
	check_simpuset();
	check_simpsset();
	check_linkedlist();
	cout << "OK" << endl;
}
