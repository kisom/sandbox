#include <cassert>
#include <iostream>
#include <ods/simplist.h>
#include <ods/simpuset.h>
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


int
main(void)
{
	check_simplist();
	check_simpuset();
	cout << "OK" << endl;
}
