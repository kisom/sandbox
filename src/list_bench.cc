// list_bench runs benchmarks against List implementations.
#include <cassert>
#include <chrono>
#include <iostream>
#include <random>
#include <string>
#include <ods/list.h>
#include <ods/simplist.h>
#include <ods/vlist.h>

using namespace std;

static mt19937		rng;
static random_device	devrand;

// reseed picks a new seed for the RNG using the system random device.
static void
reseed()
{
	rng.seed(devrand());
}


static int
randint(int low, int high)
{
	uniform_int_distribution<int>	dist(low, high);
	return dist(rng);
}

static int
randsizet(size_t low, size_t high)
{
	uniform_int_distribution<size_t>	dist(low, high);
	return dist(rng);
}

static void
benchmark(ods::List<int>& list, int ops)
{
	for (int i = 0; i < ops; i++) {
		auto op = randint(0, 3);
		size_t j = 0;
		int newval, oldval;
		
		if (list.size() > 0) {
			j = randsizet(0, list.size()-1);
		}
		switch (op) {
		// get
		case 0:
			if (list.size() == 0) {
				i--;
				continue;
			}

			cout << "\tget " << j << endl;
			assert(list.get(j) != -1);
		// set
		case 1:
			if (list.size() == 0) {
				i--;
				continue;
			}

			oldval = list.get(j);
			newval = oldval;
			while (newval == oldval) {
				newval = randint(1, 1000000);
			}
			
			cout << "\tset " << j << " from " << oldval << " to " << newval << endl;
			assert(list.set(j, newval) == oldval);
			assert(list.get(j) == newval);
			break;
		// add
		case 2:
			newval = randint(1, 1000000);
			list.add(j, newval);
			cout << "\tadd " << j << " " << newval << endl;
			assert(list.get(j) == newval);
			break;
		// remove
		case 3:
			if (list.size() == 0) {
				i--;
				continue;
			}
			
			auto old_size = list.size();
			cout << "\tremove " << j << endl;
			list.remove(j);
			assert(list.size() == (old_size-1));
			break;
		}
	}
}


static void
run(string label, ods::List<int>& list, int ops)
{
	std::chrono::steady_clock	clock;
	
	auto start = clock.now();
	benchmark(list, ops);
	auto stop = clock.now();
	
	std::chrono::duration<double> delta = stop - start;
	cerr << label << " @ " << ops << " ops: " << delta.count() << "s" << endl;
}


int
main(int argc, char *argv[])
{
	int	ops = 1000;
	
	if (argc == 2) {
		ops = stoi(argv[1]);
	}
	
	reseed();
	
	ods::SimpList<int>	sl;
	run("SimpList", sl, ops);
	
	ods::VList<int>		vl;
	run("VList", vl, ops);
}