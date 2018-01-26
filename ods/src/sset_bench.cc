// list_bench runs benchmarks against List implementations.
#include <cassert>
#include <chrono>
#include <iostream>
#include <random>
#include <string>
#include <ods/sset.h>
#include <ods/simpsset.h>

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

static void
benchmark(ods::SSet<int>& set, int ops)
{
	for (int i = 0; i < ops; i++) {
		auto op = randint(0, 3);
		int val = randint(1, 100);
		
		switch (op) {
		// add
		case 0:
		case 1:
			cout << "\tadd " << val << endl;
			set.add(val);
			break;
		// remove
		case 2:
			set.remove(val);
			break;
		// find
		case 3:
			set.find(val);
			break;
		}
	}
}


static void
run(string label, ods::SSet<int>& list, int ops)
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
	
	ods::SimpSSet<int>	us;
	run("SimpSSet", us, ops);
	
	return 0;
}