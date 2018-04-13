#include <cassert>
#include <iostream>
#include <random>
#include <vector>

#include "algotune.h"

namespace algotune {


static std::mt19937		rng;
static std::random_device	devrand;


void
reseed()
{
	rng.seed(devrand());
}


int
rand_int(int low, int high)
{
	std::uniform_int_distribution<int>	dist(low, high);
	return dist(rng);
}


int64_t
rand_int64(int64_t low, int64_t high)
{
	std::uniform_int_distribution<int64_t>	dist(low, high);
	return dist(rng);
}


std::vector<int>
gen_int_vector(int size, int low, int high)
{
	assert(low < high);
	reseed();
	std::vector<int> v(size);

	for (int i = 0; i < size; i++) {
		v[i] = rand_int(low, high);
	}

	return v;
}


std::vector<int64_t>
gen_int64_vector(int64_t size, int64_t low, int64_t high)
{
	assert(low < high);
	reseed();
	std::vector<int64_t> v(size);

	for (int64_t i = 0; i < size; i++) {
		v[i] = rand_int64(low, high);
	}

	return v;
}


int64_t
stress_test_int(bool(*func)(std::vector<int>&), int size, int low, int high)
{
	assert(func != nullptr);
	std::vector<int>	v;
	bool			result = true;
	int64_t			iterations = 0;
	int			vsize = 0;

	while (result) {
		iterations++;
		vsize = rand_int(0, size);
		v = gen_int_vector(vsize, low, high);
		result = func(v);

		if ((iterations > 1) && ((iterations % display_step) == 0)) {
			std::cout << "Iteration: " << iterations << std::endl;
		}
	}

	return iterations;
}


int64_t
stress_test_int64(bool(*func)(std::vector<int64_t>&), int64_t size, int64_t low, int64_t high)
{
	assert(func != nullptr);
	std::vector<int64_t>	v;
	bool			result = true;
	int64_t			iterations = 0;
	int64_t			vsize = 0;

	while (result) {
		iterations++;
		vsize = rand_int64(0, size);
		v = gen_int64_vector(vsize, low, high);
		result = func(v);

		if ((iterations > 1) && ((iterations % display_step) == 0)) {
			std::cout << "Iteration: " << iterations << std::endl;
		}
	}

	return iterations;
}


} // namespace algotune
