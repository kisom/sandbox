#include <random>
#include <vector>

#include "algotune.h"

namespace algotune {

int
rand_int(int low, int high)
{
	return -1;
}


int64_t
rand_int(int64_t low, int64_t high)
{
	return -1;
}


std::vector<int>
gen_int_vector(int size, int low, int high)
{
	return vector<int>(0);
}


std::vector<int64_t>
gen_int_vector(int size, int low, int high)
{
	return vector<int64_t>(0);
}


void
stress_test_int(bool(*func)(std::vector<int>))
{

}


void
stress_test_int64(bool(*func)(std::vector<int64_t))
{

}

} // namespace algotune
