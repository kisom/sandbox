#include <iostream>
#include <vector>

#include "algotune.h"

using namespace std;


static int64_t
max_pairwise_fast(vector<int> &v)
{
	size_t idx1 = 0;
	size_t idx2 = 0;
	auto n = v.size();

	for (size_t i = 1; i < n; i++) {
		if (v[i] > v[idx1]) {
			idx1 = i;
		}
	}

	for (size_t i = 1; i < n; i++) {
		if (i == idx1) continue;
		if ((v[i] > v[idx2]) || (idx2 == idx1)) {
			idx2 = i;
		}
	}

	return 	static_cast<int64_t>(v[idx1]) * static_cast<int64_t>(v[idx2]);
}


static int64_t
max_pairwise_naive(vector<int> &v)
{
	auto n = v.size();
	int64_t best = 0;

	for (size_t i = 0; i < n; i++) {
		for (size_t j = 0; j < n; j++) {
			if (i == j) continue;
			int64_t prod = v[i] * v[j];
			best = prod > best ? prod : best;
		}
	}

	return best;
}


static bool
test_max_pairwise_fast(vector<int> &v)
{
	auto result_fast = max_pairwise_fast(v);
	auto result_naive = max_pairwise_naive(v);

	return result_fast == result_naive;
}


int
main(void)
{
	vector<int> v = algotune::gen_int_vector(10, 1, 10000);
	if (!test_max_pairwise_fast(v)) {
		cerr << "test failed\n";
		return 1;
	}

	algotune::display_step = 100;
	auto iterations = algotune::stress_test_int(test_max_pairwise_fast, 10000, 1, 1000000);
	cout << "Iterations before failure: " << iterations << endl;
	return 0;
}
