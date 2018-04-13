#ifndef __LIB_ALGOTUNE__
#define __LIB_ALGOTUNE__

#include <vector>

namespace algotune {

int64_t			display_step = 1000;

void			reseed(void);

int			rand_int(int low, int high);
int64_t			rand_int64(int64_t low, int64_t high);

std::vector<int>	gen_int_vector(int size, int low, int high);
std::vector<int64_t>	gen_int64_vector(int64_t size, int64_t low, int64_t high);

int64_t			stress_test_int(bool(*func)(std::vector<int>&), int size, int low, int high);
int64_t			stress_test_int64(bool(*func)(std::vector<int64_t>&), int64_t size, int64_t low, int64_t high);

} // namespace algotune


#endif // __LIB_ALGOTUNE__
