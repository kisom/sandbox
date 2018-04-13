#ifndef __LIB_ALGOTUNE__
#define __LIB_ALGOTUNE__

#include <vector>

namespace algotune {

int			rand_int(int low, int high);
int64_t			rand_int(int64_t low, int64_t high);

std::vector<int>	gen_int_vector(int size, int low, int high);
std::vector<int64_t>	gen_int_vector(int size, int low, int high);

void			stress_test_int(bool(*func)(std::vector<int>));
void			stress_test_int64(bool(*func)(std::vector<int64_t));

} // namespace algotune


#endif // __LIB_ALGOTUNE__
