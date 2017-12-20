#include <cassert>
#include <ods/simplist.h>
using namespace std;


static void
check_simplist(void)
{
	ods::SimpList<int>	sl;

	sl.add(0, 1);
	sl.add(1, 2);
	sl.add(2, 3);
	assert(sl.size() == 3);
}


int
main(void)
{
	check_simplist();
}
