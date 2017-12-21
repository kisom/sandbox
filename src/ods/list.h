#ifndef __ODS_ODS_LIST__
#define __ODS_ODS_LIST__

#include <cstdlib>

namespace ods {

// Lists are sequences of values.
template<typename T>
class List {
public:
	virtual ~List(void) {};
	virtual std::size_t size(void) =0;
	virtual T get(std::size_t) =0;
	virtual T set(std::size_t, T) =0;
	virtual void add(std::size_t, T) =0;
	virtual T remove(std::size_t) =0;
};

} // end namespace ods

#endif
