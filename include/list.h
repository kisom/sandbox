#ifndef __ODS_LIST__
#define __ODS_LIST__


#include <cstddef>

namespace ods {

// Lists are sequences of values.
	template <typename T>
	    class List {
	public:
		virtual std::size_t size(void);
		virtual T get(std::size_t);
		virtual T set(std::size_t, T);
		virtual void add(std::size_t, T);
		virtual T remove(std::size_t);
	};

} // end namespace ods

#endif
