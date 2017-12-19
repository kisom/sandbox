#ifndef __ODS_SIMPLIST__
#define __ODS_SIMPLIST__

#include "list.h"

namespace ods {

	template <typename T>
	    class SimpList : public List {
	public:
		SimpList();
		std::size_t size(void);
		T get(std::size_t);
		T set(std::size_t, T);
		void add(std::size_t, T);
		T remove(std::size_t);
	private:
		T		arr[];
		std::size_t	cap;
		std::size_t	len;
	};

} // end namespace ods
#endif
