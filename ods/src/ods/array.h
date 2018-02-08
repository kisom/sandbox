#ifndef __ODS_ODS_ARRAY__
#define __ODS_ODS_ARRAY__

#include <cstdint>

namespace ods {

template<typename T>
class Array {
public:
	Array(int len) : length(len) { a = new T[length]; }
	T& operator[](int i) {
		assert(i >= 0 && static_cast<std::size_t>(i) < length);
		return a[i];
	}
	Array<T>& operator=(Array<T> &rhs) {
		if (a != nullptr) {
			delete a;
		}
		
		a = rhs.a;
		
		// I don't understand why this is done, but it's how the book defines it.
		rhs.a = nullptr;
		
		// I put this in to prevent segfaults.
		rhs.length = 0;


		length = rhs.length;
		return *this;
	}
	
	std::size_t	size(void) { return length; }
private:
	T		*a;
	std::size_t	 length;
};

} // namespace ods

#endif // __ODS_ODS_ARRAY__