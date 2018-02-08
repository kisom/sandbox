#ifndef __ODS_ODS_ARRAY__
#define __ODS_ODS_ARRAY__

#include <cassert>
#include <cstdint>

namespace ods {

template<typename T>
class Array {
public:
	T	*a;
	int	 length;

	Array(int len) : a(new T[len]), length(len) {}

	T& operator[](int i) {
		assert(i >= 0 && i < length);
		return a[i];
	}

	Array<T>& operator=(Array<T> &rhs) {
		if (a != nullptr) {
			delete[] a;
		}
		
		a = rhs.a;
		
		// I don't understand why this is done, but it's how the book defines it.
		rhs.a = nullptr;

		length = rhs.length;

		// I put this in to prevent segfaults.
		rhs.length = 0;
		return *this;
	}
};

} // namespace ods

#endif // __ODS_ODS_ARRAY__
