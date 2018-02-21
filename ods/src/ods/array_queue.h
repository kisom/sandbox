#ifndef __ODS_ODS_ARRAY_QUEUE__
#define __ODS_ODS_ARRAY_QUEUE__

#include <ods/array.h>

namespace ods {


template<typename T>
class ArrayQueue {
public:
	ArrayQueue(int nlen) : a(Array<T>(nlen)), j(0), n(0) {}
	int size(void) { return n; }
	int cap(void)  { return a.length; }

	static inline int max(int x, int y) {
		return x > y ? x : y;
	}

	bool add(T x) {
		if (n + 1 > a.length) {
			resize();
		}

		a[(j+n) % a.length] = x;
		n++;
		return true;
	}

	T remove() {
		T x = a[j];
		j = (j+1) % a.length;
		n--;

		if (a.length >= 3*n) {
			resize();
		}

		return x;
	}

	void resize(void) {
		Array<T> b(max(2 * n, 1));
		for (int k = 0; k < n; k++) {
			b[k] = a[(j+k) % a.length];
		}

		j = 0;
		a = b;
	}
private:
	Array<T>	a;
	int		j;
	int		n;
};

} // namespace ods

#endif // __ODS_ODS_ARRAY_QUEUE__