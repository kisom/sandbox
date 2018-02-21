#ifndef __ODS_ODS_ARRAY_DEQUE_
#define __ODS_ODS_ARRAY_DEQUE_

#include <ods/array.h>

namespace ods {


template<typename T>
class ArrayDeque {
public:
	ArrayDeque(int nlen) : a(Array<T>(nlen)), j(0), n(0) {}
	int size(void) { return n; }
	int cap(void)  { return a.length; }

	static inline int max(int x, int y) {
		return x > y ? x : y;
	}

	inline int index(int i) {
 		auto v = (j+i) % a.length;
		return v;
	}

	T get(int i) {
		return a[index(i)];
	}

	T set(int i, T x) {
		T y = a[index(i)];
		a[index(i)] = x;
		return y;
	}

	void add(int i, T x) {
		if (n + 1 > a.length) {
			resize();
		}

		// if i is in the leftmost half, shift left.
		if (i < n/2) {
			j = (j == 0) ? a.length - 1 : j - 1;
			for (int k = 0; k <= i-1; k++) {
				a[index(k)] = a[index(k+1)];
			}
		}
		// otherwise, shift right.
		else {
			for (int k = n; k > i; k--) {
				a[index(k)] = a[index(k)-1];
			}
		}

		a[index(i)] = x;
		n++;
	}

	void resize(void) {
		Array<T> b(max(2 * n, 1));
		for (int k = 0; k < n; k++) {
			b[k] = a[index(k)];
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

#endif // __ODS_ODS_ARRAY_DEQUE_