#ifndef __ODS_ODS_ARRAY_STACK__
#define __ODS_ODS_ARRAY_STACK__

#include <ods/array.h>

namespace ods {


template<typename T>
class ArrayStack {
public:
	ArrayStack(int len) : n(0), a(Array<T>(len)) {}
	int size(void) { return n; }
	int cap(void)  { return a.length; }
	T get(int i) { return a[i]; }
	T set(int i, T x) {
		if (i > n) {
			n = i+1;
		}
		T y = a[i];
		a[i] = x;
		return y;
	}

	void add(int i, T x) {
		if (n+1 > a.length) {
			resize();
		}

		for (int j = n; j > i; j--) {
			a[j] = a[j-1];
		}

		a[i] = x;
		n++;
	}

	T remove(int i) {
		T x = a[i];
		for (int j = i; j < (n-1); j++) {
			a[j] = a[j+1];
		}
		n--;

		if (a.length > 3*n) {
			resize();
		}

		return x;
	}

	static inline int max(int x, int y) {
		return x > y ? x : y;
	}

	void resize(void) {
		int nlen = max(2 * n, 1);
		Array<T> b(nlen);
		for (int i = 0; i < n; i++) {
			b[i] = a[i];
		}
		a = b;
	}

	T pop(void) {
		return a.remove(n - 1);
	}

	void push(T x) {
		return a.add(n, x);
	}
private:
	int		n;
	Array<T>	a;
};

} // namespace ods

#endif // __ODS_ODS_ARRAY_STACK__
