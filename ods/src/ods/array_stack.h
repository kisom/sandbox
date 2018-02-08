#ifndef __ODS_ODS_ARRAY_STACK__
#define __ODS_ODS_ARRAY_STACK__

#include <iostream>
#include <ods/array.h>

namespace ods {


template<typename T>
class ArrayStack {
public:
	ArrayStack(int len) : n(0), a(Array<T>(len)) {}
	int size(void) { return a.length; }
	T get(int i) { return a[i]; }
	T set(int i, T x) { 
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
			std::cout << j << "\t" << i << "\t" << a[j] << std::endl;
			a[j] = a[j+1];
		}
		n--;

		if (a.length > 3*n) {
			resize();
		}

		return x;
	}

	void resize(void) {}
private:
	int		n;
	Array<T>	a;
};

} // namespace ods

#endif // __ODS_ODS_ARRAY_STACK__
