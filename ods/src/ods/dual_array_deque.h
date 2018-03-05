#ifndef __ODS_ODS_DUAL_ARRAY_DEQUE__
#define __ODS_ODS_DUAL_ARRAY_DEQUE__

#include <ods/array.h>
#include <ods/array_stack.h>
#include <iostream>

namespace ods {


template<typename T>
class DualArrayDeque {
public:
	DualArrayDeque(int len) : front(ArrayStack<T>(len)), back(ArrayStack<T>(len)) {}
	int size(void) { return front.size() + back.size(); }
	int cap(void)  { return front.cap() + back.cap(); }
	T get(int i) {
		if (i < front.size()) {
			return front.get(front.size() - i - 1);
		}
		else {
			return back.get(i - front.size());
		}
	}
	T set(int i, T x) {
		if (i < front.size()) {
			return front.set(front.size() - i - 1, x);
		}
		else {
			return back.set(i - front.size());
		}
	}

	void add(int i, T x) {
		if (i < front.size()) {
			front.add(front.size() - i, x);
		}
		else {
			back.add(i - front.size(), x);
		}
		balance();
	}

	T remove(int i) {
		T x;
		if (i < front.size()) {
			x = front.remove(front.size() - i - 1);
		}
		else {
			x = back.remove(i - front.size());
		}
		balance();
		return x;
	}

private:
	ArrayStack<T> front;
	ArrayStack<T> back;

	static inline int max(int x, int y) {
		return x > y ? x : y;
	}

	void balance(void) {
		if ((3 * front.size() < back.size()) ||
		    (3 * back.size()  < front.size())) {
			std::cerr << "rebalancing\n";
			auto n = front.size();
			auto nf = n / 2;
			Array<T> af(max(2*nf, 1));
			for (int i = 0; i < nf; i++) {
				af[nf-i-1] = this->get(i);
			}

			auto nb = n - nf;
			Array<T> ab(max(2*nb, 1));
			for (int i = 0; i < nb; i++) {
				ab[i] = this->get(nf+i);
			}

			ArrayStack<T>	fas(af, nf);
			ArrayStack<T>	bas(ab, nb);
			this->front = fas;
			this->back  = bas;
		}
	}
};

} // namespace ods

#endif // __ODS_ODS_DUAL_ARRAY_DEQUE__
