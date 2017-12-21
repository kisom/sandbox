#ifndef __ODS_ODS_SIMPLIST__
#define __ODS_ODS_SIMPLIST__

#include <ods/list.h>
#include <cassert>
#include <cstdlib>
#include <stdexcept>

namespace ods {

template<typename T>
class SimpList : List<T> {
public:
	SimpList();
	~SimpList(void);
	std::size_t size(void);
	T get(std::size_t);
	T set(std::size_t, T);
	void add(std::size_t, T);
	T remove(std::size_t);
private:
	T		*arr;
	std::size_t	 cap;
	std::size_t	 len;

	const std::size_t DEFAULT_SIZE = 8;
};

template<typename T>
SimpList<T>::SimpList()
{
	arr = new T[DEFAULT_SIZE];
	cap = DEFAULT_SIZE;
	len = 0;
}

template<typename T>
SimpList<T>::~SimpList()
{
	delete this->arr;
	this->cap = 0;
	this->len = 0;
}


template<typename T>
std::size_t
SimpList<T>::size(void)
{
	assert(len <= cap);
	return this->len;
}


template <typename T>
T
SimpList<T>::get(std::size_t i)
{
	if (i >= this->len) {
		throw std::invalid_argument("index out of range");
	}

	return this->arr[i];
}


template <typename T>
T
SimpList<T>::set(std::size_t i, T value)
{
	if (i >= this->len) {
		throw std::invalid_argument("index out of range");
	}
	// check size, grow as needed
	// simple case: check append
	// complex case: insertion
	return value;
}


template <typename T>
void
SimpList<T>::add(std::size_t i, T value)
{
	if (i >= this->len) {
		throw std::invalid_argument("index out of range");
	}
	assert(value);

	return;
}


template <typename T>
T
SimpList<T>::remove(std::size_t i)
{
	if (i >= this->len) {
		throw std::invalid_argument("index out of range");
	}

	return this->arr[i];
}

} // end namespace ods

#endif
