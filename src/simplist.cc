#include <ods/simplist.h>
#include <cassert>
#include <cstdlib>
#include <stdexcept>

constexpr	std::size_t DEFAULT_SIZE =	8;

namespace ods {

template<typename T>
SimpList<T>::SimpList()
{
	arr = new T[DEFAULT_SIZE];
	cap = 0;
	len = 0;
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
