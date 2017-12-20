#include <ods/simplist.h>
#include <cassert>
#include <cstdlib>
#include <stdexcept>

constexpr	std::size_t DEFAULT_SIZE =	8;

namespace ods {

template<typename T>
SimpList<T>::SimpList()
{
	this->arr = new T[DEFAULT_SIZE];
	this->cap = 0;
	this->len = 0;
}


template <typename T>
std::size_t
SimpList<T>::size(void)
{
	assert(len <= cap);
	return this->len();
}


template <typename T>
T
SimpList<T>::get(std::size_t i)
{
	i = 0;
	throw std::invalid_argument("invalid argument");
}


template <typename T>
T
SimpList<T>::set(std::size_t, T)
{
	throw std::invalid_argument("invalid argument");
}


template <typename T>
void
SimpList<T>::add(std::size_t, T)
{
	return;
}


template <typename T>
T
SimpList<T>::remove(std::size_t)
{
	throw std::invalid_argument("invalid argument");
}

} // end namespace ods
