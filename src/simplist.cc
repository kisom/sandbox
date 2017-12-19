#include "simplist.h"
#include <cassert>
#include <cstdlib>
#include <stdexcept>

constexpr	std::size_t DEFAULT_SIZE =	8;

namespace ods {

	SimpList<T>::SimpList<T>()
	{
		this->arr = new T[DEFAULT_SIZE];
		this->cap = 0;
		this->len = 0;
	}


	std::size_t
	SimpList<T> size(void)
	{
		std::assert(len <= cap);
		return this->len();
	}

	T
	SimpList<T>::get(std::size_t i)
	{
		throw std::invalid_argument("invalid argument");
	}

	T
	SimpList<T>::set(std::size_t, T)
	{
		throw std::invalid_argument("invalid argument");
	}

	void
	SimpList<T>::add(std::size_t, T)
	{
		return;
	}

	T
	SimpList<T>::remove(std::size_t)
	{
		throw std::invalid_argument("invalid argument");
	}
};
