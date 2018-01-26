#ifndef __ODS_ODS_SIMPLIST__
#define __ODS_ODS_SIMPLIST__

#include <ods/list.h>
#include <cassert>
#include <cstdlib>
#include <stdexcept>

namespace ods {

template<typename T>
class SimpList : public List<T> {
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
	void		 grow();
	void		 shrink();

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

	T old = this->arr[i];
	this->arr[i] = value;
	return old;
}


template <typename T>
void
SimpList<T>::add(std::size_t i, T value)
{
	if (i > this->len) {
		throw std::invalid_argument("index out of range");
	}
	assert(value);
	
	// check size, grow as needed
	if (len == (cap - 1)) {
		this->grow();
	}
	
	// simple case: check append
	if (i == len) {
		this->arr[len++] = value;
		return;
	}
	
	// complex case: insertion
	for (std::size_t j = this->len; j > i; j--) {
		this->arr[j] = this->arr[j-1];
	}
	this->arr[i] = value;
	this->len++;
}


template <typename T>
T
SimpList<T>::remove(std::size_t i)
{
	if (i > this->len) {
		throw std::invalid_argument("index out of range");
	}
	
	T old = this->arr[i];
	
	if (i == this->len) {
		this->len--;
		return old;
	}
	
	for (std::size_t j = i; j < this->len; j++) {
		this->arr[j] = this->arr[j+1];
	}
	this->len--;

	return old;
}

template<typename T>
void
SimpList<T>::grow()
{
	std::size_t	 new_cap = this->cap * 2;
	T		*new_arr = new T[new_cap];
	
	for (std::size_t i = 0; i < this->len; i++) {
		new_arr[i] = this->arr[i];
	}
	
	delete this->arr;
	this->arr = new_arr;
	this->cap = new_cap;
}

} // end namespace ods

#endif
