#ifndef __KF_STACK_H__
#define __KF_STACK_H__

#include "defs.h"

template <typename T>
class Stack {
public:
        bool   push(T val);
	bool   pop(T *val);
	bool   peek(T *val);
	bool   get(size_t, T &);
	bool   remove(size_t, T *);
	size_t size(void) { return this->arrlen; }
	void   clear(void) { this->arrlen = 0; }
private:
	T arr[STACK_SIZE];
	size_t arrlen;
};

// push returns false if there was a stack overflow.
template <typename T>
bool
Stack<T>::push(T val)
{
	if ((this->arrlen + 1) > STACK_SIZE) {
		return false;
	}

	this->arr[this->arrlen++] = val;
	return true;
}

// pop returns false if there was a stack underflow.
template <typename T>
bool
Stack<T>::pop(T *val)
{
	if (this->arrlen == 0) {
		return false;
	}

	*val = this->arr[this->arrlen - 1];
	this->arrlen--;
	return true;
}

// peek returns false if there was a stack underflow.
template <typename T>
bool
Stack<T>::peek(T *val)
{
	if (this->arrlen == 0) {
		return false;
	}

	*val = this->arr[this->arrlen - 1];
	return true;
}


// get returns false on invalid bounds.
template <typename T>
bool
Stack<T>::get(size_t i, T &val)
{
	if (i > this->arrlen) {
		return false;
	}

	val = this->arr[i];
	return true;
}

// remove returns false on invalid bounds
template <typename T>
bool
Stack<T>::remove(size_t i, T *val)
{
	if (i > this->arrlen) {
		return false;
	}

	*val = this->arr[i];
	for (; i < (arrlen - 1); i++) {
		this->arr[i] = this->arr[i+1];
	}
	arrlen--;
	return true;
}

#endif // __KF_STACK_H__