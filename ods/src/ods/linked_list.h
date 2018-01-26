#ifndef __ODS_ODS_LINKED_LIST__
#define __ODS_ODS_LINKED_LIST__


#include <cstdlib>
#include <iostream>
#include <ods/list.h>


namespace ods {

template<typename T>
struct Node {
	struct Node	*next;
	T		 value;
};

template<typename T>
class LinkedList : public List<T> {
public:
	LinkedList();
	~LinkedList();
	std::size_t size(void);
	T get(std::size_t);
	T set(std::size_t, T);
	void add(std::size_t, T);
	T remove(std::size_t);
private:
	struct Node<T>	*head;
	std::size_t	 len;
};


template<typename T>
LinkedList<T>::LinkedList() : head(nullptr), len(0) {}

template<typename T>
LinkedList<T>::~LinkedList()
{
	struct Node<T>	*cursor = this->head;
	while (cursor != nullptr) {
		if (this->head != nullptr) {
			cursor = this->head->next;
		}
		delete this->head;
		this->head = cursor;
	}
}


template<typename T>
std::size_t
LinkedList<T>::size()
{
	return this->len;
}


template<typename T>
void
LinkedList<T>::add(std::size_t i, T value)
{
	assert(i <= this->size());
	struct Node<T>	*node = new struct Node<T>;
	node->value = value;
	node->next = nullptr;
	
	auto cursor = this->head;
	if (i == 0) {
		node->next = this->head;
		this->head = node;
	}
	else {
		for (size_t j = 0; j < (i-1); j++) {
			cursor = cursor->next;
		}
	
		if (cursor != nullptr) {
			node->next = cursor->next;
			cursor->next = node;
		}
		else {
			this->head = node;
		}
	}
	
	this->len++;
}


template<typename T>
T
LinkedList<T>::get(std::size_t i)
{
	assert(i < this->size());
	if (i == 0) {
		return this->head->value;
	}
	
	auto cursor = this->head;
	for (size_t j = 0; j < i; j++) {
		cursor = cursor->next;
	}

	return cursor->value;
}

template<typename T>
T
LinkedList<T>::set(std::size_t i, T value)
{
	auto cursor = this->head;
	for (std::size_t j = 0; j < i; j++) {
		cursor = cursor->next;
	}
	
	T prev = cursor->value;
	cursor->value = value;
	return prev;
}

template<typename T>
T
LinkedList<T>::remove(std::size_t i)
{
	if (i == 0) {
		auto old = this->head->value;
		auto target = this->head;
		this->head = this->head->next;
		delete target;
		this->len--;
		return old;
	}
	
	auto cursor = this->head;
	for (std::size_t j = 0; j < (i-1); j++) {
		cursor = cursor->next;
	}
	
	auto target = cursor->next;
	cursor->next = target->next;
	auto old = target->value;
	delete target;
	this->len--;
	return old;
}

} // namespace ods


#endif // __ODS_ODS_LINKED_LIST__