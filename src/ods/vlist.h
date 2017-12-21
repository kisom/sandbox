#ifndef __ODS_ODS_VLIST__
#define __ODS_ODS_VLIST__


#include <cstdlib>
#include <optional>
#include <vector>
#include <ods/list.h>

namespace ods {

// VList implements a vector-based list interface.
template<typename T>
class VList : public List<T> {
public:
	~VList(void);
	std::size_t size(void);
	T get(std::size_t);
	T set(std::size_t, T);
	void add(std::size_t, T);
	T remove(std::size_t);
private:
	std::vector<T> vec;
};

template<typename T>
VList<T>::~VList()
{
	this->vec.clear();
}

template<typename T>
std::size_t
VList<T>::size()
{
	return this->vec.size();
}

template<typename T>
T
VList<T>::get(std::size_t i)
{
	return this->vec.at(i);
}

template<typename T>
T
VList<T>::set(size_t i, T value)
{
	auto p = this->vec.begin() + i;
	T old = this->vec.at(i);
	this->vec.erase(p);
	this->vec.insert(p, value);
	return old;
}

template<typename T>
void
VList<T>::add(std::size_t i, T value)
{
	auto p = this->vec.begin() + i;
	this->vec.insert(p, value);
}

template<typename T>
T
VList<T>::remove(std::size_t i)
{
	auto removed = this->vec.at(i);
	auto p = this->vec.begin() + i;
	this->vec.erase(p);
	return removed;
}

} // namespace ods


#endif // __ODS_ODS_VLIST__