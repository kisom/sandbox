#ifndef __ODS_ODS_SIMPUSET__
#define __ODS_ODS_SIMPUSET__


#include <optional>
#include <ods/simplist.h>
#include <ods/uset.h>

namespace ods {
template<typename T>
class SimpUSet : USet<T> {
public:
	SimpUSet(void);
	~SimpUSet(void);
	std::size_t size(void);
	bool add(T);
	std::optional<T> remove(T);
	std::optional<T> find(T);
private:
	SimpList<T>	list;
};

template<typename T>
SimpUSet<T>::SimpUSet()
{
}


template<typename T>
SimpUSet<T>::~SimpUSet()
{
}

template<typename T>
std::size_t
SimpUSet<T>::size()
{
	return this->list.size();
}

template<typename T>
bool
SimpUSet<T>::add(T value)
{
	for (std::size_t i = 0; i < this->list.size(); i++) {
		if (this->list.get(i) == value) {
			return false;
		}
	}
	
	this->list.add(this->list.size(), value);
	return true;
}

template<typename T>
std::optional<T>
SimpUSet<T>::remove(T value)
{
	for (std::size_t i = 0; i < this->list.size(); i++) {
		if (this->list.get(i) == value) {
			auto removed = this->list.get(i);
			this->list.remove(i);
			return removed;
		}
	}
	
	return std::nullopt;
}

template<typename T>
std::optional<T>
SimpUSet<T>::find(T value)
{
	for (std::size_t i = 0; i < this->list.size(); i++) {
		if (this->list.get(i) == value) {
			return this->list.get(i);
		}
	}
	
	return std::nullopt;
}


} // namespace ods


#endif // __ODS_ODS_SIMPUSET__