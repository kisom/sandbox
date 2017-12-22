#ifndef __ODS_ODS_SIMPSSET__
#define __ODS_ODS_SIMPSSET__


#include <optional>
#include <ods/simplist.h>
#include <ods/sset.h>

namespace ods {
template<typename T>
class SimpSSet : public SSet<T> {
public:
	SimpSSet(void);
	~SimpSSet(void);
	std::size_t size(void);
	bool add(T);
	std::optional<T> remove(T);
	std::optional<T> find(T);
private:
	SimpList<T>	list;
};

template<typename T>
SimpSSet<T>::SimpSSet()
{
}


template<typename T>
SimpSSet<T>::~SimpSSet()
{
}

template<typename T>
std::size_t
SimpSSet<T>::size()
{
	return this->list.size();
}

template<typename T>
bool
SimpSSet<T>::add(T value)
{
	for (std::size_t i = 0; i < this->list.size(); i++) {
		if (this->list.get(i) == value) {
			return false;
		}
	}
	
	for (std::size_t i = 0; i < this->list.size(); i++) {
		if (this->list.get(i) < value) {
			continue;
		}
		this->list.add(i, value);
		return true;
	}
	
	this->list.add(this->list.size(), value);
	return true;
}

template<typename T>
std::optional<T>
SimpSSet<T>::remove(T value)
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
SimpSSet<T>::find(T value)
{
	for (std::size_t i = 0; i < this->list.size(); i++) {
		if (this->list.get(i) == value) {
			return this->list.get(i);
		}
	}
	
	return std::nullopt;
}


} // namespace ods


#endif // __ODS_ODS_SIMPSSET__