#ifndef __ODS_ODS_SIMPUSET__
#define __ODS_ODS_SIMPUSET__


#include <optional>
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
	return 0;
}

template<typename T>
bool
SimpUSet<T>::add(T value)
{
	assert(value);
	return false;
}

template<typename T>
std::optional<T>
SimpUSet<T>::remove(T value)
{
	assert(value);
	return std::nullopt;
}

template<typename T>
std::optional<T>
SimpUSet<T>::find(T value)
{
	assert(value);
	return std::nullopt;
}


} // namespace ods


#endif // __ODS_ODS_SIMPUSET__