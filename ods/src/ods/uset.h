#ifndef __ODS_ODS_USET__
#define __ODS_ODS_USET__


#include <cstdlib>
#include <optional>

namespace ods {
template<typename T>
class USet {
public:
	virtual ~USet(void) {};
	virtual std::size_t size(void) =0;
	virtual bool add(T) = 0;
	virtual std::optional<T> remove(T) = 0;
	virtual std::optional<T> find(T) = 0;
};
} // namespace ods

#endif // __ODS_ODS_USET__