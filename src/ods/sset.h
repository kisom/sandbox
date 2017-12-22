#ifndef __ODS_ODS_SSET__
#define __ODS_ODS_SSET__


#include <cstdlib>
#include <optional>

namespace ods {
template<typename T>
class SSet {
public:
	virtual ~SSet(void) {};
	virtual std::size_t size(void) =0;
	virtual bool add(T) = 0;
	virtual std::optional<T> remove(T) = 0;
	virtual std::optional<T> find(T) = 0;
};
} // namespace ods

#endif // __ODS_ODS_SSET__