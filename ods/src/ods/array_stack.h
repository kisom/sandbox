#ifndef __ODS_ODS_ARRAY_STACK__
#define __ODS_ODS_ARRAY_STACK__


namespace ods {


template<typename T>
class ArrayStack {
	int size(void) { return static_cast<int>(this->a->size()); }
private:
	Array<T>	a;
	int		n;
}

} // namespace ods

#endif // __ODS_ODS_ARRAY_STACK__