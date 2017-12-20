#ifndef __ODS_QUEUE__
#define __ODS_QUEUE__


// Dequeue represents a collection of elements to which we can add elements
// and remove the next element.
template<typename T>
class Dequeue {
public:
	virtual void add_first(T);
	virtual void add_last(T);
	virtual T remove_first(void);
	virtual T remove_last(void);
	virtual bool empty(void);
};
#endif