#ifndef __KF_STACK_H__
#define __KF_STACK_H__

/* data stack interaction */
bool	dstack_pop(KF_INT *);
bool	dstack_push(KF_INT);
bool	dstack_get(size_t, KF_INT *);
size_t	dstack_size(void);
void	dstack_clear(void);

/* return stack interaction */
bool	rstack_pop(KF_ADDR *);
bool	rstack_push(KF_ADDR);
bool	rstack_get(size_t, KF_ADDR *);
size_t	rstack_size(void);
void	rstack_clear(void);

#endif /* __KF_STACK_H__ */
