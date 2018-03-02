Write You a Forth, 0x07
-----------------------

:date: 2018-03-01 19:31
:tags: wyaf, forth

At this point, I've finished most of the nucleus layer. All that's left to
implement are ``EXIT``, ``I``, and ``J`` --- the first requires better
execution support, which I'll talk about at the end. The other two, I'm not so
sure about yet.

However, I made some large changes, so let's dive in. Here's the new Linux
definitions file::

        #ifndef __KF_LINUX_DEFS_H__
        #define __KF_LINUX_DEFS_H__

        #include <stddef.h>
        #include <stdint.h>

        typedef int32_t KF_INT;
        typedef uint32_t KF_UINT;
        typedef int64_t	KF_LONG;

        typedef uintptr_t KF_ADDR;
        constexpr uint8_t STACK_SIZE = 128;
        constexpr size_t ARENA_SIZE = 65535;

        #endif

I've also updated the main ``defs.h`` file to move some constants there::

        #ifndef __KF_DEFS_H__
        #define __KF_DEFS_H__

        #ifdef __linux__
        #include "linux/defs.h"
        #else
        typedef int KF_INT;
        typedef long KF_LONG;
        constexpr uint8_t STACK_SIZE = 16;
        #endif

        constexpr size_t	MAX_TOKEN_LENGTH = 16;
        constexpr size_t dshift = (sizeof(KF_INT) * 8) - 1;

        static inline KF_INT
        mask(size_t bits)
        {
                KF_INT m = 0;

                for (size_t i = 0; i < bits; i++) {
                        m += 1 << i;
                }
                
                return m;
        }

        #endif // __KF_DEFS_H__

Addresses
^^^^^^^^^

The first major change is the addition of the ``KF_ADDR`` type. This is needed
to implement the memory manipulation words. I've added some additional utility
functions for pushing and popping addresses from the data stack; they're stored
as double numbers::

    static bool
    pop_addr(System *sys, KF_ADDR *a)
    {
            KF_LONG	b;
            if (!pop_long(sys, &b)) {
                    // Status is already set.
                    return false;
            }

            *a = static_cast<KF_ADDR>(b);
            sys->status = STATUS_OK;
            return true;
    }

    static bool
    push_addr(System *sys, KF_ADDR a)
    {
            KF_LONG	b = static_cast<KF_LONG>(a);
            if (!push_long(sys, b)) {
                    // Status is already set.
                    return false;
            }

            sys->status = STATUS_OK;
            return true;
    }

Now I can actually implement ``!`` and so forth::

        static bool
        store(System *sys)
        {
                KF_ADDR	a = 0; // address
                KF_INT	b = 0; // value
                KF_LONG	c = 0; // temporary

                if (!pop_long(sys, &c)) {
                        sys->status = STATUS_STACK_UNDERFLOW;
                        return false;
                }
                a = static_cast<KF_ADDR>(c);
                
                if (!sys->dstack.pop(&b)) {
                        sys->status = STATUS_STACK_UNDERFLOW;
                        return false;
                }

                *((KF_INT *)a) = b;
                sys->status = STATUS_OK;
                return true;
        }

There's definitely a sense of finangling here.

The return stack
^^^^^^^^^^^^^^^^

The ``>R`` series of words requires a 