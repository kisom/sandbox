#ifndef __BLUEPILL_H__
#define __BLUEPILL_H__

#include "bluepill/gpio.h"
#include "bluepill/rcc.h"

static inline void
delay(unsigned long ms)
{
	for (unsigned long i = 0; i < ms; ++i) __asm__("nop");
}

#endif // __BLUEPILL_H__
