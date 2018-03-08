#include "bluepill.h"

constexpr uint32_t	LED = 13;
constexpr unsigned long	SDELAY = 100000;
constexpr unsigned long LDELAY = (SDELAY * 10) - (3 * SDELAY);

// void __attribute__ ((weak, naked)) reset_handler(void) {
int
main() {
    GPIO_C->enable_clock();
    GPIO_C->pin_mode(LED, true, OUTPUT_GPP, OUTPUT_MAX_2MHZ);

    while (true) {
	    GPIO_C->pin_clear(LED);
	    delay(LDELAY);
	    GPIO_C->pin_set(LED);
	    delay(SDELAY);
	    GPIO_C->pin_clear(LED);
	    delay(SDELAY);
	    GPIO_C->pin_set(LED);
	    delay(SDELAY);
    }
}

