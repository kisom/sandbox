#include "bluepill.h"

#define LED_PIN	13

// void __attribute__ ((weak, naked)) reset_handler(void) {
int
main() {
    GPIO_C->enable_clock();
    GPIO_C->pin_mode(LED_PIN, true, OUTPUT_GPP, OUTPUT_MAX_2MHZ);

    while(1) {
	    GPIO_C->pin_clear(LED_PIN);
	    delay(1000000);
	    GPIO_C->pin_set(LED_PIN);
	    delay(100000);
	    GPIO_C->pin_clear(LED_PIN);
	    delay(100000);
	    GPIO_C->pin_set(LED_PIN);
	    delay(100000);
    }
}

