/* based on example from https://github.com/satoshinm/pill_blink */

#include "bluepill.h"
#define LED_PIN	13

static inline void
delay(unsigned long ms)
{
	for (unsigned long i = 0; i < ms; ++i) __asm__("nop");
}

static inline void
led_off() {
	clear_pin(GPIO_C, LED_PIN);
}

static inline void
led_on() {
	set_pin(GPIO_C, LED_PIN);
}

int
main(void)
{
    *RCC |= (1 << 4); /* enable port C clock */
    output_mode(GPIO_C, LED_PIN, OUTPUT_GPP, OUTPUT_MAX_2MHZ);

    while(1) {
	    led_off();
	    delay(1000000);
	    led_on();
	    delay(100000);
	    led_off();
	    delay(100000);
	    led_on();
	    delay(100000);
    }
}

