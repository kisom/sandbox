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

void __attribute__ ((weak, naked)) reset_handler(void) {
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

__attribute__ ((section(".vectors")))
struct {
    unsigned int *initial_sp_value;
    void (*reset)(void);
    void (*nmi)(void);
    void (*hard_fault)(void);
    void (*memory_manage_fault)(void);
    void (*bus_fault)(void);
    void (*usage_fault)(void);
    void (*reserved_x001c[4])(void);
    void (*sv_call)(void);
    void (*debug_monitor)(void);
    void (*reserved_x0034)(void);
    void (*pend_sv)(void);
    void (*systick)(void);
    void (*irq[68])(void);
} vector_table = {
    .reset = reset_handler,
};
