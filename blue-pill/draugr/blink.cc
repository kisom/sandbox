#include "bluepill.h"

#define LED_PIN	13

void reset_handler(void);

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
	initial_sp_value : (unsigned int *)0x100,
	reset : reset_handler,
};

static inline void
delay(unsigned long ms)
{
	for (unsigned long i = 0; i < ms; ++i) __asm__("nop");
}

// void __attribute__ ((weak, naked)) reset_handler(void) {
void reset_handler() {
    *RCC |= (1 << 4); /* enable port C clock */
    GPIO_C->output_mode(LED_PIN, OUTPUT_GPP, OUTPUT_MAX_2MHZ);

    while(1) {
	    GPIO_C->clear_pin(LED_PIN);
	    delay(1000000);
	    GPIO_C->set_pin(LED_PIN);
	    delay(100000);
	    GPIO_C->clear_pin(LED_PIN);
	    delay(100000);
	    GPIO_C->set_pin(LED_PIN);
	    delay(100000);
    }
}

