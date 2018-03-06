#ifndef __BLUEPILL_H__
#define __BLUEPILL_H__

#include <stdint.h>

#define PORTA	(volatile unsigned int *)0x40010800
#define PORTB	(volatile unsigned int *)0x40010c00
#define PORTC	(volatile unsigned int *)0x40011000
#define PORTD	(volatile unsigned int *)0x40011400
#define PORTE	(volatile unsigned int *)0x40011800

#define RCC	(volatile unsigned int *)0x40021000

/*
 
	40011000	CRL
	40011004	CRH
	40011008	IDR
	4001100c	ODR
	40011010	BRR
	40011012	LCKR

 */
typedef struct _GPIO {
	uint32_t	CRL;	/* configuration register low */
	uint32_t	CRH;	/* configuration register high */
	uint32_t	IDR;	/* input data register */
	uint32_t	ODR;	/* output data register */
	uint32_t	BSRR;	/* bit set / reset register */
	uint16_t	BRR;	/* reset register */
	uint32_t	LCKR;	/* locking register */
} GPIO;

GPIO	*GPIO_A	 = (GPIO *)PORTA;
GPIO	*GPIO_B  = (GPIO *)PORTB;
GPIO	*GPIO_C  = (GPIO *)PORTC;
GPIO	*GPIO_D  = (GPIO *)PORTD;
GPIO	*GPIO_E  = (GPIO *)PORTE;

const uint32_t	OUTPUT_MAX_2MHZ = 2;
const uint32_t	OUTPUT_MAX_10MHZ = 1;
const uint32_t	OUTPUT_MAX_50MHZ = 3;
const uint32_t	OUTPUT_GPP = 0;
const uint32_t	OUTPUT_GOD = 1;
const uint32_t	OUTPUT_APP = 2;
const uint32_t	OUTPUT_AOD = 3;

void	set_pin(GPIO *gpio, uint32_t pin) { gpio->BSRR |= ((1 << pin) << 16); }
void	clear_pin(GPIO *gpio, uint32_t pin) { gpio->BSRR |= (1 << pin); }
void	output_mode(GPIO *gpio, uint32_t pin, uint32_t mode, uint32_t max) {
	if (pin > 7) {
		pin = ((pin - 8) * 4);
		gpio->CRH |= (max << pin);
		gpio->CRH |= (mode << (pin + 2));
	}
	else {
		pin *= 4;
		gpio->CRL |= (max << pin);
		gpio->CRL |= (mode << (pin + 2));
	}
}


#endif /* __BLUEPILL_H__ */

