#ifndef __BLUEPILL_GPIO_H__
#define __BLUEPILL_GPIO_H__

#include <stdint.h>
#include "rcc.h"

constexpr volatile unsigned int	*PORTA = reinterpret_cast<volatile unsigned int *>(0x40010800);
constexpr volatile unsigned int	*PORTB = reinterpret_cast<volatile unsigned int *>(0x40010c00);
constexpr volatile unsigned int	*PORTC = reinterpret_cast<volatile unsigned int *>(0x40011000);
constexpr volatile unsigned int	*PORTD = reinterpret_cast<volatile unsigned int *>(0x40011400);
constexpr volatile unsigned int	*PORTE = reinterpret_cast<volatile unsigned int *>(0x40011800);

class GPIO {
public:
	void	pin_set(uint32_t pin) { this->BSRR |= ((1 << pin) << 16); }
	void	pin_clear(uint32_t pin) { this->BSRR |= (1 << pin); }
	void	pin_mode(uint32_t pin, bool output, uint32_t mode, uint32_t max) {
		if (output) {
			if (pin > 7) {
				pin = ((pin - 8) * 4);
				this->CRH |= (max << pin);
				this->CRH |= (mode << (pin + 2));
			}
			else {
				pin *= 4;
				this->CRL |= (max << pin);
				this->CRL |= (mode << (pin + 2));
			}
		}
	}

	uint32_t	port_index(void) {
		/* Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn. */
		return (((unsigned int)this) - ((unsigned int)PORTA)) >> 10;
	}

	void enable_clock(void) {
		*RCC |= (1 << (this->port_index() + 2));
	}


private:
	uint32_t	CRL;	/* configuration register low */
	uint32_t	CRH;	/* configuration register high */
	uint32_t	IDR;	/* input data register */
	uint32_t	ODR;	/* output data register */
	uint32_t	BSRR;	/* bit set / reset register */
	uint16_t	BRR;	/* reset register */
	uint32_t	LCKR;	/* locking register */
};

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



#endif // __BLUEPILL_GPIO_H__
