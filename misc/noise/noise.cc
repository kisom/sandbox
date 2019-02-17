#include <math.h>
#include <stdint.h>

#include "noise.h"
#include "util.h"


// C++ conversion of Ken Perlin's improved noise generator from
// https://mrl.nyu.edu/~perlin/noise/


static double
grad(int hash, double x, double y, double z)
{
	int	h = hash & 15;
	double	u = h < 8 ? x : y;
	double	v = h < 4 ? y : h == 12 || h == 14 ? x : z;

	return	((h & 1) == 0 ? u : -u) + ((h & 2) == 0 ? v : -v);
}


static double
lerp(double t, double a, double b)
{
	return a + t * (b - a);
}


static double
fade(double t)
{
	return t * t * t * (t * (t * 6 - 15) + 10);
}


NoiseGenerator::NoiseGenerator()
{
	// Generate a random permutation of 256 values from 0 to 255
	// inclusive. This is generated by setting each value in the
	// array to its index value, then putting it through a
	// Fisher-Yates shuffle.
	for (int i = 0; i < 256; i++) {
		perm[i] = i;
	}

	for (int i = 255; i > 0; i--) {
		int j = random() % (i + 1);
		swap_u8(perm[i], perm[j]);
	}

	for (int i = 0; i < 256; i++) {
		p[i] = perm[i];
		p[i+256] = perm[i];
	}
}


double
NoiseGenerator::sample(double x, double y, double z)
{
	uint16_t	ux = (uint16_t)floor(x) & 255;
	uint16_t	uy = (uint16_t)floor(y) & 255;
	uint16_t	uz = (uint16_t)floor(z) & 255;
	double		u, v, w;
	uint16_t	a, aa, ab, b, ba, bb;

	x -= floor(x);
	u = fade(x);

	y -= floor(y);
	v = fade(y);

	z -= floor(z);
	w = fade(z);

	a = p[ux] + uy;
	aa = p[a] + uz;
	ab = p[a + 1] + uz;

	b = p[ux + 1] + uy;
	ba = p[b] + uz;
	bb = p[b + 1] + uz;

	return lerp(w, lerp(v, lerp(u, grad(p[aa], x, y, z),
					grad(p[ba], x-1, y, z)),
				lerp(u, grad(p[ab], x, y-1, z),
					grad(p[bb], x-1, y-1, z))),
		       lerp(v, lerp(u, grad(p[aa+1], x, y, z-1),
				       grad(p[ba+1], x-1, y, z-1)),
			       lerp(u, grad(p[ab+1], x, y-1, z-1),
				       grad(p[bb+1], x-1, y-1, z-1))));
}
