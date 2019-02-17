#include <iostream>
#include "noise.h"

using namespace std;


int
main(void)
{
	NoiseGenerator	noise;

	for (double t = 0; t < 5; t += 0.01) {
		cout << t << "\t" << noise.sample(t, t, t) << endl;
	}
}
