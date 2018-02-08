#include <iostream>
#include <ods/array.h>
using namespace std;

int
main(void)
{
	Array<int>	a(2);
	Array<int>	b(5);;
	cout << "a[0] " << a[0] << endl;
	
	b = a;
	cout << "a[0] " << a[0] << endl;
}