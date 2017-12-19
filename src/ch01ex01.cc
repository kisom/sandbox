#include <cstdlib>
#include <queue>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

// Solve the following problems by reading a text file one line at a time
// and performing operations on each line in the appropriate data structure(s).
// Your implementations should be fast enough that even files containing
// a million lines can be processed in a few seconds.


// Read the input one line at a time and then write the lines out in
// reverse order, so that the last input line is printed first, then the
// second last input line, and so on.
static void
problem1(const char *path)
{
	return;
}


// main should just execute the problems in sequence.
int
main(int argc, char *argv[])
{
	if (argc != 2) {
		cerr << "No input file specified, exiting." << endl;
		exit(1);
	}
	
	problem1(argv[1]);
}