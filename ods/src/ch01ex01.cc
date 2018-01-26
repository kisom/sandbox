#include <cstdlib>
#include <deque>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <stack>
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
	ifstream	ifs(path);
	string		line;
	stack<string>	s;
	
	if (!ifs) {
		return;
	}
	
	while (getline(ifs, line)) {
		s.push(line);
	}
	
	while (!s.empty()) {
		cout << s.top() << endl;
		s.pop();
	}
	
	ifs.close();
	return;
}


// Read the first 50 lines of input and then write them out in reverse
// order. Read the next 50 lines and then write them out in reverse
// order. Do this until there are no more lines left to read, at which
// point any remaining lines should be output in reverse order.
//
// In other words, your output will start with the 50th line, then the
// 49th, then the 48th, and so on down to the first line. This will be
// followed by the 100th line, followed by the 99th, and so on down to
// the 51st line. And so on.
//
// Your code should never have to store more than 50 lines at any given
// time.
static void
problem2(const char *path)
{
	ifstream	ifs(path);
	string		line;
	stack<string>	s;
	bool		stop = false;
	
	if (!ifs) {
		return;
	}
	
	while (!stop) {
		while (s.size() < 50) {
			if (getline(ifs, line)) {
				s.push(line);
			} else {
				stop = true;
				break;
			}
		}
		
		while (!s.empty()) {
			cout << s.top() << endl;
			s.pop();
		}
	}
}


// Read the input one line at a time. At any point after reading the
// first 42 lines, if some line is blank (i.e., a string of length 0), then
// output the line that occured 42 lines prior to that one. For example,
// if Line 242 is blank, then your program should output line 200.
// This program should be implemented so that it never stores more
// than 43 lines of the input at any given time.
static void
problem3(const char *path)
{
	ifstream	ifs(path);
	string		line;
	deque<string>	q;
	
	if (!ifs) {
		return;
	}
	
	while (getline(ifs, line)) {
		if (q.size() > 42) {
			q.pop_back();
		}
		q.push_front(line);
		
		if (line == "") {
			cout << q.back() << endl;
		}
	}
}

// Read the input one line at a time and write each line to the output
// if it is not a duplicate of some previous input line. Take special care
// so that a file with a lot of duplicate lines does not use more memory
// than what is required for the number of unique lines.
static void
problem4(const char *path)
{
 	ifstream	ifs(path);
	string		line;
	set<string>	set;

	if (!ifs) {
		return;
	}
	
	while (getline(ifs, line)) {
		if (set.count(line) == 0) {
			cout << line << endl;
			set.insert(line);
		}
	}
}


// Read the input one line at a time and write each line to the output
// only if you have already read this line before. (The end result is that
// you remove the first occurrence of each line.) Take special care so
// that a file with a lot of duplicate lines does not use more memory
// than what is required for the number of unique lines.
static void
problem5(const char *path)
{
 	ifstream	ifs(path);
	string		line;
	set<string>	set;

	if (!ifs) {
		return;
	}
	
	while (getline(ifs, line)) {
		if (set.count(line) == 1) {
			cout << line << endl;
		} else {
			set.insert(line);
		}
	}
}

// main should just execute the problems in sequence.
int
main(int argc, char *argv[])
{
	int	problem = -1;

	if (argc < 2) {
		cerr << "No input file specified, exiting." << endl;
		exit(1);
	}
	
	if (argc == 3) {
		problem = stoi(string(argv[2]));
	}
	
	if (problem > 0) {
		switch (problem) {
		case 1:
			cout << "*** PROBLEM 1 ***" << endl;
			problem1(argv[1]);
			cout << endl << endl;
			break;
		case 2:
			cout << "*** PROBLEM 2 ***" << endl;
			problem2(argv[1]);
			cout << endl << endl;
			break;
		case 3:
			cout << "*** PROBLEM 3 ***" << endl;
			problem3(argv[1]);
			cout << endl << endl;
			break;
		case 4:
			cout << "*** PROBLEM 4 ***" << endl;
			problem4(argv[1]);
			cout << endl << endl;
			break;
		case 5:
			cout << "*** PROBLEM 5 ***" << endl;
			problem5(argv[1]);
			cout << endl << endl;
			break;
		default:
			cerr << "Unknown problem " << problem << endl;
			exit(1);
		}
		exit(0);
	}
	
	cout << "*** PROBLEM 1 ***" << endl;
	problem1(argv[1]);
	cout << endl << endl;

	cout << "*** PROBLEM 2 ***" << endl;
	problem2(argv[1]);
	cout << endl << endl;

	cout << "*** PROBLEM 3 ***" << endl;
	problem3(argv[1]);
	cout << endl << endl;
	
	cout << "*** PROBLEM 4 ***" << endl;
	problem4(argv[1]);
	cout << endl << endl;

	cout << "*** PROBLEM 5 ***" << endl;
	problem5(argv[1]);
	cout << endl << endl;
}