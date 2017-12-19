#include <iostream>
#include <queue>
#include <stack>
#include <string>
using namespace std;



static void
print_queue(queue<int>& q)
{
	while (!q.empty()) {
		cout << q.front() << " ";
		q.pop();
	}
	cout << endl;
}


int
main(int argc, char *argv[])
{
	stack<int>	s;
	queue<int>	q;

	for (int i = 1; i < argc; i++) {
		string	line(argv[i]);
		int	arg = stoi(line);

		s.push(arg);
	}

	while (!s.empty()) {
		// NB: the pop() interface in the book returns the topmost
		// element while removing it; the C++ STL interface does
		// not, requiring the separate top() and pop() invocations.
		int arg = s.top();
		s.pop();
		q.push(arg);
	}

	print_queue(q);
}
