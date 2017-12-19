#include <iostream>
#include <stack>
#include <string>
using namespace std;

int
main(int argc, char *argv[])
{
	stack<char>	s;
	
	for (int i = 1; i < argc; i++) {
		string	ms(argv[i]);
		bool	matched = true;
		
		for (auto c : ms) {
			switch (c) {
			case '{':
			case '(':
			case '[':
				s.push(c);
				break;
			case '}':
				if (s.top() != '{') {
					cerr << "Saw '" << s.top() << "' but expected '{'." << endl;
					matched = false;
				}
				else {
					s.pop();
				}
				break;
			case ']':
				if (s.top() != '[') {
					cerr << "Saw '" << s.top() << "' but expected '['." << endl;
					matched = false;
				}
				else {
					s.pop();
				}
				break;
			case ')':
				if (s.top() != '(') {
					cerr << "Saw '" << s.top() << "' but expected '('." << endl;
					matched = false;
				}
				else {
					s.pop();
				}
				break;
			default:
				cerr << "Invalid character in string: " << c << endl;
				matched = false;
			}
			
			if (!matched) {
				break;
			}
			
		}
		
		if (!matched) {
			cerr << "'" << ms << "' is not a matched string." << endl;
		}
		else {
			cout << "'" << ms << "' is a matched string." << endl;
		}
	}
}