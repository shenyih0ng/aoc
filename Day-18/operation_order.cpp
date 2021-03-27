#include <stack>
#include <iostream>
#include <boost/multiprecision/cpp_int.hpp>

#include <aocdefault.h>

using namespace std;
using namespace boost::multiprecision;

template<typename T>
ostream& operator<< (ostream& out, stack<T>& s) {
	stack<T> copy = s;
	out << "----T" << endl;
	while (!copy.empty()) {
		out << copy.top() << endl;
		copy.pop();
	}
	out << "----B" << endl;

	return out;
}

bool is_digit (char c) {
	return (int) c >= 48 && (int) c <= 57;
}

/*
 * Used for Part 1 evaluation
 *
 */
bool p1_is_higher_p (char qOp, char rOp) {
	if (qOp == '(') {
		return false;
	}
	return true;
}

/*
 * Used for Part 2 evaluation
 *
 */
bool p2_is_higher_p (char qOp, char rOp) {
	if (qOp == rOp) {
		return true;
	} else if (qOp == '+') {
		return true;
	} else {
		return false;
	}
}

cpp_int eval (string ex) {
	stack<cpp_int> val_stack;

	string val_str;
	stringstream ex_stream(ex);
	while (getline(ex_stream, val_str, ' ')) {
		cpp_int n_val;
		if (val_str == "+" || val_str == "*") {
			cpp_int val_1 = val_stack.top();
			val_stack.pop();

			cpp_int val_2 = val_stack.top();
			val_stack.pop();
			
			if (val_str == "+") {
				n_val = val_1 + val_2;
			} else {
				n_val = val_1 * val_2;
			}
		} else {
			n_val.assign(val_str);
		}

		val_stack.push(n_val);
	}

	return val_stack.top();
}

/*
 * Convert infix expression to postfix
 * - caveat: p+ > p*
 */
string to_postfix(string ex, bool (*p_compare)(char, char)) {
	string postfix;
	stack<char> operators;
	
	string operand;	
	for (int idx = 0; idx < ex.size(); idx++) {
		char c = ex[idx];
		if (c == ' ') {continue;}

		if (is_digit(c)) {
			operand += c;
		} else {
			if (operand != "") {
				postfix += operand + ' ';
				operand.clear();
			}

			if (c == ')') {
				while(operators.top() != '(') {
					postfix.push_back(operators.top());
					postfix += ' ';
					operators.pop();
				}
				operators.pop(); // pop the open parenthesis
			} else if (c == '(') {
				// highest precedence -> auto push to stack
				operators.push(c);
			} else {
				// check if prev operator has a higher precendence
				if (!operators.empty()) {
					char poperator = operators.top();
					if ((*p_compare)(poperator, c)) {
						operators.pop();
						postfix.push_back(poperator);
						postfix += ' ';
					}
				}
				operators.push(c);
			}	
		}
	}

	if (operand != "") {
		postfix += operand + ' ';
	}

	while (!operators.empty()) {
		postfix.push_back(operators.top());
		postfix += ' ';
		operators.pop();
	}
	
	return postfix;	
}

int main (int argc, char* argv[]) {
	ifstream input_stream(get_input_file_path(argc, argv));
	
	if (input_stream.is_open()) {
		cpp_int p1_val = 0;
		cpp_int p2_val = 0;
		string eq;
		while (getline(input_stream, eq)) {
			string pf_p1 = to_postfix(eq, p1_is_higher_p);
			string pf_p2 = to_postfix(eq, p2_is_higher_p);

			p1_val += eval(pf_p1);
			p2_val += eval(pf_p2);

		}

		cout << "p1: " << p1_val << endl;
		cout << "p2: " << p2_val << endl;
	}
}	
