#include <boost/lexical_cast.hpp>
#include <boost/multiprecision/cpp_int.hpp>
#include <iostream>
#include <stack>

#include <aocdefault.h>

using namespace std;
using namespace boost::multiprecision;

/*
 * A wacky implementation of infix expression evaluation (*purely for
 * learning/fun sake)
 *
 * - It does not account for operator precedence (treats every operator as same
 * precedence)
 * - Does not convert infix expression into postfix/prefix as a intermediate
 * form for calcuation
 *
 * WILL ONLY WORK FOR PART 1
 */

bool is_digit(char c) { return (int)c >= 48 && (int)c <= 57; }

// DATA MODELLING

enum OpTypes { op_base, op_operand, op_operator };

struct Op {
        Op() = default;
        ~Op() = default;
        virtual OpTypes get_type() { return op_base; }
};

struct Operand : Op {
        cpp_int data;
        Operand(cpp_int _i) : data(_i) {}
        OpTypes get_type() { return op_operand; }
};

struct Operator : Op {
        char data;
        Operator(char _c) : data(_c) {}
        OpTypes get_type() { return op_operator; }
};

// CORE

void reduce(stack<Op*>& ops_stack, cpp_int operand) {
        if (!ops_stack.empty()) {
                Op* op = ops_stack.top();
                if (op->get_type() == op_operator) {
                        Operator* t_operator = (Operator*)ops_stack.top();
                        ops_stack.pop();  // remove operator
                        Operand* prev_operand = (Operand*)ops_stack.top();
                        ops_stack.pop();  // remove prev operand

                        cpp_int rVal;
                        if (t_operator->data == '+') {
                                rVal = prev_operand->data + operand;
                        } else if (t_operator->data == '*') {
                                rVal = prev_operand->data * operand;
                        }
                        ops_stack.push(new Operand(rVal));

                        delete op;
                        delete prev_operand;
                } else {
                        ops_stack.push(new Operand(operand));
                }
        } else {
                ops_stack.push(new Operand(operand));
        }
}

cpp_int eval(string eq) {
        stack<Op*> ops;

        int idx = 0;
        string operand;
        while (idx < eq.size()) {
                char c = eq[idx];

                if (c == ' ') {  // skip white spaces
                        idx++;
                        continue;
                }

                if (is_digit(c)) {
                        operand += c;
                }

                if (idx == eq.size() - 1 || !is_digit(c)) {
                        if (operand.size() > 0) {
                                cpp_int operand_val(operand);
                                reduce(ops, operand_val);
                                operand.clear();
                        }

                        if (c == ')') {
                                Operand* operand_pval =
                                    (Operand*)ops
                                        .top();  // resulting val in parentheses
                                ops.pop();       // remove resulting val
                                Op* _openingP = ops.top();
                                ops.pop();  // remove opening parentheses
                                reduce(ops, operand_pval->data);

                                delete operand_pval;
                                delete _openingP;
                        } else if (idx != eq.size() - 1) {
                                ops.push((c == '(' || c == ')')
                                             ? new Op()
                                             : new Operator(c));
                        }
                }
                idx++;
        }

        Operand* final_operand = (Operand*)ops.top();
        cpp_int final_val(final_operand->data);
        delete final_operand;

        return final_val;
}

int main(int argc, char* argv[]) {
        const string INPUT_PATH = "./operation_order.txt";

        ifstream input_stream(INPUT_PATH);

        if (input_stream.is_open()) {
                cpp_int accum = 0;
                string eq;
                while (getline(input_stream, eq)) {
                        cpp_int val = eval(eq);
                        accum += val;
                }
                cout << "eval sum: " << accum << endl;
        }

        return 0;
}
