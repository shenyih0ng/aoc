#include <math.h>
#include <boost/multiprecision/cpp_int.hpp>
#include <unordered_map>

#include <aocdefault.h>

using namespace std;
namespace mp = boost::multiprecision;

#define A 7
#define P 20201227

/**
 * modular arithmetic
 *
 * a mod n = a1 <-> a === a1 (mod n)
 * b mod n = b1 <-> b === b1 (mod n)
 * (ab) mod n = c1 <-> (ab) === c1 (mod n)
 * thus, the problem can be simplified into 7^x mod 20201227
 *
 * baby step giant step (bsgs)
 *
 * A^X % P = B
 * X = Xb + Xgm
 *
 * A^Xb = B(A^(-Xgm))
 * where A is the generator to the cyclic group,
 *	 B is the target/element in the group we want to find),
 *	 m = ceil(sqrt(num of elements in the group))
 */

void init_xB(unordered_map<mp::cpp_int, int>& xB, int* m) {
        // baby step
        for (int iM = 0; iM < *m; iM++) {
                mp::cpp_int val =
                    mp::powm(mp::cpp_int(A), mp::cpp_int(iM), mp::cpp_int(P));
                xB[val] = iM;
        }
}

int get_loop_size(int pubkey, int* m, mp::cpp_int* inv_Am,
                  unordered_map<mp::cpp_int, int>& xB) {
        for (int _iM = 0; _iM < *m; _iM++) {
                // giant step
                mp::cpp_int _rhs =
                    mp::powm(*inv_Am, mp::cpp_int(_iM), mp::cpp_int(P));
                mp::cpp_int _rhs2 = _rhs * mp::cpp_int(pubkey);
                mp::cpp_int rhs = _rhs2 % P;
                if (xB.find(rhs) != xB.end()) {
                        return _iM * (*m) + xB[rhs];
                }
        }

        return -1;
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                string card_pubkey_str, door_pubkey_str;
                getline(input_stream, card_pubkey_str);
                getline(input_stream, door_pubkey_str);

                int card_pubkey = atoi(card_pubkey_str.c_str());
                int door_pubkey = atoi(door_pubkey_str.c_str());

                int _m = ceil(
                    sqrt(P - 1));  // since P is prime - phi(prime) = prime-1
                mp::cpp_int m = mp::cpp_int(_m);

                mp::cpp_int inv_Am_exp = m * (P - 2);

                // Euler's theorem (fermat small number)
                // gcd (a, m) = 1 if a is coprime to m
                // * if m is prime inv_A === a^(m-2)
                mp::cpp_int inv_Am =
                    mp::powm(mp::cpp_int(A), inv_Am_exp, mp::cpp_int(P));

                unordered_map<mp::cpp_int, int> xB;
                init_xB(xB, &_m);

                int card_ls = get_loop_size(card_pubkey, &_m, &inv_Am, xB);
                int door_ls = get_loop_size(door_pubkey, &_m, &inv_Am, xB);

                mp::cpp_int ekey = powm(mp::cpp_int(card_pubkey),
                                        mp::cpp_int(door_ls), mp::cpp_int(P));
                cout << "[p1]: " << ekey << endl;
        }

        return 0;
}
