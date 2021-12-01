#include <boost/multiprecision/cpp_int.hpp>
#include <iostream>
#include <sstream>
#include <vector>

#include <aocdefault.h>

using namespace std;
namespace mp = boost::multiprecision;

const string BUS_NOT_IN_SERVICE = "x";

vector<int> get_buses_in_service(string bus_ids) {
        vector<int> buses_in_service;
        stringstream bus_ids_stream(bus_ids);
        string bus_id;
        while (getline(bus_ids_stream, bus_id, ',')) {
                if (bus_id != BUS_NOT_IN_SERVICE) {
                        buses_in_service.push_back(stoi(bus_id));
                }
        }

        return buses_in_service;
}

int get_bus_id_swait_time(int depart_ts, string bus_ids) {
        vector<int> service_bus_ids = get_buses_in_service(bus_ids);

        int earliest_bus_id;
        int shortest_wait = -1;
        vector<int>::const_iterator bIt = service_bus_ids.begin();
        for (; bIt != service_bus_ids.end(); ++bIt) {
                int bus_id = *bIt;
                int closest_timestamp = depart_ts / bus_id * bus_id;
                int wait = (closest_timestamp + bus_id) - depart_ts;
                if (wait < shortest_wait || shortest_wait == -1) {
                        shortest_wait = wait;
                        earliest_bus_id = bus_id;
                }
        }

        return earliest_bus_id * shortest_wait;
}

mp::cpp_int get_earliest_timestamp(string bus_ids) {
        vector<pair<int, int>> bId_delta;
        mp::cpp_int N = 1;

        string bus_id;
        int delta = 0;
        stringstream bus_ids_stream(bus_ids);
        while (getline(bus_ids_stream, bus_id, ',')) {
                if (bus_id != BUS_NOT_IN_SERVICE) {
                        int _bus_id = stoi(bus_id);
                        N *= _bus_id;
                        bId_delta.push_back(
                            make_pair(_bus_id, _bus_id - delta));
                }
                delta++;
        }

        mp::cpp_int val;
        for (int i = 0; i < bId_delta.size(); i++) {
                // chinese remainder theorem (CRT)
                int _Mi = bId_delta[i].first;
                mp::cpp_int _NdNi = N / _Mi;
                mp::cpp_int inv_NdNi = mp::powm(
                    _NdNi, _Mi - 2, _Mi);  // all bus_ids are conveniently prime
                val += inv_NdNi * _NdNi * bId_delta[i].second;
        }

        return val % N;
}

int main(int argc, char* argv[]) {
        ifstream input_stream(get_input_file_path(argc, argv));

        if (input_stream.is_open()) {
                int d_ts;
                string bus_ids;
                input_stream >> d_ts;
                input_stream >> bus_ids;

                // Part 1
                cout << "[p1]: " << get_bus_id_swait_time(d_ts, bus_ids)
                     << endl;

                // Part 2
                cout << "[p2]: " << get_earliest_timestamp(bus_ids) << endl;
        }

        return 1;
}
