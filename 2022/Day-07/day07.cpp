#include <assert.h>

#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <vector>

#define DISK_SPACE 70000000
#define REQUIRED_SPACE 30000000

struct File {
	long long int size;
	std::string path;
};

struct Dir {
	std::string path;
	std::vector<File> files;
	std::vector<Dir *> dirs;
	long long int size;
};

Dir *add_dir(std::string path, std::map<std::string, Dir *> &dir_map) {
	if (dir_map.find(path) == dir_map.end()) {
		Dir *new_dir = new Dir();
		new_dir->path = path;
		dir_map[path] = new_dir;
	}

	return dir_map[path];
}

long long int calculate_dir_size(std::string dir_path,
								 std::map<std::string, Dir *> &dir_map) {
	long long int size = 0;

	Dir *dir = dir_map[dir_path];
	for (File f : dir->files) size += f.size;
	for (Dir *d : dir->dirs) size += calculate_dir_size(d->path, dir_map);

	dir->size = size;
	return size;
}

std::string get_path(std::vector<std::string> &path_stack) {
	std::string path = "/";
	for (int idx = 1; idx < path_stack.size(); idx++) {
		if (idx < path_stack.size() - 1) {
			path += path_stack[idx] + "/";
		} else {
			path += path_stack[idx];
		}
	}

	return path;
}

int main(int argc, char *argv[]) {
	std::ifstream input_stream("./input.txt");
	std::string line;
	std::vector<std::string> logs;

	while (getline(input_stream, line)) logs.push_back(line);

	std::regex file_regex("(\\d+) ([\\w|.]+)");
	std::vector<std::string> path_stack = {"/"};
	std::map<std::string, Dir *> dir_map;
	for (int idx = 0; idx < logs.size(); idx++) {
		std::string log = logs[idx];
		bool is_cmd = log[0] == '$';
		bool is_ls_cmd = is_cmd && log.substr(2, 2) == "ls";
		bool is_cd_cmd = is_cmd && log.substr(2, 2) == "cd";

		if (is_cd_cmd) {
			std::string dir_name = log.substr(5);
			if (dir_name == "..") {
				path_stack.pop_back();
				continue;
			}
			if (dir_name == "/")
				path_stack.erase(path_stack.begin(), path_stack.end());

			std::string dir_path = get_path(path_stack);
			add_dir(dir_path, dir_map);
			path_stack.push_back(dir_name);
		} else if (is_ls_cmd) {
			std::string curr_dir_path = get_path(path_stack);

			int ls_idx = idx + 1;
			while (ls_idx < logs.size() && logs[ls_idx][0] != '$') {
				std::string curr_log = logs[ls_idx];
				bool is_dir = curr_log.substr(0, 3) == "dir";
				if (is_dir) {
					std::string nested_dir_name = curr_log.substr(4);
					path_stack.push_back(nested_dir_name);
					std::string nested_dir_path = get_path(path_stack);
					path_stack.pop_back();

					dir_map[curr_dir_path]->dirs.push_back(
						add_dir(nested_dir_path, dir_map));
				} else {
					std::smatch matches;
					std::regex_search(curr_log, matches, file_regex);
					assert(matches.size() == 3);

					long long int file_size = std::stoi(matches[1]);
					std::string file_name = matches[2];
					dir_map[curr_dir_path]->files.push_back(
						{file_size, file_name});
				}
				ls_idx++;
			}
		}
	}

	long long int required_space =
		REQUIRED_SPACE - (DISK_SPACE - calculate_dir_size("/", dir_map));

	long long int p1_total_size = 0;
	long long int p2_dir_size = 0;
	long long int min_diff = INT64_MAX;
	for (std::map<std::string, Dir *>::iterator it = dir_map.begin();
		 it != dir_map.end(); it++) {
		long long int dir_size = it->second->size;

		if (dir_size < 100000) p1_total_size += dir_size;

		long long int dir_delta = dir_size - required_space;
		if (dir_delta >= 0 && dir_delta < min_diff) {
			min_diff = dir_delta;
			p2_dir_size = dir_size;
		}
	}

	std::cout << "Part 1: " << p1_total_size << std::endl;
	std::cout << "Part 2: " << p2_dir_size << std::endl;

	return EXIT_SUCCESS;
}

