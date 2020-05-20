#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <iostream>
#include <optional>
#include <set>

using namespace std;

/*
set<size_t> get_all_targets(const uint32_t *buf, size_t len);
*/

extern "C" bool proton_armcheck_check_buffer(const uint32_t *buf, size_t len,
											 size_t *fail_idx,
											 const char **fail_instr_name,
											 const char **fail_test_name);

int main(int argc, char **argv) {
    int fd = [&] {
        if (argc < 2) {
            return open("fuck_0000.out", O_RDONLY);
        } else {
            return open(argv[1], O_RDONLY);
        }
    }();
    if (fd < 0) {
        perror("Could not open file");
        return -1;
    }
	size_t len = lseek(fd, 0, SEEK_END);
    uint32_t *buf = (uint32_t *)mmap(NULL, len, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);

    /*
    const auto alltarg = get_all_targets(buf, len / 4);
    cout << hex;
    for (auto t: alltarg) {
        cout << "0x" << (4 * t) << endl;
    }
    */

    size_t fail_idx;
    const char *fail_instr_name;
    const char *fail_test_name;
    if (!proton_armcheck_check_buffer(buf, len / 4, &fail_idx, &fail_instr_name, &fail_test_name)) {
        printf("failed: %s (%s) at %lx\n", fail_instr_name, fail_test_name, fail_idx);
        return -2;
    }

    return 0;
}
