#include <cstdio>
#include <unordered_map>
#include <iostream>
#include <string>


typedef uint64_t type_t;
typedef std::unordered_map<type_t, type_t> Memory;

void execute(Memory& mem, char* mask, type_t cell, type_t val) {
    type_t oval = val;

    for (int i = 0; i < 36; ++i) {
        char c = mask[35 -i];
        type_t bit = 1;
        bit <<= i;

        if (c != 'X') {
            c -= '0';

            val &= (~bit);
            val |= (bit * c);
        }
    }

    mem[cell] = val;
}

type_t toRealBin(type_t v, type_t e=0) {
    if (v <= 1) {
        return v * (1 << e);
    }
    int d = v / 10;
    int m = v % 10;
    return m * (1 << e) + toRealBin(d, e+1);
}

int main() {
    std::unordered_map<type_t, type_t> mem;

    char mask[40];
    type_t cell, val;
    std::string instr;

    while (true) {
        std::getline (std::cin, instr);
        if (std::cin.eof()) {
            break;
        }

        if (sscanf(instr.c_str(), "mem[%ld] = %ld", &cell, &val)) {
            execute(mem, mask, cell, val);
        }
        else {
            sscanf(instr.c_str(), "mask = %s", mask);
        }
        fflush(stdout);
    }

    type_t acc = 0;
    for (auto const & [key, value]: mem) {
        acc += value;
    }
    std::cout << acc << std::endl;

    return 0;
}
