#include <iostream>
#include <fstream>
#include <list>
#include <vector>
#include <string.h>
#include <algorithm>


typedef int64_t type_t;

const int MAXN = 20000;

// Get value from list otherwise nothing
type_t g(type_t* memo, const int& val) {
    return (val < 0) ? 0 : memo[val];
}


int main() {
    type_t memo[MAXN];
    memset(memo, 0, sizeof memo);

    std::vector<type_t> v;
    for (type_t tmp; std::cin >> tmp; ) {
        v.push_back(tmp);
    }
    std::sort(v.begin(), v.end());


    memo[0] = 1;
    for (const auto& i: v) {
        std::cout << i << std::endl;
        memo[i] = (g(memo, i -1)
                  +g(memo, i -2)
                  +g(memo, i -3));
    }

    std::cout << memo[v.back()] << std::endl;
    return 0;
}
