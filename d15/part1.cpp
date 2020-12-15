// --- Day 15: Rambunctious Recitation ---
#include <unordered_map>
#include <cstdio>
#include <iostream>

typedef long type_t;

int main() {

    std::unordered_map<type_t, type_t> nums;
    type_t turn = 0;
    type_t nextNum, lastCall;

    while(scanf("%ld,", &nextNum) != EOF) {
        nums[nextNum] = ++turn;
        std::cout << "read " << nextNum << std::endl;
    }

    while (turn != 2020){
        // nextNum contains previous said number
        // nums[nextNum] = turn;

        auto exists = nums.find(nextNum) != nums.end();
        if (exists) {
            lastCall = nums[nextNum];
        }
        nums[nextNum] = turn;


        if (!exists) {
            nextNum = 0;
        } else {
            nextNum = turn - lastCall;
        }
        turn += 1;

    }
    std::cout << nextNum << std::endl;
}
