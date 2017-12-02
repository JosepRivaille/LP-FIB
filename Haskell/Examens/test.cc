#include <bits/stdc++.h>
using namespace std;

typedef vector<int> VI;
typedef vector<VI> VVI;

VI input;

int main() {
    int n, tmp;
    cin >> n;
    for (uint i = 0; i < n; ++i) {
        cin >> tmp;
        input.push_back(tmp);
    } segments();
}

VVI segments() {
    VVI result;
    VI temp(1);
    temp[0] = input[0];
    for (uint i = 1; i < input.size(); ++i) {
        if (input[i] > input[0]) {
            temp.push_back(input[i]);
        } else {
            result.push_back(temp);
            temp = VI(1);
            temp[0] = input(i);
        }
    }
    return result;
}
