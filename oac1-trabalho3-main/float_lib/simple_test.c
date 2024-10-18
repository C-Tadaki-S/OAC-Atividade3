#include "float_lib.h"
#include <stdio.h>
#include <stdlib.h>

void assert_mint(mint a, mint b) {
    if (a != b) {
        printf("Expected %d, got %d\n", b, a);
        exit(1);
    }
}

void assert_mfloat(mfloat a, mfloat b) {
    if (a != b) {
        printf("Expected %x, got %x\n", b, a);
        exit(1);
    }
}

// You can use https://www.h-schmidt.net/FloatConverter/IEEE754.html
int main() {
    mint a_int = 10;
    mint b_int = -20;

    mfloat a_float = floatsisf(a_int);
    mfloat b_float = floatsisf(b_int);

    assert_mfloat(a_float, 0x41200000);
    assert_mfloat(b_float, 0xc1a00000);

    mfloat res_float = subsf3(a_float, b_float);
    mint res_int = fixsfsi(res_float);

    assert_mint(res_int, 30);
}

