#include "float_lib.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <fenv.h>

#define VERBOSE 0

typedef struct {
    uint32_t total;
    uint32_t passed;
} test_result;

// from https://math.stackexchange.com/a/4508789
uint8_t within(float a, float b, float tol) {
    float d = fabs(a - b);
    if (d == 0) {
        return 1;
    }
    // Since d =/= 0 we know a, b =/= 0 so division by zero will not happen
    float err = d/(fabs(a)+fabs(b));
    return err < tol;
}

typedef union {
    mfloat i; // acess as int
    float  f; // acess as float
              // Lembrando que na implementacao de voces, voces nao podem usar float!
} floatint;

int assert_mint(int32_t a, int32_t b) {
    if (a != b) {
        if (VERBOSE) {
            printf("ERROR. Expected %d, got %d\n", b, a);
        }
        return 0;
    } else {
        if (VERBOSE) {
            printf("OK. %d\n", a);
        }
        return 1;
    }
}

int assert_float(floatint a, floatint b, float tol) {
    if (!within(a.f, b.f, tol)) {
        if (VERBOSE) {
            printf("ERROR. Expected %f (%x), got %f (%x)\n", b.f, b.i, a.f, a.i);
        }
        return 0;
    } else {
        if (VERBOSE) {
            printf("OK. %f (%x)\n", a.f, a.i);
        }
        return 1;
    }
}

int assert_floatsisf(int32_t a) {
    floatint actual, expected;

    actual.i = floatsisf(a);
    expected.f = (float) a;

    return assert_float(actual, expected, 0.00001);
}

test_result test_cases_floatsist() {
    test_result result = {0, 0};
    puts("Testing floatsist");
    for (mint i = INT32_MIN; i < -0; i/=3) {
        result.passed += assert_floatsisf(i);
        result.total++;
    }
    result.passed += assert_floatsisf(0);
    result.total++;
    for (mint i = INT32_MAX; i > 0; i/=3) {
        result.passed += assert_floatsisf(i);
        result.total++;
    }
    return result;
}

int assert_fixsfsi(float a) {
    floatint arg;
    mint actual, expected;

    arg.f = a;
    actual = fixsfsi(arg.i);
    expected = (mint) a;

    return assert_mint(actual, expected);
}

test_result test_cases_fixsfsi() {
    test_result result = {0, 0};
    puts("Testing fixsfsi");
    for (float i = INT32_MIN; i < -1e-7; i/=3) {
        result.passed += assert_fixsfsi(i);
        result.total++;
    }
    result.passed += assert_fixsfsi(0.0);
    result.total++;
    for (float i = INT32_MAX; i > 1e-7; i/=3) {
        result.passed += assert_fixsfsi(i);
        result.total++;
    }
    return result;
}

int assert_negsf2(float a) {
    floatint arg;
    floatint actual, expected;

    arg.f = a;
    actual.i = negsf2(arg.i);
    expected.f = -a;

    return assert_float(actual, expected, 0);
}

test_result test_cases_negsf2() {
    test_result result = {0, 0};
    puts("Testing negsf2");
    for (float i = FLT_MAX; i > 0; i/=3) {
        result.passed += assert_negsf2(i);
        result.passed += assert_negsf2(-i);
        result.total += 2;
    }
    result.passed += assert_negsf2(0.0);
    result.total++;
    return result;
}

// float flags from: https://stackoverflow.com/a/15655732
int assert_addsf3(float a, float b) {
    floatint arg1, arg2;
    floatint actual, expected;

    arg1.f = a;
    arg2.f = b;
    actual.i = addsf3(arg1.i, arg2.i);

    feclearexcept(FE_ALL_EXCEPT);
    expected.f = a + b;
    if (fetestexcept(FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW)) {
        return -1; // ignore invalid test cases
    }

    return assert_float(actual, expected, 0.025);
}

test_result test_cases_addsf3() {
    test_result result = {0, 0};
    puts("Testing addsf3");
    for (float i = FLT_MAX; i > 1e-20; i/=113) {
        for (float j = FLT_MAX; j > 1e-20; j/=113) {
            int r1 = assert_addsf3(i, j);
            int r2 = assert_addsf3(-i, j);
            int r3 = assert_addsf3(i, -j);
            int r4 = assert_addsf3(-i, -j);

            if (r1 != -1) {
                result.passed += r1;
                result.total++;
            }

            if (r2 != -1) {
                result.passed += r2;
                result.total++;
            }

            if (r3 != -1) {
                result.passed += r3;
                result.total++;
            }

            if (r4 != -1) {
                result.passed += r4;
                result.total++;
            }
        }
    }
    return result;
}

int assert_subsf3(float a, float b) {
    floatint arg1, arg2;
    floatint actual, expected;

    arg1.f = a;
    arg2.f = b;
    actual.i = subsf3(arg1.i, arg2.i);

    feclearexcept(FE_ALL_EXCEPT);
    expected.f = a - b;
    if (fetestexcept(FE_INVALID | FE_OVERFLOW | FE_UNDERFLOW)) {
        return -1; // ignore invalid test cases
    }

    return assert_float(actual, expected, 0.025);
}

test_result test_cases_subsf3() {
    test_result result = {0, 0};
    puts("Testing subsf3");
    for (float i = FLT_MAX; i > 1e-20; i/=113) {
        for (float j = FLT_MAX; j > 1e-20; j/=113) {
            int r1 = assert_subsf3(i, j);
            int r2 = assert_subsf3(-i, j);
            int r3 = assert_subsf3(i, -j);
            int r4 = assert_subsf3(-i, -j);

            if (r1 != -1) {
                result.passed += r1;
                result.total++;
            }

            if (r2 != -1) {
                result.passed += r2;
                result.total++;
            }

            if (r3 != -1) {
                result.passed += r3;
                result.total++;
            }

            if (r4 != -1) {
                result.passed += r4;
                result.total++;
            }
        }
    }
    return result;
}

int main() {
    test_result rfloatsist = test_cases_floatsist();
    test_result rfixsfsi = test_cases_fixsfsi();
    test_result rnegsf2 = test_cases_negsf2();
    test_result raddsf3 = test_cases_addsf3();
    test_result subsf3 = test_cases_subsf3();

    puts("==============FINAL RESULTS==============");
    printf("floatsist: %d/%d (%.2f)\n", rfloatsist.passed, rfloatsist.total, (float) rfloatsist.passed/rfloatsist.total * 100);
    printf("fixsfsi: %d/%d (%.2f)\n", rfixsfsi.passed, rfixsfsi.total, (float) rfixsfsi.passed/rfixsfsi.total * 100);
    printf("negsf2: %d/%d (%.2f)\n", rnegsf2.passed, rnegsf2.total, (float) rnegsf2.passed/rnegsf2.total * 100);
    printf("addsf3: %d/%d (%.2f)\n", raddsf3.passed, raddsf3.total, (float) raddsf3.passed/raddsf3.total * 100);
    printf("subsf3: %d/%d (%.2f)\n", subsf3.passed, subsf3.total, (float) subsf3.passed/subsf3.total * 100);
    puts("=========================================");
    return 0;
}

