#include "convolution.h"
#include "gradient_magnitude.h"
#include <immintrin.h>

Matrix conv2d(Matrix img, Matrix kernel) {
    Matrix out = {
        img.width - kernel.width + 1,
        img.height - kernel.height + 1,
        malloc(img.width * img.height * sizeof(int32_t))
    };

    #pragma omp parallel for
    for (int i = 0; i < out.height; i++) {
        for (int j = 0; j < out.width; j+=8) {
            // sum = 0;
            __m256 sum = _mm256_setzero_ps();

            for (int k = 0; k < kernel.height; k++) {
                for (int l = 0; l < kernel.width; l++) {
                    int x = j + l;
                    int y = i + k;

                    float weight = kernel.data[k*kernel.width + l];

                    // pixeis = img.data[y*img.width + x];
                    __m256 pixeis = _mm256_loadu_ps(img.data + y*img.width + x);

                    // sum += weight * pixeis[m];
                    __m256 weights = _mm256_set1_ps(weight);
                    sum = _mm256_add_ps(sum, _mm256_mul_ps(weights, pixeis));
                }
            }
            // out.data[i * out.width + j] = sum;
            _mm256_storeu_ps(out.data + i * out.width + j, sum);
        }
    }
    return out;
}
