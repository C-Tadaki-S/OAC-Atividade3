#include "convolution.h"
#include "gradient_magnitude.h"

// Vers'ao intermediaria para o sind que ajuda a visualizar o vertor de soma e pixel
// a ideia aqui eh carregar 16 pixels de uma vez

Matrix conv2d(Matrix img, Matrix kernel) {
    Matrix out = {
        img.width,
        img.height,
        malloc(img.width * img.height * sizeof(int32_t))
    };
    #pragma omp parallel for
    for (int i = 0; i < out.height; i++) {
        for (int j = 0; j < out.width; j++) {
            //__m256i sum = _mm256_setzero_si256(); // sum = 0
            float sum[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
            for (int k = 0; k < kernel.height; k++) {
                for (int l = 0; l < kernel.width; l++) {
                    int x = j + l;
                    int y = i + k;

                    float weight = kernel.data[k*kernel.width + l];

                    float pixeis[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
                    for (int m = 0; m < 16; m++) {
                        pixeis[m] = img.data[y*img.width + x + m];
                        sum[m] += weight * pixeis[m];
                    }
                }
            }
            for(int m = 0; m < 16; m++) {
                out.data[i * out.width + j + m] = sum[m];
            }
        }
    }
    return out;
}
