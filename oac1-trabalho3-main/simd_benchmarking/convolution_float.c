#include "convolution.h"

Matrix conv2d(Matrix img, Matrix kernel) {
    Matrix out = {
        img.width - kernel.width + 1,
        img.height - kernel.height + 1,
        malloc(img.width * img.height * sizeof(float))
    };

    #pragma omp parallel for
    for (int i = 0; i < out.height; i++) {
        for (int j = 0; j < out.width; j++) {
            float sum = 0;
            for (int k = 0; k < kernel.height; k++) {
                for (int l = 0; l < kernel.width; l++) {
                    int x = j + l;
                    int y = i + k;

                    float weight = kernel.data[k*kernel.width + l];
                    float pixel = img.data[y*img.width + x];

                    sum += weight * pixel;
                }
            }
            out.data[i * out.width + j] = sum;
        }
    }

    return out;
}
