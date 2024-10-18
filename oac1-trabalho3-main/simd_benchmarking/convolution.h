#ifndef CONVOLUTION_H
#define CONVOLUTION_H

#include <stdint.h>
#include "image.h"
#include "gradient_magnitude.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))
#define clamp(x, a, b) max(a, min(b, x))

Matrix conv2d(Matrix img, Matrix kernel);

#endif

