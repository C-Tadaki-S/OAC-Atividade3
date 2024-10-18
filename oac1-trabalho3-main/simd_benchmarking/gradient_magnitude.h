#ifndef GRADIENT_MAGNITUDE_H
#define GRADIENT_MAGNITUDE_H

#include "image.h"
#include <math.h>
#include <stdint.h>

Matrix gaussian_kernel(double sigma);
Matrix dx_gaussian_kernel(double sigma);
Matrix dy_gaussian_kernel(double sigma);
Matrix gradient_magnitude(Matrix fx, Matrix fy);

#endif
