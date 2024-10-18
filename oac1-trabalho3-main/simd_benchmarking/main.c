#include "image.h"
#include "convolution.h"
#include "gradient_magnitude.h"

int main() {
    Matrix image = read_image("images/image.pgm");

    double sigma = 7.0;
    Matrix dx_kernel = dx_gaussian_kernel(sigma);
    Matrix dy_kernel = dy_gaussian_kernel(sigma);

    Matrix fx = conv2d(image, dx_kernel);
    Matrix fy = conv2d(image, dy_kernel);
    Matrix magnitude = gradient_magnitude(fx, fy);

    //write_image(fx, "images/fx.pgm");
    //write_image(fy, "images/fy.pgm");
    write_image(magnitude, "images/magnitude.pgm");

    return EXIT_SUCCESS;
}

