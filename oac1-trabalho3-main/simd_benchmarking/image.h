#ifndef IMAGE_H
#define IMAGE_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int width;
  int height;
  float *data;
} Matrix;

void write_image(Matrix image, const char *filename);
Matrix read_image(const char *filename);
void free_image(Matrix image);
void print_image(Matrix image);

#endif
