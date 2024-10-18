#include "image.h"
#include <stdint.h>

typedef struct {
    int32_t max;
    int32_t min;
} MaxMin;

MaxMin max_min_in_list(float* list, int32_t size) {
    float max = list[0];
    float min = list[0];

    for (int32_t i = 1; i < size; i++) {
        if (list[i] > max) {
            max = list[i];
        }
        if (list[i] < min) {
            min = list[i];
        }
    }

    return (MaxMin) {
        max,
        min
    };
}

void write_image(Matrix image, const char *filename) {
  FILE *file = fopen(filename, "wb");

  char *comment = "# Mandelbrot set";
  fprintf(file, "P5\n %s\n %d\n %d\n %d\n", comment, image.width, image.height, 255);

  MaxMin max_min = max_min_in_list(image.data, image.width * image.height);
  float max = max_min.max;
  float min = max_min.min;

  for (int i = 0; i < image.width * image.height; i++) {
      float data = image.data[i];
      uint8_t pixel = (data - min) * 255 / (max - min);
      fputc(pixel, file);
  };

  fclose(file);
}

Matrix read_image(const char *filename) {
    FILE *file = fopen(filename, "rb");

    int width, height;
    fscanf(file, "P5\n%d %d\n255\n", &width, &height);

    float *data = malloc(width * height * sizeof(float));

    for (int i = 0; i < width * height; i++) {
        data[i] = fgetc(file);
    }

    fclose(file);

    return (Matrix) {
        width,
        height,
        data
    };
}

void print_image(Matrix image) {
    for (int i = 0; i < image.height; i++) {
        for (int j = 0; j < image.width; j++) {
            printf("%06d ", (int) image.data[i * image.width + j]);
        }
        printf("\n");
    }
}

void free_image(Matrix image) {
    free(image.data);
}
