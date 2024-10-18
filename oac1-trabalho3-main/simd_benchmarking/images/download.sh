#! /bin/bash

# Photo from https://unsplash.com/photos/LiIPyqbaU1A
  

URL='https://images.unsplash.com/photo-1544618867-7cd3be4bbaf2?ixlib=rb-4.0.3&q=85&fm=jpg&crop=entropy&cs=srgb&dl=vinicius-low-LiIPyqbaU1A-unsplash.jpg'

curl -o image.jpg $URL
convert image.jpg image.pgm
rm image.jpg
