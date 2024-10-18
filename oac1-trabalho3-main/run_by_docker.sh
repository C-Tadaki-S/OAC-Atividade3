docker build -t oac1-t3 .
docker run -it --privileged --rm -v $PWD/src:/app -w /app oac1-t3
