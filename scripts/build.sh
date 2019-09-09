#!/bin/bash

MAIN_DIR=$(dirname $0)/..

cd $MAIN_DIR

mkdir -p $MAIN_DIR/output
tee output/main.c <<EOF > /dev/null
#include <stdio.h>

int add(int, int);
int fac(int);
int fib(int);

int main(int argc, char** argv) {
  printf("Output from generated functions:\n");
  printf("1 + 2 = %d\n", add(1, 2));
  printf("fac(10) = %d\n", fac(10));
  printf("fib(10) = %d\n", fib(10));
  return 0;
}
EOF

cabal new-run
clang output/code.ll -c -o output/code.o
ar rc output/code.a output/code.o
clang -O3 output/{main.c,code.a} -o output/main

cd - > /dev/null
cd $MAIN_DIR/output

opt -O0 -dot-cfg code.ll > /dev/null
dot -Tpng .add.dot > add-before.png
dot -Tpng .fac.dot > fac-before.png
dot -Tpng .fib.dot > fib-before.png

opt -O3 -dot-cfg code.ll > /dev/null
dot -Tpng .add.dot > add-after.png
dot -Tpng .fac.dot > fac-after.png
dot -Tpng .fib.dot > fib-after.png

cd - > /dev/null

echo "Done! You can check files in output/ directory for results."
