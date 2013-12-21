
#include <stdlib.h>

#ifndef SWAP
#define SWAP(x) swap_bytes(&x, sizeof(x));

void swap_bytes(void * pntr, size_t n);

#endif
