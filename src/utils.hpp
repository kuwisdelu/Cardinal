
#ifndef UTILS
#define UTILS

#include <R.h>
#include <Rdefines.h>

#include <stdlib.h>

#define SWAP(x) swap_bytes(&x, sizeof(x));

extern "C" {

	void swap_bytes(void * pntr, size_t n);

}

template<typename T>
T * DataPtr(SEXP x);

template<typename T>
SEXPTYPE DataType();

#endif // UTILS
