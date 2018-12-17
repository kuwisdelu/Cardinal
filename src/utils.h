
#ifndef UTILS
#define UTILS

#include <R.h>
#include <Rinternals.h>

#include <cstdlib>

#define SWAP(x) swap_bytes(&x, sizeof(x));

extern "C" {

	void swap_bytes(void * pntr, size_t n);

}

template<typename T>
T * DataPtr(SEXP x);

template<typename T>
SEXPTYPE DataType();

template<typename T>
bool DataValid(T x);

template<typename T>
T DataNA();

#endif // UTILS
