
#include <R.h>
#include <Rinternals.h>

#include "utils.h"

template<typename T>
SEXP find_local_maxima(SEXP x, int r) {
	int len = LENGTH(x);
	SEXP isLocMax;
	PROTECT(isLocMax = allocVector(LGLSXP, len));
	int * locmax = LOGICAL(isLocMax);
	for ( int i = 0; i < len; ++i )
		locmax[i] = false;
	T * y = DataPtr<T>(x);
	for ( int i = r; i < len - r; ++i )
	{
		locmax[i] = true;
		for ( int j = i - r; j <= i + r; ++j )
		{
			if ( j < i )
			{
				if ( y[j] >= y[i] )
				{
					locmax[i] = false;
					break;
				}
			}
			if ( j > i )
			{
				if ( y[j] > y[i] )
				{
					locmax[i] = false;
					break;
				}
			}
		}
	}
	UNPROTECT(1);
	return isLocMax;
}

extern "C" {

	SEXP localMaxima(SEXP x, SEXP r)
	{
		switch(TYPEOF(x)) {
			case INTSXP:
				return find_local_maxima<int>(x, asInteger(r));
			case REALSXP:
				return find_local_maxima<double>(x, asInteger(r));
		}
		return R_NilValue;
	}

}
