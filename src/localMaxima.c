
#include <R.h>

void localMaxima(double * x, int * isLocMax, int * length, int * halfWindowSize) {
	int last = *length - *halfWindowSize;
	int span = (*halfWindowSize * 2) + 1;
	for ( int i = *halfWindowSize; i < last; ++i ) {
		isLocMax[i] = 1;
		int edge = 0;
		for ( int j = i - *halfWindowSize; j < i + span; ++j) {
			if ( i != j ) {
				if ( x[j] > x[i] )
				{
					isLocMax[i] = 0;
					break;
				}
				else if ( x[j] == x[i] )
				{
					edge = 1;
				}
				else if ( x[j] < x[i] )
				{
					edge = 0;
				}
			}
			isLocMax[i] = !edge;
		}
	}
}
