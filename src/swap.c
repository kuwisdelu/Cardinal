
#include "swap.h"

void swap_bytes(void * pntr, size_t n)
{
    char * p = pntr;
    size_t lo, hi;
    for ( lo = 0, hi = n - 1; lo < hi; lo++, hi-- )
    {
        char tmp = p[lo];
        p[lo] = p[hi];
        p[hi] = tmp;
    }
}
