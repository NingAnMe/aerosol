
#ifndef __VVSPQRY_H__
#define __VVSPQRY_H__ 1

#include "vpfview.h"

void spatial_query( view_type *view,
                    map_environment_type *mapenv,
                    double xsearch,
                    double ysearch,
                    int points,
                    int lines,
                    int areas );

#endif

