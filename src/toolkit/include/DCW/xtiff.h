
#ifndef __XTIFF_H__
#define __XTIFF_H__ 1

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
/* tifflib.h */


int X_save_tiff_image( char *filename, Display *display,
                       Window win, GC gc );

int X_read_tiff_image( char *filename, Display *display,
                       Pixmap pix, GC gc );

#endif

