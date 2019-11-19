
#ifndef __VVTHEME_H__
#define __VVTHEME_H__ 1

#include "vpfview.h"

int create_theme( view_type *view,
                  char *description,
                  char *expression,
                  char *database,
                  char *library,
                  char *coverage,
                  char *feature_class,
                  theme_symbol_type symbol );

int modify_theme( view_type *view,
                  int theme_number,
                  char *description,
                  char *expression,
                  char *database,
                  char *library,
                  char *coverage,
                  char *feature_class,
                  theme_symbol_type symbol );

int change_theme_symbology( view_type *view,
                            int theme_number,
                            theme_symbol_type symbol );

int delete_theme( view_type *view,
                  int theme_number );

#endif

