#include <cairo.h>

#define CAIRO_CHECK_VERSION(major, minor, micro)                    \
        (CAIRO_VERSION >= CAIRO_VERSION_ENCODE(major, minor, micro))
