#ifndef HEAWM_WIN_HASH_H
#define HEAWM_WIN_HASH_H

#include <xcb/xcb.h>

struct win *win_hash_get(xcb_window_t);
void win_hash_set(xcb_window_t, struct win *);
void win_hash_del(xcb_window_t);

#endif
