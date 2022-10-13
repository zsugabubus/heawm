#ifndef HEAWM_EDITOR_H
#define HEAWM_EDITOR_H

#include <xcb/xcb_keysyms.h>
#include <xkbcommon/xkbcommon.h>

struct editor {
	/* Cursor position. [0,len] */
	int cur;
	/* Text length. */
	int len;
	/* The text itself. */
	char buf[128];
};

void editor_clear(struct editor *);
void editor_feed(struct editor *, xkb_keysym_t, xcb_mod_mask_t);
void editor_insert(struct editor *, int index, char const *str, int n);

#endif
