#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "editor.h"

#define OK(i) (0 <= (i) && (i) <= ed->len)

static bool
editor_is_wbrk(struct editor const *ed, int i)
{
	switch (ed->buf[i]) {
	case ' ':
	case '/':
		return true;

	default:
		return false;
	}
}

static int
editor_seek_char(struct editor const *ed, int i, int dir)
{
	while (OK(i + dir) && (0x80 & ed->buf[(i += dir)]));
	return i;
}

static int
editor_seek_word(struct editor const *ed, int i, int dir)
{
	while (OK(i + dir) && editor_is_wbrk(ed, (i += dir)));
	while (OK(i + dir) && !editor_is_wbrk(ed, i + (dir < 0 ? dir : 0)))
		i += dir;
	return i;
}

void
editor_clear(struct editor *ed)
{
	ed->cur = 0;
	ed->len = 0;
	ed->buf[0] = '\0';
}

void
editor_insert(struct editor *ed, int index, char const *str, int n)
{
	if (sizeof ed->buf <= (size_t)ed->len + n + 1 /* NUL */)
		return;
	memmove(ed->buf + index + n, ed->buf + index, ed->len - index);
	memcpy(ed->buf + index, str, n);
	if (index <= ed->cur)
		ed->cur += n;
	ed->len += n;
	ed->buf[ed->len] = '\0';
}

/* [start,end) */
static void
editor_cut(struct editor *ed, int start, int end)
{
	memmove(ed->buf + start, ed->buf + end, ed->len - end);
	if (end <= ed->cur)
		ed->cur -= end - start;
	else if (start < ed->cur)
		ed->cur = start;
	ed->len -= end - start;
	ed->buf[ed->len] = '\0';
}

void
editor_feed(struct editor *ed, xkb_keysym_t keysym, xcb_mod_mask_t mods)
{
	mods &= ~XCB_MOD_MASK_SHIFT;

	if (!mods) {
		switch (keysym) {
		case XKB_KEY_Home:
			goto start_of_line;

		case XKB_KEY_End:
			goto end_of_line;

		case XKB_KEY_Right:
			goto forward_char;

		case XKB_KEY_Left:
			goto backward_char;

		case XKB_KEY_BackSpace:
			goto delete_backward_char;

		case XKB_KEY_Delete:
			goto delete_forward_char;

		default:
		{
			char str[32];
			int n = xkb_keysym_to_utf8(keysym, str, sizeof str);
			assert(0 <= n);
			/* n includes terminating NUL byte. It cannot
			 * be 1 because it signifies empty return. */
			if (n <= 1)
				break;
			n -= 1 /* NUL */;
			/* Ignore control characters. */
			if (1 == n && str[0] < ' ')
				break;
			editor_insert(ed, ed->cur, str, n);
			return;
		}
		}
	} else if (XCB_MOD_MASK_CONTROL == mods) {
		switch (keysym) {
		case XKB_KEY_a:
		start_of_line:
			ed->cur = 0;
			return;

		case XKB_KEY_e:
		end_of_line:
			ed->cur = ed->len;
			return;

		case XKB_KEY_f:
		forward_char:
			ed->cur = editor_seek_char(ed, ed->cur, 1);
			return;

		case XKB_KEY_b:
		backward_char:
			ed->cur = editor_seek_char(ed, ed->cur, -1);
			return;

		case XKB_KEY_h:
		delete_backward_char:
			editor_cut(ed, editor_seek_char(ed, ed->cur, -1), ed->cur);
			return;

		case XKB_KEY_d:
		delete_forward_char:
			editor_cut(ed, ed->cur, editor_seek_char(ed, ed->cur, 1));
			return;

		case XKB_KEY_u:
		delete_backward_line:
			editor_cut(ed, 0, ed->cur);
			return;

		case XKB_KEY_k:
			if (ed->len <= ed->cur)
				goto delete_backward_line;
			editor_cut(ed, ed->cur, ed->len);
			return;

		case XKB_KEY_w:
		delete_backward_word:
			editor_cut(ed, editor_seek_word(ed, ed->cur, -1), ed->cur);
			return;

		case XKB_KEY_Right:
			goto forward_word;

		case XKB_KEY_Left:
			goto backward_word;

		case XKB_KEY_BackSpace:
			goto delete_backward_word;
		}
	} else if (XCB_MOD_MASK_1 == mods) {
		switch (keysym) {
		case XKB_KEY_f:
		forward_word:
			ed->cur = editor_seek_word(ed, ed->cur, 1);
			return;

		case XKB_KEY_b:
		backward_word:
			ed->cur = editor_seek_word(ed, ed->cur, -1);
			return;

		case XKB_KEY_d:
		{
			int end = editor_seek_word(ed, ed->cur, 1);
			int start = editor_seek_word(ed, end, -1);
			editor_cut(ed, start, end);
		}
			return;
		}
	}
}
