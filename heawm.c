#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <memory.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <xcb/randr.h>
#include <xcb/shape.h>
#include <xcb/xcb_atom.h>
#include <xcb/xcb_aux.h>
#include <xcb/xcb_cursor.h>
#include <xcb/xcb_event.h>
#include <xcb/xcb.h>
#include <xcb/xcb_image.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xcb_xrm.h>
#include <xcb/xproto.h>
#include <xcb/xinput.h>
#include <xkbcommon/xkbcommon.h>

/* https://github.com/LemonBoy/bar */

#include "atoms.h"

#define DEFAULT_CURSOR_NAME "default"

#define NAME_LEN 2

/* sizeof array... but in elements */
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

struct box {
	int16_t x, y; /* box location in pixels relative to the root box */
	uint16_t width, height; /* box dimensions in pixel;
	                           (width, height) = (0, 0) <=> unmapped */
	uint16_t mouse_x, mouse_y; /* relative mouse positions */
	xcb_window_t window;
	xcb_window_t frame;

	uint16_t num_children; /* number of |children| */
	uint16_t iter;

	uint16_t num_columns; /* number of columns (in grid layout); 0 means auto */
	char name[NAME_LEN]; /* permanent name */
	char *title; /* X11 window title */
	uint8_t weight; /* relative to others; only if grid consists of a
	                   single column or row */

	bool position_changed: 1, /* box repositioned */
	     layout_changed: 1, /* any of the children's boundary box changed */
	     focus_changed: 1, /* focused child changed */
	     content_changed: 1, /* set by children to indicate parent should
	                            descend with update (means that this flag
	                            must be propagated upwards until root) */
	     title_changed: 1,
	     focus_lock_changed: 1,
	     self_focus_changed: 1,

	     concealed: 1, /* show only when has focus inside */
	     focus_lock: 1, /* swap newly focused window with focused window */
	     self_focus: 1; /* only for containers */
	uint32_t focus_seq; /* focus sequence */
	struct box *parent; /* the bigger box */
	struct box *children[];
};

struct label {
	struct box *base;
	xcb_window_t window;
	xcb_window_t shape;
	int x: 3,
	    y: 3;
	char name[NAME_LEN];
};

struct head {
	/* xrandr https://stackoverflow.com/questions/22108822/how-do-i-get-the-resolution-of-randr-outputs-through-the-xcb-randr-extension */
	xcb_screen_t *screen;
	xcb_visualtype_t *visual_type;

	size_t num_labels_used;
	size_t num_labels_mapped;
	size_t num_labels;
	struct label *labels;

	xcb_window_t bar;
	struct box *root;
};

/* stuff you can draw with on boxes */
struct hand {
	xcb_input_device_id_t key;
	xcb_input_device_id_t button;
};

/* a strictly monotonically increasing number that gets assigned to focused
 * boxes; this way their global order can be tracked with benefits:
 * - no doubly linked list or single linked list with hacks
 * - focused box can be easily get from parent without additionaly fields */
uint32_t focus_seq;
struct box *lastest_input; /* box that last time received keyboard input (only non-container) */

static uint32_t num_heads;
static struct head *heads;

static uint32_t num_hands;
static struct hand *hands;

static bool allow_map_focus = true;

/* XCB_ATOM_WM_HINTS */
/* XCB_ATOM_WM_NAME */
/* XCB_ATOM_WM_CLASS */
/* XCB_ATOM_WM_TRANSIENT_FOR */

static char const *const ATOM_NAMES[] =
{
	"_NET_WM_STATE",
	"_NET_WM_STATE_MODAL",
	"_NET_WM_STATE_DEMANDS_ATTENTION",
	"_NET_WM_STATE_FULLSCREEN",
	"_NET_WM_TRANSIENT_FOR",
	"_NET_WM_NAME",
	"_NET_ACTIVE_WINDOW",
	"UTF8_STRING"
#if 0
	/* https://specifications.freedesktop.org/wm-spec/1.3/ar01s05.html */
	/* https://specifications.freedesktop.org/wm-spec/1.3/ar01s07.html */
	/* https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html#idm46035372536800 */
	"_NET_WM_WINDOW_STATE",
	"_NET_WM_WINDOW_TYPE",
	"_NET_WM_TYPE_DOCK",
#endif
};
/* resolved values */
static xcb_atom_t atoms[ARRAY_SIZE(ATOM_NAMES)];

static xcb_connection_t *conn;
static xcb_key_symbols_t *symbols;

static int argc;
static char **argv;

static char user_input[NAME_LEN];
static enum {
	label_boxes,
	label_corners,
} label_what;

#define for_each_box(x, root, start, block) \
	for ((x) = (start);;) { \
		(x)->iter = 0; \
		block; \
		while ((x)->num_children <= (x)->iter) { \
			if ((x) == (root)) { \
				(x) = NULL; \
				goto out_for_each; \
			} \
			assert(NULL != (x)->parent); \
			(x) = (x)->parent; \
		} \
		(x) = (x)->children[x->iter++]; \
	} \
out_for_each:;


static struct box *
find_box_by_window(struct box *root, struct box *start, xcb_window_t const window)
{
	struct box *box;
	for_each_box(box, root, start, {
		if (window == box->window)
			break;
	})
	return box;
}

static int const LABEL_WIDTH = 50;
static int const LABEL_HEIGHT = 50;
static float const LABEL_STROKE = 2.5;
static float const LABEL_SIZE = 22;

static void
repaint_label(struct head const *const head, struct label const *const label, bool shape)
{
	char name[NAME_LEN + 1];

	memcpy(name, label->name, sizeof label->name);
	name[NAME_LEN] = '\0';

	cairo_surface_t *const surface = shape
		? cairo_xcb_surface_create_for_bitmap(conn, head->screen, label->shape, LABEL_WIDTH, LABEL_HEIGHT)
		: cairo_xcb_surface_create(conn, label->window, head->visual_type, LABEL_WIDTH, LABEL_HEIGHT);
	cairo_t *const cr = cairo_create(surface);

	cairo_select_font_face(cr, "monospace",
			CAIRO_FONT_SLANT_NORMAL,
			CAIRO_FONT_WEIGHT_BOLD);

	/* https://www.designworkplan.com/read/signage-and-color-contrast */
	/* http://www.writer2001.com/colwebcontrast.htm */
	/* https://dequeuniversity.com/tips/color-contrast */
	/* cairo_rectangle(cr, 0, 0, 40, 40);
	cairo_set_source_rgb(cr, 1.0, 1.0, 0.0); */
	/* cairo_fill(cr); */

	if (shape) {
		cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
		cairo_rectangle(cr, 0, 0, LABEL_WIDTH, LABEL_HEIGHT);
		cairo_fill(cr);
	}

	cairo_set_font_size(cr, LABEL_SIZE);

	cairo_text_extents_t te;
	cairo_text_extents(cr, name, &te);

	cairo_move_to(cr,
			0.5 - te.x_bearing + (LABEL_WIDTH - te.width) / 2,
			0.5 - te.y_bearing + (LABEL_HEIGHT - te.height) / 2);

	cairo_text_path(cr, name);
	
	if (!shape)
		/* label stroke */
		cairo_set_source_rgb(cr, 0, 0, 0);
	else
		cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
	cairo_set_line_width(cr, LABEL_STROKE);
	cairo_stroke(cr);

	cairo_move_to(cr,
			0.5 - te.x_bearing + (LABEL_WIDTH - te.width) / 2,
			0.5 - te.y_bearing + (LABEL_HEIGHT - te.height) / 2);

	if (!shape)
		/* label color */
		cairo_set_source_rgb(cr, 1, 1, 0);
	cairo_show_text(cr, name);

	cairo_surface_flush(surface);
	cairo_surface_destroy(surface);
	cairo_destroy(cr);
}

static struct label *
new_label(struct head *const head)
{
	if (head->num_labels_used == head->num_labels) {
		size_t const new_size = (head->num_labels * 8 / 5/*golden ratio*/) + 1;
		struct label *p = realloc(head->labels, new_size * sizeof *p);
		if (NULL == p)
			return NULL;
		head->labels = p;
		head->num_labels = new_size;

		/* initialize new labels */
		for (uint32_t i = head->num_labels_used;
		     i < head->num_labels;
		     ++i)
		{
			struct label *label = &head->labels[i];
			label->window = XCB_WINDOW_NONE;
		}
	}

	return &head->labels[head->num_labels_used++];
}

static void
print_error(xcb_generic_error_t const *const error)
{
	fprintf(stderr, "yay! X11 error: %s\n", xcb_event_get_error_label(error->error_code));
}

static bool
check_cookie(xcb_void_cookie_t cookie)
{
	xcb_generic_error_t *error;

	if (NULL != (error = xcb_request_check(conn, cookie))) {
		print_error(error);
		free(error);
		return false;
	} else {
		return true;
	}
}

static void
show_label(struct head const *const head, struct label *const label, int16_t relx, int16_t rely)
{
	xcb_screen_t *const screen = head->screen;
	int16_t x, y;

	x = label->base->x + relx * label->base->width / 2 - LABEL_WIDTH;
	y = label->base->y + rely * label->base->height / 2;

	if (XCB_WINDOW_NONE == label->window) {
		/* setup label window */
		label->window = xcb_generate_id(conn);

		xcb_create_window(conn, XCB_COPY_FROM_PARENT,
				label->window,
				screen->root,
				x, y,
				LABEL_WIDTH, LABEL_HEIGHT,
				0,
				XCB_WINDOW_CLASS_INPUT_OUTPUT,
				screen->root_visual,
				XCB_CW_BACKING_STORE |
				/* XCB_CW_SAVE_UNDER | */
				XCB_CW_OVERRIDE_REDIRECT |
				XCB_CW_EVENT_MASK,
				&(uint32_t const[]) {
					/* advise X server to save contents of
					 * underlying windows so we do not
					 * generate expose events excessively */
					XCB_BACKING_STORE_WHEN_MAPPED,
					/* true, */
					1,
					XCB_EVENT_MASK_EXPOSURE
				});

		/* setup its shape */
		label->shape = xcb_generate_id(conn);

		xcb_create_pixmap(conn,
			/* mask is on or off */
			1,
			label->shape,
			label->window,
			LABEL_WIDTH, LABEL_HEIGHT);

		/* we need a valid pixmap so we use the bounding mask but we
		 * use offsets to move it outside of the area making effective
		 * input region empty */
		xcb_shape_mask(conn,
				XCB_SHAPE_SO_SET, XCB_SHAPE_SK_INPUT,
				label->window,
				LABEL_WIDTH, LABEL_HEIGHT,
				label->shape);
	}

	/* move label to its place and make sure its above base window */
	/* FIXME: base->window may not be a sibling of label (for containers without title) */
	xcb_configure_window_checked(conn, label->window,
			XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y |
			XCB_CONFIG_WINDOW_SIBLING |
			XCB_CONFIG_WINDOW_STACK_MODE,
			&(const uint32_t[]){
				x, y,
				label->base->window,
				XCB_STACK_MODE_ABOVE
			});

	repaint_label(head, label, true);

	xcb_shape_mask(conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_BOUNDING,
			label->window,
			0, 0,
			label->shape);

	xcb_map_window(conn, label->window);
}

#define perr(what) \
	fprintf(stderr, "%s: %s: %s", \
			__func__, \
			what, \
			strerror(errno));

static void
restart(void)
{
	execvp(
#if __linux__
		"/proc/self/exe"
#elif __FreeBSD__
		"/proc/curproc/file"
#elif __sun__
		"/proc/self/path/a.out"
#else
		argv[0]
#endif
		, argv);
	perr("execvp");
}

static void
quit(void)
{
	/* BROADCAST(quit, &(struct quit_args){0}); */

	if (NULL != conn) {
		xcb_flush(conn);
		xcb_key_symbols_free(symbols);
		xcb_disconnect(conn);
	}
}

static void
sigrestart(int signum)
{
	(void)signum;

	restart();
}

static void
sigbye(int signum)
{
	(void)signum;

	exit(EXIT_SUCCESS);
}

static void
intern_atoms(void)
{
	xcb_intern_atom_cookie_t cookies[ARRAY_SIZE(ATOM_NAMES)];

	for (unsigned i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		char const *const name = ATOM_NAMES[i];

		cookies[i] = xcb_intern_atom_unchecked(conn, 0, strlen(name), name);
	}

	for (unsigned i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		xcb_intern_atom_reply_t *const reply =
			xcb_intern_atom_reply(conn, cookies[i], NULL);

		if (NULL == reply)
			continue;

		atoms[i] = reply->atom;
		free(reply);
	}
}

/* return whether search finished */
static bool
find_box_by_name(struct box **optimum, char name[static NAME_LEN])
{
	*optimum = NULL;

	bool complete = true;
	uint8_t const n = strnlen(name, NAME_LEN);

	uint32_t i = 0;
	do {
		struct head *const head = &heads[i];

		struct box *box;
		for_each_box(box, head->root, head->root, {
			if (0 == memcmp(name, box->name, n)) {
				complete &= NAME_LEN <= n || '\0' == box->name[n];
				if (NULL == *optimum || (*optimum)->focus_seq < box->focus_seq)
					*optimum = box;
			}
		})
	} while (++i < num_heads);

	return complete;
}

/* in keychords */
static uint8_t
box_path(struct box const *from, struct box const *to, char path[])
{
	return 1;
}

static bool
box_is_container(struct box const *const box)
{
	return 0 < box->num_children || 0 == box->focus_seq;
}

static bool
name_box(struct box *box)
{
	bool const iscontainer = box_is_container(box);
	/* collect the min distance, max focus_seq */
	struct {
		/* uint8_t distance; */
		uint32_t focus_seq;
	} letters[127];
	/* then search for max distance, min focus_seq */

	memset(letters, 0, sizeof letters);

	/* TODO: if box has name, leave it and check if there are conflicts in its parent */

	assert(0 < num_heads && "no heads but boxes");

	uint8_t const n = strnlen(box->name, NAME_LEN);

	uint32_t i = 0;
	do {
		struct head *const head = &heads[i];

		struct box *to;
		for_each_box(to, head->root, head->root, {
			if (0 == memcmp(to->name, box->name, n)) {
				uint32_t *const p = &letters[(unsigned char)to->name[n]].focus_seq;
				if (to->parent != box->parent) {
					if (*p < to->focus_seq)
						*p = to->focus_seq;
				} else {
					*p = -1;
				}
			}
		})
	} while (++i < num_heads);

	unsigned char optimum = '\0';
	for (uint8_t start = iscontainer ? 'A' : 'a', end = start + ('Z' - 'A');
	     start <= end;
	     ++start)
	{
		if (letters[start].focus_seq < letters[optimum].focus_seq)
			optimum = start;
	}

	if ('\0' == optimum)
		return false;

	box->name[n] = optimum;
	return true;
}

static uint32_t
box_compute_num_columns(struct box const *const box)
{
	switch (box->num_columns) {
	case 0:
	{
		uint32_t i = 1;
		while (i * i < box->num_children)
			++i;
		return i;
	}
		break;

	default:
		return box->num_columns;

	case UINT16_MAX:
		return box->num_children;
	}
}

static void
box_set_position(struct box *const box, int16_t x, int16_t y)
{
	box->position_changed |=
		x != box->x ||
		y != box->y;
	box->x = x;
	box->y = y;
}

static void
box_set_size(struct box *const box, uint16_t width, uint16_t height)
{
	box->layout_changed |=
		width != box->width ||
		height != box->height;
	box->width = width;
	box->height = height;
}

static struct head *
get_focused_head(void)
{
	if (0 == num_heads)
		return NULL;

	struct head *head = heads;
	while (head->root->focus_seq != focus_seq)
		++head;

	return head;
}

static bool
box_get_focus(struct box *const box, struct box **const focus)
{
	if (0 == box->num_children)
		return false;

	struct box *const *child = box->children;
	while (box->focus_seq != (*child)->focus_seq)
		++child;

	*focus = *child;
	return true;
}

static void
set_focus(void)
{
	struct head const *const head = get_focused_head();

	if (NULL == head) {
		assert(0);
		return;
	}

	struct box *box = head->root;
	while (box_get_focus(box, &box))
		;

	
	xcb_get_property_reply_t *reply;
	reply =  xcb_get_property_reply(conn,
				xcb_get_property(conn, 0, box->window,
					XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 0, 0),
				NULL);
	if (reply) {
		printf("title=%.*s\n",
				xcb_get_property_value_length(reply),
				(char *)xcb_get_property_value(reply));
	} else {
		printf("notit\n");
	}

	/* xcb_input_xi_set_focus(conn, box->window, XCB_CURRENT_TIME, devid); */

	/* xcb_input_xi_warp_pointer */
	/* probably requried */
	/* xcb_input_xi_set_client_pointer(conn, box->window, devid); */
	xcb_set_input_focus(conn, XCB_INPUT_FOCUS_POINTER_ROOT, box->window, XCB_CURRENT_TIME);
	xcb_change_property(conn, XCB_PROP_MODE_REPLACE,
		head->screen->root, ATOM_NET_ACTIVE_WINDOW,
		XCB_ATOM_WINDOW, 32, 1, &box->window);

	xcb_warp_pointer(conn,
			XCB_WINDOW_NONE,
			box->window,
			0, 0, 0, 0,
			box->mouse_x,
			box->mouse_y);
}

static void
update_box(struct head *const head, struct box *const box)
{
	static int depth = 0;

	depth += 3;
	printf("%*.supdate box %p\n", depth, "", (void *)box);
	/* rearrange everything */
	if (NULL == box) {
		for (uint32_t i = 0; i < num_heads; ++i) {
			struct head *const head = &heads[i];

			/* will regenerate labels */
			head->num_labels_used = 0;

			update_box(head, head->root);

			while (head->num_labels_used < head->num_labels_mapped) {
				struct label *label = &head->labels[--head->num_labels_mapped];
				xcb_unmap_window(conn, label->window);
			}
			head->num_labels_mapped = head->num_labels_used;
		}

		depth -= 3;

		set_focus();
		return;
	}

	uint32_t mask = 0;
	uint32_t list[7];
	uint8_t i = 0;

	printf("%*.sw=%x pos=(%d,%d) size=(%d,%d)\n", depth, "",
			box->window,
			box->x, box->y,
			box->width, box->height);

	if (box->position_changed) {
		mask |=
			XCB_CONFIG_WINDOW_X |
			XCB_CONFIG_WINDOW_Y;
		list[i++] = box->x;
		list[i++] = box->y;

		/* because of relative position values, layout change must be
		 * propagated so children will be repositioned correctly */
		box->layout_changed = true;
	}

	if (box->layout_changed) {
		if (0 < box->width && 0 < box->height) {
			mask |=
				XCB_CONFIG_WINDOW_WIDTH |
				XCB_CONFIG_WINDOW_HEIGHT;
			list[i++] = box->width;
			list[i++] = box->height;

			if (box->window != XCB_WINDOW_NONE)
				xcb_map_window(conn, box->window);

			uint32_t num_children = box->num_children;

			if (0 == num_children && NULL != box->parent && NULL != box->parent->parent)
				goto not_a_container;

			if (0 == num_children)
				goto empty_container;

			if (0) {
				for (uint32_t i = 0; i < num_children; ++i) {
					struct box *child = box->children[i];

					if (box->focus_seq == child->focus_seq) {
						box_set_position(child, box->x + 0, box->y + 0);
						box_set_size(child, box->width, box->height);
					} else {
						box_set_size(child, 0, 0);
					}
				}
			} else {
				uint16_t num_columns = box_compute_num_columns(box);
				uint16_t width_error = box->width - num_children;
				uint16_t row = (num_children + num_columns - 1) / num_columns;
				int16_t y = 0;
				uint16_t height = box->height / row;
				uint32_t total_weight = 0;
				uint32_t i = 0;

				if (1 == num_columns || 1 == row) {
					for (uint32_t i = 0; i < box->num_children; ++i)
						total_weight += box->children[i]->weight;
				}

				printf("%*.sgrid=%d/%d\n", depth, "", box->num_children, num_columns);
				for (uint16_t width = box->width / num_columns,
					      width_error = box->width - width * num_columns;
				     0 < num_children;
				     --row, y += height, num_children -= num_columns)
				{
					if (num_children < num_columns) {
						num_columns = num_children;
						width = box->width / num_columns;
					}

					int16_t x = 0;
					for (uint16_t col = 0; col < num_columns; ++col, ++i) {
						struct box *child = box->children[i];

						printf("%*.sgrid[y=^%d,x=%d]:\n", depth, "", row, col);

						box_set_position(child, box->x + x + 5, box->y + y + 5);
						box_set_size(child, width - 10, height - 10);

						update_box(head, child);

						x += width;
					}
				}
			}
		not_a_container:;
		} else {
		empty_container:
			if (box->window != XCB_WINDOW_NONE)
				xcb_unmap_window(conn, box->window);
		}
	} else if (1 || box->content_changed) {
		for (uint32_t i = 0; i < box->num_children; ++i) {
			struct box *const child = box->children[i];
			update_box(head, child);
		}
	}

	/* if (e->value_mask & XCB_CONFIG_WINDOW_SIBLING) */
	/* if (e->value_mask & XCB_CONFIG_WINDOW_STACK_MODE) */

	box->position_changed = false;
	box->layout_changed = false;
	box->focus_changed = false;
	box->content_changed = false;
	box->title_changed = false;
	box->focus_lock_changed = false;
	/* box->self_focus_changed = false; */

	if (box->window != XCB_WINDOW_NONE) {
		if (0 < i) {
			mask |= XCB_CONFIG_WINDOW_BORDER_WIDTH;
			list[i++] = box->parent && box->parent->focus_seq == box->focus_seq ? 2 : 2;
		}

		xcb_change_window_attributes(conn, box->window,
				XCB_CW_BORDER_PIXEL, &(uint32_t){box->parent && box->parent->focus_seq == box->focus_seq ? 0xffaf5f : 0xaf5fff});

		printf("%*.sconfigure\n", depth, "");
		xcb_map_window(conn, box->window);
		xcb_configure_window(conn, box->window, mask, list);

		struct label *label = new_label(head);
		label->base = box;
		printf("%*.sname=%.*s\n", depth, "", NAME_LEN, box->name);
		memcpy(label->name, box->name, sizeof box->name);

		show_label(head, label, 2, 0);

	}

	depth -= 3;
}

static void
unparent_box(struct box *const box, bool const forget)
{
	struct box *parent = box->parent;
	struct box **child = &parent->children[0];
	while (box != *child)
		++child;

	/* pos of |box| in its parent */
	uint32_t const pos = child - parent->children;

	memmove(
		child,
		child + 1,
		(--parent->num_children - pos) * sizeof *child
	);

	if (!forget)
		return;

	uint32_t old_focus_idx = parent->focus_seq;
	do {
		uint32_t max_focus_seq = 0;
		for (uint32_t i = 0; i < parent->num_children; ++i) {
			struct box *const child = parent->children[i];
			if (max_focus_seq < child->focus_seq)
				max_focus_seq = child->focus_seq;
		}
		/* we unparented the focused box so maximum dropped */
		if (parent->focus_seq == max_focus_seq)
			return;

		/* maximum may changed but not focused children */
		parent->focus_changed = parent->focus_seq != old_focus_idx;

		old_focus_idx = parent->focus_seq;
		parent->focus_seq = max_focus_seq;
	} while (NULL != (parent = parent->parent));

	focus_seq = 0;

	uint32_t i = 0;
	do {
		struct head *const head = &heads[i];
		if (focus_seq < head->root->focus_seq)
			focus_seq = head->root->focus_seq;
	} while (++i < num_heads);
}

static void auto_delete_box(struct box *box);

/* for containers only */
static void
delete_box(struct box *box)
{
	/* xcb_destroy_window(conn, box->window); */

	struct box *const parent = box->parent;

	parent->layout_changed = true;

	printf("delete %x\n", box->window);
	unparent_box(box, true);

	auto_delete_box(parent);

	free(box);
}

/* for containers only */
static void
auto_delete_box(struct box *box)
{
	/* only care about useless boxes */
	if (0 != box->num_children)
		return;

	struct box *const parent = box->parent;
	/* do not delete root box */
	if (NULL == parent)
		return;

	/* body boxes are special because they can be deleted only if every box on every head is empty */
	if (NULL == parent->parent) {
		uint32_t pos = 0;
		while (parent->children[pos] != box)
			++pos;

		for (uint32_t i = 0; i < num_heads; ++i) {
			struct box const *const body_root = heads[i].root->children[pos];
			if (0 < body_root->num_children)
				return;
		}

		for (uint32_t i = 0; i < num_heads; ++i) {
			struct box *const body_root = heads[i].root->children[pos];
			delete_box(body_root);
		}
	} else {
		delete_box(box);
	}
}

static struct head *
get_head_by_root_window(xcb_window_t root)
{
	struct head *head = heads;

	while (root != head->screen->root)
		++head;

	return head;
}

static struct head *
get_head_by_root(struct box const *const root)
{
	assert(NULL == root->parent);

	struct head *head = heads;

	while (root != head->root)
		++head;

	return head;
}

/* box must not be a root because box->parent == NULL => new box */
static bool
move_box_(struct box *into, uint32_t const pos, struct box *box)
{
	struct box *const boxparent = box->parent;

	/* already its parent */
	if (into == boxparent)
		goto insert_child;

	/* first check if we can make place for |box| in |into| */
	struct box *const new = realloc(into, sizeof *into + sizeof box * (into->num_children + 1));
	if (NULL == new)
		/* FIXME: undo creation body */
		return false;

	++new->num_children;

	/* unparent |box| */
	if (NULL != boxparent)
		unparent_box(box, into != boxparent);

	/* if |into| location changed because of realloc(), update all
	 * references to it */
	if (new != into) {
/* update |var| if that points to the old reference of |new| */
#define UPDATE_REF(var) if (into == (var)) var = new;

		/* update global references */
		UPDATE_REF(lastest_input);

		/* update parent references */
		if (NULL != new->parent) {
			struct box **child = new->parent->children;

			while (into != *child)
				++child;

			*child = new;
		} else {
			/* no parent means it is attached to head */
			get_head_by_root(into)->root = new;
		}

		/* update children references */
		for (uint32_t i = 0; i < into->num_children; ++i) {
			struct box *const child = into->children[i];
			child->parent = new;
		}

#undef UPDATE_REF

		into = new;
	}

	if (1 == into->num_children)
		into->focus_seq = box->focus_seq;

insert_child:
	assert(pos <= into->num_children - 1);
	into->layout_changed = true;

	/* shift children downwards */
	memmove(
		into->children + pos + 1,
		into->children + pos,
		(into->num_children - 1 - pos) * sizeof *into->children
	);

	into->children[pos] = box;
	box->parent = into;

	if (NULL != boxparent) {
		boxparent->layout_changed = true;
		auto_delete_box(boxparent);
	}

	return true;
}

static bool
move_box(struct box *into, uint32_t const pos, struct box *box)
{
	if (NULL == into->parent) {
		if (into == box->parent) {
			uint32_t body_pos = 0;
			while (into->children[body_pos] != box)
				++body_pos;

			for (uint32_t i = 0; i < num_heads; ++i) {
				struct box *const body_root = heads[i].root;
				(void)move_box_(body_root, pos, body_root->children[body_pos]);
			}

			return true;
		}

		/* |into| may move, we must save head that surely points to that */
		struct head *const saved_head = get_head_by_root(into);
		char *name;

		for (uint32_t i = 0; i < num_heads; ++i) {
			struct box *const body_root = calloc(1, sizeof *body_root);

			if (0 == i) {
				name_box(body_root);
				name = body_root->name;
			} else {
				/* copy assigned name for further body_roots */
				memcpy(body_root->name, name, sizeof body_root->name);
			}

			if (!move_box_(heads[i].root, pos, body_root)) {
				assert(0);
#if 0
				while (i > 0)
					delete_box(heads[--i].root->children[pos]);

				return false;
#endif
			}
		}

		/* NOTE: |into| is invalid */

		return move_box_(saved_head->root->children[pos], 0, box);
	}

	return move_box_(into, pos, box);
}

#define DEFINE_BOX_SETTER(prop) \
	static void \
	box_set_##prop(struct box *const box, bool enable) \
	{ \
		if (enable != box->prop) { \
			box->prop = enable; \
			box->prop##_changed = true; \
		} \
	}

DEFINE_BOX_SETTER(focus_lock)
DEFINE_BOX_SETTER(self_focus)

static struct box *
box_screen(xcb_screen_t const *const screen)
{
	struct box *box = calloc(1, sizeof *box);

	box->x = 0;
	box->y = 0;
	box->width = screen->width_in_pixels;
	box->height = screen->height_in_pixels;

	box->window = screen->root;
	box->focus_lock = true;
	box->layout_changed = true;
	box->focus_lock_changed = true;

	strncpy(box->name, "Q", sizeof box->name);

	return box;
}

#if 0
static struct box
new_box(void)
{
	struct box *box = calloc(1, sizeof *box);
	if (NULL == box)
		return NULL;

	box->window = xcb_generate_id(conn);

	xcb_void_cookie_t const cookie =
		xcb_create_window_checked(conn,
			XCB_COPY_FROM_PARENT, /* depth */
			box->window, /* window id */
			screen->root, /* parent */
			0, /* x */
			0, /* y */
			1000, /* width */
			700, /* height */
			0, /* border width */
			XCB_WINDOW_CLASS_INPUT_OUTPUT, /* class */
			screen->root_visual, /* visual */
			0,
			NULL);
	if (!check_cookie(cookie)) {
		return NULL;
	}

	xcb_change_window_attributes(conn, box->window, XCB_CW_EVENT_MASK, &(uint32_t const[]){
		XCB_EVENT_MASK_STRUCTURE_NOTIFY |
		XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY |
		XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
	});

	return box;
}
#endif

static void
box_focus(struct box *box)
{
	++focus_seq;

	do {
		box->focus_changed |= box->focus_seq != focus_seq;
		box->focus_seq = focus_seq;
	} while (NULL != (box = box->parent));
}

/* manage window */
static struct box *
box_window(xcb_window_t const window, bool focus)
{
	struct box *box = calloc(1, sizeof *box);


	xcb_void_cookie_t const cookie =
		xcb_change_window_attributes_checked(conn, window, XCB_CW_EVENT_MASK, &(uint32_t const []){
			XCB_EVENT_MASK_KEY_PRESS
		});
	if (!check_cookie(cookie)) {
		fprintf(stderr, "heck\n");
	}

	xcb_get_property_cookie_t cookies[2];
	void *values;
	xcb_get_property_reply_t *reply;
	xcb_window_t transient_for = XCB_WINDOW_NONE;
	xcb_atom_t state = XCB_ATOM_NONE;


	cookies[0] = xcb_get_property(conn, 0, window, ATOM_NET_WM_STATE,         XCB_ATOM_ATOM,   0, 1);
	cookies[1] = xcb_get_property(conn, 0, window, XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW, 0, 1);
	/* cookies[2] = xcb_get_property(conn, 0, window, XCB_ATOM_WM_CLASS, XCB_ATOM_STRING, 0, 1); */

	if (NULL != (reply = xcb_get_property_reply(conn, cookies[0], NULL))) {
		state = *(xcb_atom_t const *)xcb_get_property_value(reply);
		free(reply);
	}
	if (NULL != (reply = xcb_get_property_reply(conn, cookies[1], NULL))) {
		transient_for = *(xcb_window_t const *)xcb_get_property_value(reply);
		free(reply);
	}

	struct head *const head = get_focused_head();
	xcb_screen_t *const screen = head->screen;
	struct box *parent = head->root;

	while (!parent->self_focus &&
	       box_get_focus(parent, &parent))
		;

	assert(0 == parent->num_children);

	if (0 < parent->focus_seq &&
	    NULL != parent->parent)
		parent = parent->parent;

	printf("box %d\n", window);
	box->window = window;
	/* box->frame = XCB_WINDOW_NONE; */

	move_box(parent, parent->num_children, box);
#if 0
	if (state == ATOM_NET_WM_STATE_MODAL &&
			find_box_by_window(transient_for
			) {

	}
#endif
	if (focus)
		box_focus(box);

	name_box(box);
	return box;
}

#if 0
static void
manage_client(xcb_window_t window)
{
	printf("manage %d\n", window);

	xcb_void_cookie_t cookie;

	/* tell X server not to destroy this subwindow when our window manager
	 * terminates; such windows will be reparented to root */
	xcb_change_save_set(conn, XCB_SET_MODE_INSERT, window);

	cookie = xcb_reparent_window_checked(conn, window, box->window, 0, 0);
	if (!check_cookie(cookie)) {
		return;
	}

	cookie = xcb_map_window_checked(conn, window);
	if (!check_cookie(cookie)) {
		/* window may be destroyed */
		return;
	}

	cookie = xcb_map_window_checked(conn, box->window);
	if (!check_cookie(cookie)) {
		return;
	}

	auto_name_box(box);

	printf("managed\n");
}
#endif

static void
setup_sighandlers(void)
{
	struct sigaction sa;

	sigfillset(&sa.sa_mask);
	sa.sa_flags =  /* SA_RESTART |  */ SA_NOCLDSTOP | SA_NOCLDWAIT;

	sa.sa_handler = sigrestart;
	sigaction(SIGHUP, &sa, NULL);

	sa.sa_handler = sigbye;
	sigaction(SIGINT, &sa, NULL);
	sigaction(SIGTERM, &sa, NULL);

	/* automatically clean up children */
	sa.sa_handler = SIG_DFL;
	sigaction(SIGCHLD, &sa, NULL);
}

static void
fd_set_cloexec(int const fd)
{
	if (-1 == fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC))
		perr("fcntl");
}

static void
connect_server(void)
{
	/* connect to X server */

	while (xcb_connection_has_error((conn = xcb_connect(NULL, NULL)))) {
		if (NULL != getenv("DISPLAY")) {
			fprintf(stderr, "could not open display %s\n",
					getenv("DISPLAY"));
		} else {
			fprintf(stderr, "DISPLAY is not set\n");

			if (!setenv("DISPLAY", ":0", 0)) {
				fprintf(stderr, "using DISPLAY=:0\n");
				continue;
			}
		}

		exit(EXIT_FAILURE);
	}

	fd_set_cloexec(xcb_get_file_descriptor(conn));

	symbols = xcb_key_symbols_alloc(conn);
	/* BROADCAST(connected, &(struct connected_args){ }); */

#if 0
	xcb_input_list_input_devices_reply_t *in =
			xcb_input_list_input_devices_reply(conn, xcb_input_list_input_devices_unchecked(conn), NULL);
	printf("device count=%d strlen=%d\n",
			xcb_input_list_input_devices_infos_length(in),
			xcb_input_list_input_devices_names_length(in));

	;
	for (xcb_input_device_info_iterator_t iter = xcb_input_list_input_devices_devices_iterator(in);
	     0 < iter.rem;
	     xcb_input_device_info_next(&iter))
	{
		xcb_input_device_info_t *const device = iter.data;

		printf("  id=%d\n", device->device_id);
		printf("  use=%d %d\n", device->device_use,
				device->device_use == XCB_INPUT_DEVICE_USE_IS_X_EXTENSION_POINTER ||
				device->device_use == XCB_INPUT_DEVICE_USE_IS_X_POINTER);
	}
#else
	xcb_input_xi_query_device_reply_t *devices =
		xcb_input_xi_query_device_reply(conn,
			xcb_input_xi_query_device_unchecked(conn,
				XCB_INPUT_DEVICE_ALL_MASTER),
			NULL);

	num_hands = xcb_input_xi_query_device_infos_length(devices);
	hands = calloc(num_hands, sizeof *hands);

	for (xcb_input_xi_device_info_iterator_t iter = xcb_input_xi_query_device_infos_iterator(devices);
	     0 < iter.rem;
	     xcb_input_xi_device_info_next(&iter))
	{
		xcb_input_xi_device_info_t *const device = iter.data;

		printf("  id=%d\n", device->deviceid);
		printf("  type=%d\n", device->type);
		printf("  ismp=%d\n", device->type == XCB_INPUT_DEVICE_TYPE_MASTER_POINTER);
		printf("  attachment=%d\n", device->attachment);
		printf("  name=%s\n", xcb_input_xi_device_info_name(device));
		printf("\n");

		switch (device->type)
		{
			case XCB_INPUT_DEVICE_TYPE_MASTER_POINTER:
			{

			}
				break;

			case XCB_INPUT_DEVICE_TYPE_MASTER_KEYBOARD:
			{

			}
				break;
		}

	}

	free(devices);
#endif

#if 0
	xcb_xrm_database_t *database = xcb_xrm_database_from_default(conn);
	if (NULL == database) {

	}

	char *value;
	if (0 <= xcb_xrm_resource_get_string(database, "Xft.dpi", NULL, &value)) {
		printf("val=%s\n", value);
	}

	xcb_xrm_database_free(database);
#endif
}

static void
manage_screen_windows(xcb_screen_t *screen)
{
	xcb_query_tree_reply_t *const reply =
		xcb_query_tree_reply(conn,
			xcb_query_tree_unchecked(conn, screen->root),
			NULL);
	if (NULL == reply)
		return;

	xcb_window_t const *const children = xcb_query_tree_children(reply);
	int const num_children = xcb_query_tree_children_length(reply);

	xcb_get_window_attributes_cookie_t *const cookies =
		malloc(num_children * sizeof *cookies);
	if (NULL == cookies)
		goto out_free_reply;

	for (int i = 0; i < num_children; ++i)
		cookies[i] = xcb_get_window_attributes_unchecked(conn, children[i]);

	for (int i = 0; i < num_children; ++i) {
		xcb_get_window_attributes_reply_t *const reply =
			xcb_get_window_attributes_reply(conn, cookies[i], NULL);

		if (NULL == reply)
			continue;

		if (!reply->override_redirect &&
		    reply->map_state == XCB_MAP_STATE_VIEWABLE)
			box_window(children[i], true);

		free(reply);
	}

	free(cookies);

out_free_reply:
	free(reply);
}

static void
setup_screen_cursor(xcb_screen_t *screen)
{
	xcb_cursor_context_t *ctx;

	if (0 <= xcb_cursor_context_new(conn, screen, &ctx)) {
		xcb_cursor_t const cursor = xcb_cursor_load_cursor(ctx, DEFAULT_CURSOR_NAME);

		xcb_change_window_attributes(conn, screen->root,
				XCB_CW_CURSOR, &cursor);

		xcb_free_cursor(conn, cursor);
		xcb_cursor_context_free(ctx);
	}
}

static void
setup_screen_keys(xcb_screen_t *screen)
{
#if 0
	xcb_key_symbols_t *symbols = xcb_key_symbols_alloc(conn);

	xcb_keycode_t *const keycodes = xcb_key_symbols_get_keycode(symbols, XK_space);

	 

	for (xcb_keycode_t const *keycode = keycodes;
	     XCB_NO_SYMBOL != *keycode;
	     ++keycode)
		xcb_grab_key(conn, 1, screen->root,
				XCB_NONE, *keycode,
				XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC);
	free(keycodes);

	xcb_key_symbols_free(symbols);
#endif

	/* xcb_grab_key(conn, 1, screen->root,
			0, 133,
			XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); */

	xcb_grab_key(conn, false, screen->root,
			XCB_MOD_MASK_4, XCB_GRAB_ANY,
			XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC);

	xcb_grab_button(conn, false, screen->root,
			XCB_EVENT_MASK_BUTTON_PRESS,
			XCB_GRAB_MODE_SYNC, XCB_GRAB_MODE_ASYNC,
			XCB_NONE,
			XCB_NONE,
			XCB_BUTTON_INDEX_1,
			XCB_BUTTON_MASK_ANY);
}

static void
manage_screen(xcb_screen_t *screen)
{
	xcb_void_cookie_t const cookie =
		xcb_change_window_attributes_checked(conn, screen->root,
				XCB_CW_EVENT_MASK, &(uint32_t const []){
			/* XCB_EVENT_MASK_BUTTON_PRESS |
			XCB_EVENT_MASK_BUTTON_RELEASE | */
			XCB_EVENT_MASK_EXPOSURE |
			XCB_EVENT_MASK_FOCUS_CHANGE |
			XCB_EVENT_MASK_KEY_PRESS |
			XCB_EVENT_MASK_KEY_RELEASE |
			XCB_EVENT_MASK_PROPERTY_CHANGE |
			XCB_EVENT_MASK_STRUCTURE_NOTIFY |
			XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY |
			XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
		});
	if (!check_cookie(cookie)) {
		fprintf(stderr, "possibly other window manager is run\n");
		return;
	}

	xcb_input_xi_select_events(conn, screen->root, 1,
		(xcb_input_event_mask_t const *)&(struct mask_values {
				xcb_input_event_mask_t mask;
				uint32_t values[1];
			} const){
			{
				.deviceid = XCB_INPUT_DEVICE_ALL,
				.mask_len = ARRAY_SIZE(((struct mask_values *)0)->values)
			},
			{
				XCB_INPUT_XI_EVENT_MASK_HIERARCHY
			}
		}
	);

	setup_screen_cursor(screen);

	manage_screen_windows(screen);

	setup_screen_keys(screen);

}

static void
manage_each_screen(void)
{
	/* prevent windows from changing */
	xcb_grab_server(conn);

	xcb_setup_t const *const setup = xcb_get_setup(conn);

	xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);

	num_heads = xcb_setup_roots_length(setup);
	heads = calloc(num_heads, sizeof *heads);

	for (uint32_t i = 0; 0 < iter.rem; ++i, xcb_screen_next(&iter)) {
		xcb_screen_t *const screen = iter.data;
		struct head *head = &heads[i];

		head->screen = screen;
		head->root = box_screen(head->screen);

		for (xcb_depth_iterator_t depth_iter = xcb_screen_allowed_depths_iterator(screen);
		     0 < depth_iter.rem;
		     xcb_depth_next(&depth_iter))
		{
			for (xcb_visualtype_iterator_t visual_iter = xcb_depth_visuals_iterator(depth_iter.data);
			     0 < visual_iter.rem;
			     xcb_visualtype_next(&visual_iter))
			{
				if (screen->root_visual == visual_iter.data->visual_id) {
					head->visual_type = visual_iter.data;
					goto visual_found;
				}
			}
		}
	visual_found:;
	}

	for (uint32_t i = 0; i < num_heads; ++i)
		manage_screen(heads[i].screen);

	xcb_ungrab_server(conn);

	/* char name[NAME_LEN] = { 'Q' };
	body_new(name); */
}

static void
handle_error(xcb_generic_error_t const *const event)
{
	print_error(event);
}

static void
handle_property_notify(xcb_property_notify_event_t const *const event)
{
	char *atom = xcb_get_atom_name_name(
		xcb_get_atom_name_reply(conn, xcb_get_atom_name(conn, event->atom), NULL)
	);
	printf("w=%x %s\n", event->window, atom);
}

static void
handle_focus_out(xcb_focus_out_event_t const *const event)
{
	(void)event;
	set_focus();
}

#define MOD_MASK ( \
	XCB_MOD_MASK_SHIFT   | \
	XCB_MOD_MASK_CONTROL | \
	XCB_MOD_MASK_1       | \
	XCB_MOD_MASK_2       | \
	XCB_MOD_MASK_3       | \
	XCB_MOD_MASK_4       | \
	XCB_MOD_MASK_5       )

static void
handle_key_press(xcb_key_press_event_t *const event)
{
	xcb_keysym_t const sym = xcb_key_press_lookup_keysym(symbols, event, event->state & XCB_MOD_MASK_SHIFT);

	if (!(event->state & XCB_MOD_MASK_4)) {
		printf("noo\n");
		allow_map_focus = XKB_KEY_Return == sym;
		return;
	} else {
		allow_map_focus = true;
	}

	printf("%c (%ux)\n", sym, sym);

	struct head *const head = get_focused_head();
	if (NULL == head)
		return;

	struct box *focus = head->root;
	while (!focus->self_focus &&
	       box_get_focus(focus, &focus))
		;

	switch ((MOD_MASK & event->state) & ~(XCB_MOD_MASK_4 | XCB_MOD_MASK_SHIFT)) {
	case 0:
		if (XKB_KEY_a <= sym && sym <= XKB_KEY_z) {
			struct box *box;

			user_input[0] = 'a' + (sym - XKB_KEY_a);
			if (find_box_by_name(&box, user_input)) {
				/* only if it is the first character of the input */
				bool const try_with_control = 0 == NAME_LEN || '\0' == user_input[1];

				memset(user_input, 0, sizeof user_input);

				if (NULL != box) {
					box_focus(box);
					update_box(NULL, NULL);
				}

				if (!try_with_control)
					return;
			}
		} else if (XKB_KEY_Return == sym) {
			if (0 == fork()) {
				setsid();
				execlp(getenv("TERMINAL"), getenv("TERMINAL"), NULL);
				perr("execlp");
				_exit(127);
			}

			return;
		}
		/* fall through */
	case XCB_MOD_MASK_CONTROL:
		switch (sym) {
		case XKB_KEY_Return:

			printf("aa\n");

			break;

		case XKB_KEY_x:
			--focus->num_columns;
			focus->layout_changed = true;
			break;

		case XKB_KEY_a:
			++focus->num_columns;
			focus->layout_changed = true;
			break;

		case XKB_KEY_r:
			break;

		case XKB_KEY_m:
			break;

		case XKB_KEY_l:
			/* focus lock makes sense only if we have more children */
			while (focus->num_children <= 1)
				if (NULL == (focus = focus->parent))
					return;
			box_set_focus_lock(focus, !focus->focus_lock);
			break;

		case XKB_KEY_f:
			break;

		case XKB_KEY_n:
			break;
		}

		update_box(head, NULL);
		return;
	}
}

static void
handle_unmap_notify(xcb_unmap_notify_event_t const *const event)
{
	for (uint32_t i = 0; i < num_heads; ++i) {
		struct head const *const head = &heads[i];

		for (struct box *start = head->root, *box;
		     NULL != (box = find_box_by_window(head->root, start, event->window));)
		{
			start = box->parent;
			assert(NULL != start && "root box unmapped by X server");
			delete_box(box);
		}
	}

	update_box(NULL, NULL);
}

static void
handle_map_request(xcb_map_request_event_t const *const event)
{
	box_window(event->window, allow_map_focus);

	update_box(NULL, NULL);
}

static void
handle_configure_notify(xcb_configure_notify_event_t const *const event)
{
	for (uint32_t i = 0; i < num_heads; ++i) {
		struct head const *const head = &heads[i];
		if (event->window != head->screen->root)
			continue;

		box_set_size(head->root, event->width, event->height);
		update_box(NULL, NULL);

		break;
	}
}

static void
handle_expose(xcb_expose_event_t const *const event)
{
	for (uint32_t i = 0; i < num_heads; ++i) {
		struct head const *const head = &heads[i];
		for (uint32_t j = 0; j < head->num_labels_used; ++j) {
			struct label *label = &head->labels[j];

			if (label->window != event->window)
				continue;

			repaint_label(head, label, false);

			return;
		}
	}
}

static void
handle_button_press(xcb_button_press_event_t const *const event)
{
	printf("press %d\n", event->state);

	bool propagate = true;
	if (XCB_MOD_MASK_4 == (event->state & MOD_MASK)) {
		struct head *head = get_head_by_root_window(event->root);
		if (NULL != head) {
			struct box *box = find_box_by_window(head->root, head->root, event->child);
			if (NULL != box && !box_is_container(box)) {
				propagate = false;
				box_focus(box);
				update_box(NULL, NULL);
			}
		}
	}

	xcb_allow_events(conn,
			propagate
				? XCB_ALLOW_REPLAY_POINTER
				: XCB_ALLOW_SYNC_POINTER,
			XCB_CURRENT_TIME);

}

static void
handle_mapping_notify(xcb_mapping_notify_event_t *const event)
{
	for (uint32_t i = 0; i < num_heads; ++i) {
		struct head const *const head = &heads[i];

		xcb_ungrab_key(conn, XCB_GRAB_ANY, head->screen->root, XCB_BUTTON_MASK_ANY);

		xcb_refresh_keyboard_mapping(symbols, event);

		setup_screen_keys(head->screen);
	}
}

static void
handle_client_message(xcb_client_message_event_t const *const event)
{
	(void)event;
}

static void
handle_input_hierarchy(xcb_input_hierarchy_event_t const *const event)
{
	if (!(event->flags & (
		XCB_INPUT_HIERARCHY_MASK_MASTER_ADDED |
		XCB_INPUT_HIERARCHY_MASK_MASTER_REMOVED
	)))
		return;
}

int
main(int _argc, char *_argv[])
{
	argc = _argc, argv = _argv;
	atexit(quit);

	setup_sighandlers();

	/* BROADCAST(start, &(struct start_args){0}); */

	connect_server();

	intern_atoms();

	manage_each_screen();

	update_box(NULL, NULL);

	/* xcb_flush + wait for all replies */
	xcb_aux_sync(conn);
	/* xcb_flush(conn); */

	for (xcb_generic_event_t *event;
	     NULL != (event = xcb_wait_for_event(conn));
	     free(event))
	{
		/* printf("event = %s\n", xcb_event_get_label(event->response_type)); */

		switch (XCB_EVENT_RESPONSE_TYPE(event)) {
		case 0:
			handle_error((void *)event);
			/* fall through */
		default:
			/* no actions taken so we do not need to flush */
			continue;
/*
create_notify:
			if (transient for):
			xcb_change_window_attributes(XCB_CW_OVERRIDE_REDIRECT)
			*/

		case XCB_MAP_REQUEST:
			handle_map_request((void *)event);
			break;

		case XCB_CONFIGURE_NOTIFY:
			handle_configure_notify((void *)event);
			break;

		case XCB_MAPPING_NOTIFY:
			handle_mapping_notify((void *)event);
			break;

		case XCB_UNMAP_NOTIFY:
			handle_unmap_notify((void *)event);
			break;

		case XCB_EXPOSE:
			handle_expose((void *)event);
			break;

		case XCB_BUTTON_PRESS:
			handle_button_press((void *)event);
			break;

		case XCB_KEY_PRESS:
			handle_key_press((void *)event);
			break;

		case XCB_FOCUS_OUT:
			handle_focus_out((void *)event);
			break;

		case XCB_PROPERTY_NOTIFY:
			handle_property_notify((void *)event);
			break;

		case XCB_CLIENT_MESSAGE:
			handle_client_message((void *)event);
			break;

		/* XInput */
		case XCB_INPUT_HIERARCHY:
			handle_input_hierarchy((void *)event);
			break;

		}

		xcb_flush(conn);
	}

	return EXIT_SUCCESS;
}
