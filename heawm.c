#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <memory.h>
#include <poll.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
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

/* motif: https://gitlab.gnome.org/GNOME/gtk/-/blob/master/gdk/x11/MwmUtil.h */
/* https://people.gnome.org/~tthurman/docs/metacity/xprops_8h-source.html */

#include "atoms.h"

#define NAME_LEN 2
/* sizeof array... but in elements */
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))

#ifdef __GNUC__
# define maybe_unused __attribute__((unused))
#else
# define maybe_unused
#endif

#if defined(__GNUC__) || defined(clang)
# define unreachable __builtin_unreachable()
#else
# define unreachable
#endif

/** clock we use for measuring elapsed time for hands */
#ifdef CLOCK_MONOTONIC_COARSE
# define HAND_TIMEOUT_CLOCK CLOCK_MONOTONIC_COARSE
#else
# define HAND_TIMEOUT_CLOCK CLOCK_MONOTONIC
#endif

struct box_pointer_xy {
	xcb_input_fp1616_t x, y; /* relative pointer location */
};

/** a box that holds smaller boxes. you surely know such box so i do not have
 * to introduce it better */
struct box {
	/** box location relative to the root box */
	int16_t x, y;
	/** dimensions */
	uint16_t width, height;

	/** user requested boundaries */
	int16_t user_x, user_y;
	uint16_t user_width, user_height;

	/** client window */
	xcb_window_t window;
	xcb_window_t frame;

	/** number of |children| */
	uint16_t num_children,
	/** internal variable used to track progress of recursive searching
	 *
	 * - it does not occupy too much space and we do not have to
	 *   dynamically allocate more memory for backtracking or track maximum
	 *   possible depth
	 * - it also makes very convenient to continue the walk where we left
	 *   off */
	         iter;

	/** number of columns
	 *
	 * namely:
	 * - 0: auto,
	 * - 1: column,
	 * - -1: row,
	 * - other: fixed, user set */
	uint16_t num_columns;
	/* TODO: maybe a different name; label/id? */
	char name[NAME_LEN]; /* permanent name */
	/** relative size to siblings
	 *
	 * used only when box has one variable dimension, i.e. when parent
	 * layout consits of a single row or column. */
	uint8_t weight;
	/** the body */
	uint8_t body;

	/* TODO: different name: attributes? */
	char *title; /* stuff that desribes window (X11 window title or monitor name) */

	/* we always update the whole scene. these variables help to track what
	 * changed */
	bool position_changed: 1, /* box repositioned */
	     layout_changed: 1, /* any of the children's boundary box changed */
	     focus_changed: 1, /* focused child changed */
	     content_changed: 1, /* set by children to indicate parent should
	                            descend with update (means that this flag
	                            must be propagated upwards until root) */
	     title_changed: 1,
	     close_by_force: 1,

	     focus_hand_changed: 1,
	     concealed: 1, /* show only when has focus inside */
	     focus_lock: 1; /* swap newly focused window with focused window */
	/** tracking focus without needing heavy linked-lists
	 *
	 * (1) {never focused box}->focus_seq := 0
	 * (2) {old focus}->focus_seq < {new focus}->focus_seq
	 * (3) parent->focus_seq := MAX(children[..]->focus_seq)
	 *  => {focused box(es)}->focus_seq := root->focus_seq
	 *  => {focused child(ren)}->focus_seq := parent->focus_seq
	 */
	uint32_t focus_seq;

	/** which hand focused the box (last time)
	 *
	 * because simultenously multiple hands can focus a box, it is only
	 * valid if box->focus_seq != root->focus_seq. in these cases
	 * focus_hand is garbage and to find out which hand(s) hold(s) the
	 * focus: hands[..]->input_focus ?= box.
	 */
	uint8_t focus_hand;

	/** the bigger box */
	struct box *parent;

	/** contained boxes */
	struct box *children[];
	/* struct box_pointer_xy pointers[num_hands]; */
};

/* root's children are XRandR monitors from all screens
 * monitor
 *   parent = screen root */
static struct box *root;
static struct box const EMPTY_BOX;

struct label {
	struct box *base;
	xcb_window_t window;
	xcb_window_t shape;
	int x: 3,
	    y: 3;
	/* <= num_hands */
	uint8_t nth;
	uint8_t hand;
	char name[NAME_LEN];
};

/* static uint32_t num_heads;
static struct head *heads; */

struct body {
	xcb_screen_t *screen;
	xcb_visualtype_t *visual_type;

	/* labels are created for a specific screen */
	uint32_t num_labels_used;
	uint32_t num_labels_mapped;
	uint32_t num_labels;
	struct label *labels;
};

static uint8_t num_bodies;
static struct body *bodies;
static int default_screen;

static int next_timeout_ms = -1;

/* user */
struct hand {
	/** keyboard and pointer are always in pair. except if not. */
	xcb_input_device_id_t pointer; /** pointer device */
	xcb_input_device_id_t keyboard; /** key device */

	/** focus freshly mapped related windows */
	bool auto_focus;

	/** user input buffer for longer box names */
	char user_input[NAME_LEN];

	/** number of labels owned by this hand */
	uint32_t num_labels;

	/** time until |hand_timed_out()| is called; -1 if disarmed */
	int timeout_ms;

	/** show box labels in default mode */
	bool label_boxes;

	/** |mode| related state */
	struct {
		struct box *box;
		uint8_t dir;
	} mode_base;
	enum {
		mode_default,
		mode_default_label_boxes,
		mode_setcolumns,
		mode_move_a,
		mode_move_b,
		mode_size_side,
		mode_name,
	} mode;

	/** box that last time received keyboard input (only non-container);
	 * we also store the box that was focused before last time since if
	 * we start typing into the currently focused window that is not
	 * really interesting to remember for */
	struct box *latest_input[2];

	/* box that last time received keyboard input (only non-container) */
	/* box focus_seq history would need to be hand aware however
	 * maintaining a proper stack for every hand. it could be achieved by
	 * adding a prev and next pointers to each box per hand OR storing a
	 * per hand sequence number. (A)
	 *
	 * a cheaper solution would be to store a bitvector per box to indicate
	 * that in what hands box->focus_seq is valid. however it's bad because
	 * after all hands stepped of the window and B again focuses the common
	 * window we can do two things:
	 * - clear all previous hand references, thus clearing them from hands
	 *   sequence history meaning that every unfocused window can have only
	 *   a single owner. focused ones can have one or more but then it's
	 *   enough to store a hand_index instead of a bit vector and in case
	 *   of a focused window looking up hands which ones has the window
	 *   focused. (B)
	 * - keep: now w4 dies and A focuses W, however it should be w3 then w1.
	 * A->W<-B,  w1<-A W B->w2,  w1 w3 w4<-A W<-B
	 *
	 * But what is the difference between (A) and (B)? Only at the handling
	 * of cross focused windows. That's the difference between O(U) and
	 * O(1)? Does not worth it in the light of multiple master devices are
	 * mainly for separate work (but it still not agaist it). + if
	 * everybody uses her own little workspace there is no differenc + a
	 * few common windows can be remembered by its name.
	 *
	 * however still there is a hanging issue: what to do with different
	 * window labels regarding every hand maintains a separate focus stack
	 * (does not count whether we would choose (A) or (B)).
	 * - jumping to the more lately used letter is good.
	 * - random letters popping up is bad => may be shorter (or longer)
	 *   than needed for some users.
	 * - combining the above two: display the longest needed for all hands
	 *   and accept the shortest.
	 * - improving the above: consider only hands that requested labels.
	 * - users should not look at the small popping up labels so they do not get confused
	 * */
	 /* either a window or a box */
	struct box *input_focus;

	/* user focused box that may be a container since it will not
	 * receive keyboard events... or any at all. as soon as user starts
	 * typing focus := input_focus */
	struct box *focus;
};

/** number of |hands| */
static uint8_t num_hands;
/** stores list of humans and/or EBEs who have at least a keyboard or a pointer
 * device (a master device pair) for controlling X */
static struct hand *hands;

/* XCB_ATOM_WM_HINTS */
/* XCB_ATOM_WM_NAME */
/* XCB_ATOM_WM_CLASS */
/* XCB_ATOM_WM_TRANSIENT_FOR */
/* https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/ICCCM/icccm.html */

static char const *const ATOM_NAMES[] =
{
	"_NET_WM_STATE",
	"_NET_WM_STATE_MODAL",
	"_NET_WM_STATE_DEMANDS_ATTENTION",
	"_NET_WM_STATE_FULLSCREEN",
	"_NET_WM_TRANSIENT_FOR",
	"WM_CLIENT_LEADER",
	"_NET_WM_PID",
	"_NET_WM_NAME",
	"WM_DELETE_WINDOW",
	"WM_PROTOCOLS",
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
/** resolved |ATOM_NAMES| */
static xcb_atom_t atoms[ARRAY_SIZE(ATOM_NAMES)];

/** connection to X server */
static xcb_connection_t *conn;
/** user preferred screen; provided by $DISPLAY */
static int preferred_screen;
/** major opcode of XInput extension */
static uint8_t xi_opcode;
/** beginning of XRandR event range */
static uint8_t randr_first_event;
static xcb_key_symbols_t *symbols;

/** */
static int argc;
static char **argv;

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
repaint_label(struct label const *const label, bool shape)
{
	char name[NAME_LEN + 1];

	memcpy(name, label->name, sizeof label->name);
	name[NAME_LEN] = '\0';

	struct body const *const body = &bodies[label->base->body];
	cairo_surface_t *const surface = shape
		? cairo_xcb_surface_create_for_bitmap(conn, body->screen, label->shape, LABEL_WIDTH, LABEL_HEIGHT)
		: cairo_xcb_surface_create(conn, label->window, body->visual_type, LABEL_WIDTH, LABEL_HEIGHT);
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
new_label_for(struct box *const box)
{
	struct label *label;
	struct body *const body = &bodies[box->body];

	if (body->num_labels_used == body->num_labels) {
		size_t const new_size = (body->num_labels * 8 / 5/*golden ratio*/) + 1;
		label = realloc(body->labels, new_size * sizeof *label);
		if (NULL == label)
			return NULL;
		body->labels = label;
		body->num_labels = new_size;

		/* initialize new labels */
		for (uint32_t i = body->num_labels_used;
		     i < body->num_labels;
		     ++i)
		{
			struct label *label = &body->labels[i];
			label->window = XCB_WINDOW_NONE;
		}
	}

	label = &body->labels[body->num_labels_used++];
	label->base = box;
	return label;
}

static void
print_error(xcb_generic_error_t const *const error, char const *const request)
{
	fprintf(stderr, "%s:%s: X error: %s (serial number: %d, opcode: %d/%d)\n",
			__FILE__, request,
			xcb_event_get_error_label(error->error_code),
			error->sequence,
			error->major_code, error->minor_code);
	assert(0);
}

maybe_unused
static bool
check_cookie(xcb_void_cookie_t const cookie, char const *const request)
{
	xcb_generic_error_t *error;

	if (NULL != (error = xcb_request_check(conn, cookie))) {
		print_error(error, request);
		free(error);
		assert(!"check_cookie");
		return false;
	} else {
		return true;
	}
}

#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

#ifndef HEAWM_NDEBUG
# define CHECK(request, ...) check_cookie(request##_checked(__VA_ARGS__), STRINGIFY(__LINE__) ": " #request)
#else
# define CHECK(request, ...) (request(__VA_ARGS__), 0)
#endif

#define GET_REPLY(x, request, ...) request##_reply_t *const x = \
	request##_reply(conn, request##_unchecked(__VA_ARGS__), NULL)

static bool
box_is_container(struct box const *const box)
{
	return !(box->name[0] & 0x20);
}

static void
show_label(struct hand *const hand, struct label *const label, int16_t relx, int16_t rely)
{
	assert(NULL != label->base);
	struct body const *const body = &bodies[label->base->body];
	xcb_screen_t const *const screen = body->screen;
	int16_t x, y;

	x = label->base->x + relx * label->base->width / 2 - LABEL_WIDTH;
	y = label->base->y + rely * label->base->height / 2;
	if (1 == relx)
		x += LABEL_WIDTH / 2;
	if (0 == relx)
		x += LABEL_WIDTH;
	if (1 == rely)
		y -= LABEL_WIDTH / 2;
	if (2 == rely)
		y -= LABEL_WIDTH;

	/* FIXME: take into account screen rotation */

	if (XCB_WINDOW_NONE == label->window) {
		/* setup label window */
		label->window = xcb_generate_id(conn);

		CHECK(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
				label->window,
				screen->root,
				x, y,
				LABEL_WIDTH, LABEL_HEIGHT,
				0,
				XCB_WINDOW_CLASS_INPUT_OUTPUT,
				screen->root_visual,
				/* XCB_CW_BACKING_STORE | */
				XCB_CW_BACKING_PLANES |
				XCB_CW_BACKING_PIXEL |
				XCB_CW_SAVE_UNDER |
				XCB_CW_OVERRIDE_REDIRECT |
				XCB_CW_EVENT_MASK,
				&(uint32_t const[]){
					0xffFF00,
					0,
					true,
					true,
					XCB_EVENT_MASK_EXPOSURE
				});

		/* setup its shape */
		label->shape = xcb_generate_id(conn);

		CHECK(xcb_create_pixmap, conn,
			/* mask is on or off */
			1,
			label->shape,
			label->window,
			LABEL_WIDTH, LABEL_HEIGHT);

		/* we need a valid pixmap so we use the bounding mask but we
		 * use offsets to move it outside of the area making effective
		 * input region empty */
		CHECK(xcb_shape_mask, conn,
				XCB_SHAPE_SO_SET, XCB_SHAPE_SK_INPUT,
				label->window,
				LABEL_WIDTH, LABEL_HEIGHT,
				label->shape);
	}

	/* move label to its place and make sure its above base window */
	/* FIXME: base->window may not be a sibling of label (for containers without title) */

	CHECK(xcb_configure_window, conn, label->window,
			XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y |
			(!box_is_container(label->base) ? XCB_CONFIG_WINDOW_SIBLING : 0) |
			XCB_CONFIG_WINDOW_STACK_MODE,
			&(const uint32_t[]){
				x, y,
				(!box_is_container(label->base) ? label->base->window : XCB_STACK_MODE_ABOVE),
				XCB_STACK_MODE_ABOVE
			});

	repaint_label(label, true);

	CHECK(xcb_shape_mask, conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_BOUNDING,
			label->window,
			0, 0,
			label->shape);

	CHECK(xcb_map_window, conn, label->window);
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

	struct box *box;
	for_each_box(box, root, root, {
		if (0 == memcmp(name, box->name, n)) {
			complete &= NAME_LEN <= n || '\0' == box->name[n];
			if (NULL == *optimum || (*optimum)->focus_seq < box->focus_seq)
				*optimum = box;
		}
	})

	return complete;
}

/* in keychords */
static uint8_t
box_path(struct box const *from, struct box const *to, char path[])
{
	return 1;
}

static bool
name_box(struct box *box, bool iscontainer)
{
	/* collect the min distance, max focus_seq */
	struct {
		uint32_t focus_seq;
	} letters[127];
	/* then search for max distance, min focus_seq */

	memset(letters, 0, sizeof letters);

	/* TODO: if box has name, leave it and check if there are conflicts in its parent */

	uint8_t const n = strnlen(box->name, NAME_LEN);

	struct box *to;
	for_each_box(to, root, root, {
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

	unsigned char optimum = '\0';
	for (unsigned char start = iscontainer ? 'A' : 'a', end = start + ('Z' - 'A');
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

static bool
box_is_monitor(struct box const *const box)
{
	return NULL == box->parent->parent;
}

static bool
box_is_floating(struct box const *const box)
{
	return 0 < box->user_width;
}

static uint16_t
box_compute_num_columns(struct box const *const box)
{
	switch (box->num_columns) {
	case 0:
	{
		uint16_t i = 1;
		while (i * i < box->num_children)
			++i;
		return i;
	}

	default:
		return box->num_columns;

	case UINT16_MAX:
		return box->num_children;
	}
}

static bool
box_is_split(struct box const *const box)
{
	uint16_t const num_columns = box_compute_num_columns(box);
	return 1 == num_columns || box->num_children <= num_columns;
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
update_focus(struct hand *const hand)
{
	struct box *focus = hand->input_focus;

#if 0
	xcb_get_property_reply_t *reply;
	reply =  xcb_get_property_reply(conn,
				xcb_get_property(conn, 0, focus->window,
					XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 0, 0),
				NULL);
	if (reply) {
		printf("title=%.*s\n",
				xcb_get_property_value_length(reply),
				(char *)xcb_get_property_value(reply));
	} else {
		printf("notit %d\n", hand->pointer);
	}
#endif

	assert(NULL == focus || focus->focus_seq == root->focus_seq);

	CHECK(xcb_input_xi_set_focus, conn,
			NULL != focus
				? focus->window
				: bodies[0].screen->root,
			XCB_CURRENT_TIME, hand->keyboard);
	CHECK(xcb_input_xi_set_client_pointer, conn,
			NULL != focus ? focus->window : XCB_WINDOW_NONE,
			hand->pointer);

	if (NULL != focus) {
		CHECK(xcb_input_xi_warp_pointer, conn,
				XCB_WINDOW_NONE,
				focus->window,
				0, 0, 0, 0,
				0, 0,
				hand->pointer);
	}

	if (hand == &hands[0]) {
		/* FIXME: use appropriate body */
		CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				bodies[0].screen->root, ATOM_NET_ACTIVE_WINDOW,
				XCB_ATOM_WINDOW, 32, 1, &(xcb_window_t){
					NULL != focus ? focus->window : XCB_WINDOW_NONE
				});
	}
}

static void
update_box(struct box *const box)
{
	static int depth = 0;

	depth += 3;

	printf("%*.supdate box %p (children=%d container=%d flock=%d title=\"%s\")\n",
			depth, "", (void *)box, box->num_children, box_is_container(box), box->focus_lock, box->title);

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

	if (box->layout_changed && NULL != box->parent) {
		if (0 < box->width && 0 < box->height) {
			mask |=
				XCB_CONFIG_WINDOW_WIDTH |
				XCB_CONFIG_WINDOW_HEIGHT;
			list[i++] = box->width;
			list[i++] = box->height;

			uint16_t num_children = box->num_children;

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
					for (uint16_t i = 0; i < box->num_children; ++i)
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

						int GAP = box->focus_lock ? 0 : 4;
						box_set_position(child, box->x + x + GAP, box->y + y + GAP);
						box_set_size(child, width - GAP * 2, height - GAP * 2);

						update_box(child);

						x += width;
					}
				}
			}
		not_a_container:;
		} else {
		empty_container:
			if (box->window != XCB_WINDOW_NONE)
				CHECK(xcb_unmap_window, conn, box->window);
		}
	} else if (1 || box->content_changed) {
		for (uint16_t i = 0; i < box->num_children; ++i) {
			struct box *const child = box->children[i];
			update_box(child);
		}
	}

	/* if (e->value_mask & XCB_CONFIG_WINDOW_SIBLING) */
	/* if (e->value_mask & XCB_CONFIG_WINDOW_STACK_MODE) */

	printf("%*.sname=%.*s\n", depth, "", NAME_LEN, box->name);
	if (box->window != XCB_WINDOW_NONE) {
		if (0 < i) {
			mask |= XCB_CONFIG_WINDOW_BORDER_WIDTH;
			list[i++] = box->parent->focus_lock ? 0 : (box->parent && box->parent->focus_seq == box->focus_seq && 0 != box->focus_seq ? 2 : 2);
		}

		CHECK(xcb_change_window_attributes, conn, box->window,
				XCB_CW_BORDER_PIXEL, &(uint32_t){
					root->focus_seq == box->focus_seq && 0 != box->focus_seq ? 0xffaf5f : 0xaf5fff
				});

		printf("%*.sfocus_seq=%d\n", depth, "", box->focus_seq);
		if (0 < i) {
			printf("%*.sconfigure\n", depth, "");
			CHECK(xcb_configure_window, conn, box->window, mask, list);
		}
		/* we only after configure happened */
		CHECK(xcb_map_window, conn, box->window);

		if (box->parent->focus_changed && box->focus_seq == root->focus_seq && !box_is_container(box)) {
			assert(0 != box->focus_seq && "focus changed from 0 -> 0?");
			/* so now hand input_focus surely mapped (at least request sent to the
			 * server, so we can focus them now */
			for (uint8_t i = 0; i < num_hands; ++i) {
				struct hand *const hand = &hands[i];
				struct box const *const focus = hand->input_focus;

				if (box != focus)
					continue;

				update_focus(hand);
			}
		}

		struct hand *const hand = box->focus_hand != 0xff ? &hands[box->focus_hand] : NULL;
		if (NULL != hand) {
			struct label *label;

			/* box->focus_seq == root->focus_seq */
			switch (hand->mode) {
			case mode_default:
			{
				if (!hand->label_boxes)
					break;
			case mode_default_label_boxes:
				/* always show label boxes when nameing */
			case mode_name:

				label = new_label_for(box);

				memcpy(label->name, box->name, sizeof box->name);
				show_label(hand, label, box_is_container(box) ? 1 : 2, 0);
			}
				break;

				/* ez */
				break;

			case mode_move_a:
			case mode_move_b:
			{
				/* not a real child */
				if (box_is_monitor(box))
					break;

				/* FIXME: check if hand really focuses */
				bool const focused = box->focus_seq == root->focus_seq && !box_is_container(box);

				label = new_label_for(box);
				label->name[0] = focused ? 'h' : 'a' + hand->num_labels++;
				label->name[1] = '\0';
				show_label(hand, label, 0, 0);

				label = new_label_for(box);
				label->name[0] = focused ? 'j' : 'a' + hand->num_labels++;
				label->name[1] = '\0';
				show_label(hand, label, 0, 2);

				label = new_label_for(box);
				label->name[0] = focused ? 'l' : 'a' + hand->num_labels++;
				label->name[1] = '\0';
				show_label(hand, label, 2, 0);

				label = new_label_for(box);
				label->name[0] = focused ? 'k' : 'a' + hand->num_labels++;
				label->name[1] = '\0';
				show_label(hand, label, 2, 2);


			}
				break;

			case mode_setcolumns:
			{
				if (box->parent != hand->mode_base.box)
					break;

				printf("LABEL CHILDREN\n");
				label = new_label_for(box);
				label->name[0] = 'a' + hand->num_labels++;
				label->name[1] = '\0';
				show_label(hand, label, 1, 1);
			}
				break;

			case mode_size_side:
			{
				if (box_is_monitor(box))
					break;

				if (box_is_floating(box))
					goto all_four;

				if (!box_is_split(box->parent))
					break;

			all_four:
				label = new_label_for(box);
				strncpy(label->name, "h", sizeof label->name);
				show_label(hand, label, 0, 1);

				label = new_label_for(box);
				strncpy(label->name, "j", sizeof label->name);
				show_label(hand, label, 1, 2);

				label = new_label_for(box);
				strncpy(label->name, "k", sizeof label->name);
				show_label(hand, label, 1, 0);

				label = new_label_for(box);
				strncpy(label->name, "l", sizeof label->name);
				show_label(hand, label, 2, 1);

			}
				break;

			}
		}
	}

	box->position_changed = false;
	box->layout_changed = false;
	box->focus_changed = false;
	box->content_changed = false;
	box->title_changed = false;
	/* box->self_focus_changed = false; */


	depth -= 3;
}

/* realize our virtual boxes for the X server */
static void
do_update(void)
{
	/* will regenerate labels */
	for (uint8_t i = 0; i < num_bodies; ++i) {
		struct body *const body = &bodies[i];
		body->num_labels_mapped = body->num_labels_used;
		body->num_labels_used = 0;
	}

	for (uint8_t i = 0; i < num_hands; ++i) {
		struct hand *const hand = &hands[i];
		hand->num_labels = 0;
	}

	/* xcb_aux_sync(conn); */
	update_box(root);

	printf("%d\n", bodies[0].num_labels_used);
	for (uint8_t i = 0; i < num_bodies; ++i) {
		struct body *const body = &bodies[i];
		while (body->num_labels_used < body->num_labels_mapped) {
			struct label *label = &body->labels[--body->num_labels_mapped];
			CHECK(xcb_unmap_window, conn, label->window);
		}
	}

}

/* ensure that parent maximum */
static void
box_update_focus_seq(struct box *box)
{
	assert(box_is_container(box));
	uint32_t old_focus_idx = box->focus_seq;
	do {
		uint32_t max_focus_seq = 0;
		for (uint16_t i = 0; i < box->num_children; ++i) {
			struct box *const child = box->children[i];
			if (max_focus_seq < child->focus_seq)
				max_focus_seq = child->focus_seq;
		}
		/* we unparented the focused box so maximum dropped */
		if (box->focus_seq == max_focus_seq)
			return;

		/* maximum may changed but not the focused children */
		box->focus_changed = box->focus_seq != old_focus_idx;

		old_focus_idx = box->focus_seq;
		box->focus_seq = max_focus_seq;
	} while (NULL != (box = box->parent));
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

	box_update_focus_seq(parent);
}

static void auto_delete_box(struct box *box);

static void
focus_box(struct hand *hand, struct box *box);

static struct box *
find_most_recent_box(struct hand *hand, struct box *root, uint32_t focus_seq)
{
	/* use a dummy box so we do not have to check for NULL */
	struct box const *optimum = &EMPTY_BOX;

	struct box *box;
	for_each_box(box, root, root, {
		if (/* is it a client window? */
		    !box_is_container(box) &&
		    /* if not find a box that... */
		    ((/* is not focused by any hand; so we do not interfere */
		      focus_seq != box->focus_seq &&
		      /* but was focused by the current hand; it's in the history */
		      box->focus_hand == hand - hands &&
		      /* and we need the most recent one */
		      optimum->focus_seq < box->focus_seq) ||
		     /* or fallback to a never focused window */
		     (box->focus_seq == 0 && optimum == &EMPTY_BOX)))
			optimum = box;
	})

	return &EMPTY_BOX != optimum ? (struct box *)optimum : NULL;
}

/* focus something */
static void
focus_all_hands(uint32_t focus_seq)
{
	for (uint8_t i = 0; i < num_hands; ++i) {
		struct hand *hand = &hands[i];

		if (NULL != hand->input_focus)
			continue;

		struct box const *optimum = find_most_recent_box(hand, root, focus_seq);

		if (NULL != optimum) {
			focus_box(hand, (struct box *)optimum);
			focus_seq = root->focus_seq;
		} else {
			hand->focus = NULL;
			hand->input_focus = NULL;
		}
	}
}

static void
increase_focus_seq(void);

static void
hand_leave_mode(struct hand *const hand)
{
	printf("-- leave mode\n");
	hand->mode = mode_default;
	hand->mode_base.box = NULL;
}

/* for containers only */
static void
delete_box(struct box *box)
{
	/* xcb_destroy_window(conn, box->window); */

	struct box *const parent = box->parent;

	parent->layout_changed = true;

	bool focus_changed;

	/* if we removed the currently selected box, and there is only one
	 * hand, root->focus_seq will dropped that could make a previously
	 * selected seems like it's selected (possibly by someone other) */
	uint32_t const real_focus_seq = root->focus_seq;

	/* hands only reference focused boxes, so if this box is not focused we
	 * can skip that */
	if ((focus_changed = box->focus_seq == root->focus_seq)) {
		for (uint8_t i = 0; i < num_hands; ++i) {
			struct hand *hand = &hands[i];

			/* forget current focus */
			if (box == hand->focus)
				hand->focus = NULL;
			if (box == hand->input_focus)
				hand->input_focus = NULL;
			if (box == hand->mode_base.box)
				hand_leave_mode(hand);
		}

		increase_focus_seq();
	}

	printf("delete %x\n", box->window);
	unparent_box(box, true);

	if (focus_changed)
		focus_all_hands(real_focus_seq);

	auto_delete_box(parent);

	free(box);
}

static void
auto_delete_box(struct box *box)
{
	assert(box_is_container(box) && "for containers only");

	/* only care about useless boxes */
	if (0 != box->num_children)
		return;

	struct box *const parent = box->parent;

	assert(NULL != parent && "user cannot address |root| so cannot delete");

	/* user tried drag-n-dropping monitor to trash */
	/* TODO: initiate the kill sequence and burn out some pixels */
	if (NULL == parent->parent)
		return;

	/* body boxes are special because they can be deleted only if their
	 * twin boxes on heads are empty too */
	if (NULL == parent->parent->parent) {
		uint16_t pos = 0;
		while (parent->children[pos] != box)
			++pos;

		for (uint16_t i = 0; i < root->num_children; ++i) {
			struct box const *const head = root->children[i];
			struct box const *const leg = head->children[pos];
			if (0 < leg->num_children)
				return;
		}

		for (uint16_t i = 0; i < root->num_children; ++i) {
			struct box const *const head = root->children[i];
			struct box *const leg = head->children[pos];
			delete_box(leg);
		}
	} else {
		delete_box(box);
	}
}

/* box must not be a root because box->parent == NULL => new box */
static bool
move_box_(struct box *into, uint32_t const pos, struct box *box)
{
	struct box *const boxparent = box->parent;

	/* unparent |box| */
	if (NULL != boxparent)
		unparent_box(box, into != boxparent);

	/* already its parent */
	if (into == boxparent)
		goto insert;

	/* first check if we can make place for |box| in |into| */
	struct box *const new = realloc(into, sizeof *into + sizeof box * (into->num_children + 1));
	if (NULL == new)
		/* FIXME: undo creation body */
		return false;

	/* if |into| location changed because of realloc(), update all
	 * references to it */
	if (new != into) {
/* update |var| if that points to the old reference of |new| */
		for (uint8_t i = 0; i < num_hands; ++i) {
#define UPDATE_REF(var) if (into == (var)) var = new;

			struct hand *hand = &hands[i];
			UPDATE_REF(hand->latest_input[0]);
			UPDATE_REF(hand->latest_input[1]);
			UPDATE_REF(hand->focus);
			UPDATE_REF(hand->mode_base.box);

#undef UPDATE_REF
		}

		/* update parent references */
		if (NULL != new->parent) {
			struct box **child = new->parent->children;

			while (into != *child)
				++child;

			*child = new;
		} else {
			root = new;
		}

		into = new;

		/* update children references */
		for (uint16_t i = 0; i < into->num_children; ++i) {
			struct box *const child = into->children[i];
			child->parent = new;
		}
	}
	box->parent = into;
	++into->num_children;
	if (NULL != into->parent)
		box->focus_lock = into->parent->focus_lock; /* inherit */

insert:
	assert(pos <= into->num_children - 1);

	memmove(
		into->children + pos + 1,
		into->children + pos,
		(into->num_children - 1 - pos) * sizeof *into->children
	);
	into->children[pos] = box;

	if (NULL != boxparent) {
		boxparent->layout_changed = true;
		auto_delete_box(boxparent);
	}

	if (into != boxparent)
		box_update_focus_seq(into);

	into->layout_changed = true;
	return true;
}

static bool
move_box(struct box *into, uint32_t const pos, struct box *box)
{
	/* is monitor? */
	if (box_is_monitor(into)) {
		/* alread there */
		if (into == box->parent) {
			uint16_t leg_pos = 0;
			while (into->children[leg_pos] != box)
				++leg_pos;

			for (uint16_t i = 0; i < root->num_children; ++i) {
				struct box *const head = root->children[i];
				(void)move_box_(head, pos, head->children[leg_pos]);
			}

			return true;
		}

		char *name;

		for (uint16_t i = 0; i < root->num_children; ++i) {
			struct box *const head = root->children[i];
			struct box *const leg = calloc(1, sizeof *leg);

			if (0 == i) {
				name_box(leg, true);
				name = leg->name;
			} else {
				/* copy assigned name for further body_roots */
				memcpy(leg->name, name, sizeof leg->name);
			}

			if (!move_box_(head, pos, leg)) {
				assert(0);
#if 0
				while (i > 0)
					delete_box(heads[--i].root->children[pos]);

				return false;
#endif
			}

			if (into == head) {
				/* NOTE: |head| may be moved */
				struct box *const head = root->children[i];
				if (!move_box_(head->children[pos], 0, box)) {
					assert(0);
				}
			}
		}

		return true;
	}

	return move_box_(into, pos, box);
}

static void \
box_set_focus_lock(struct box *const box, bool enable) \
{
	if (enable != box->focus_lock) {
		box->focus_lock = enable;
		box->layout_changed = true;
	}
}

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
increase_focus_seq(void)
{
	uint32_t const old_focus_seq = root->focus_seq;
	++root->focus_seq;

	for (uint8_t i = 0; i < num_hands; ++i) {
		struct hand *hand = &hands[i];
		struct box *box = hand->input_focus;

		if (NULL == box)
			continue;

		box->focus_hand = i;

		do {
			box->focus_seq = root->focus_seq;
			box->focus_changed |= box->focus_seq != old_focus_seq;
		} while (NULL != (box = box->parent));
	}
}

static void
swap_boxes(struct box *one, struct box *other)
{
	if (one == other)
		return;

	printf("swap %.*s <-> mostrecent=%.*s\n", NAME_LEN, one->name, NAME_LEN, other->name);
	printf("  %d %d rootfq=%d\n", one->focus_seq, other->focus_seq, root->focus_seq);

	struct box **pone;
	struct box **pother;

	for (pone = one->parent->children; *pone != one; ++pone);
	for (pother = other->parent->children; *pother != other; ++pother);

	struct box *const other_parent = other->parent;
	(*pone = other)->parent = one->parent;
	(*pother = one)->parent = other_parent;

	one->parent->layout_changed = true;
	other->parent->layout_changed = true;

	box_update_focus_seq(one->parent);
	box_update_focus_seq(other->parent);
}

static void
focus_box(struct hand *hand, struct box *box)
{
	if (NULL != hand->input_focus) {
		GET_REPLY(pointer, xcb_input_xi_query_pointer, conn, hand->input_focus->window, hand->pointer);
		if (NULL != pointer) {
			/* hand->input_focus->pointer_x = reply->root_x;
			hand->input_focus->pointer_y = reply->root_y; */
			free(pointer);
		}
	}

	hand->focus = box->parent;
	assert(hand->focus);
	assert(box->num_children == 0);
	hand->input_focus = box;
	assert(!box_is_container(hand->input_focus) && "input_focus must be a client window");

	uint32_t const old_focus_seq = root->focus_seq;

	/* TODO: correctly set hand->input_focus */
	/* while (hand->input_focus-> */
	
	struct box *locked = box;
	while (NULL != locked->parent && locked->parent->focus_lock)
		locked = locked->parent;

	printf("focus\n");
	increase_focus_seq();
	assert(box->focus_changed);

	if (locked != box) {
		struct box *const most_recent = find_most_recent_box(hand, locked, root->focus_seq);
		if (NULL != most_recent)
			swap_boxes(box, most_recent);
		/* if there was no such box, that's not a problem: |box| will
		 * be that beginning from now */
	}

}

/* manage window */
static struct box *
box_window(xcb_window_t const root_window, xcb_window_t const window)
{
	printf("box\n");
	struct box *box = calloc(1, sizeof *box);

	CHECK(xcb_change_window_attributes, conn, window,
			XCB_CW_EVENT_MASK,
			&(uint32_t const []){
				XCB_EVENT_MASK_KEY_PRESS
			});

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

	printf("box %d\n", window);
	box->window = window;
	/* box->frame = XCB_WINDOW_NONE; */

	struct hand *hand = NULL;
	struct box *parent = NULL;
	uint32_t pos;
	bool focus = false;

	if (0 < num_hands) {
		struct box *box = NULL;
		if (XCB_WINDOW_NONE != transient_for)
			box = find_box_by_window(root, root, transient_for);

		/* find the hand that could possibly created this window */
		if (NULL != box) {
			if (box->focus_seq == root->focus_seq) {
				struct hand *second_hand = NULL;
				for (uint32_t i = 0; i < num_hands; ++i) {
					hand = &hands[i];
					if (box == hand->input_focus) {
						if (hand->auto_focus)
							goto found_hand;
						if (NULL == second_hand)
							second_hand = hand;
					}
				}
				hand = second_hand;
			found_hand:;
				focus = hand->auto_focus;
			} else {
				hand = &hands[box->focus_hand];
				/* inactive window opened another window so do not focus */
			}
		} else {
			hand = &hands[0];
			for (uint8_t i = 0; i < num_hands; ++i) {
				if (hands[i].auto_focus) {
					hand = &hands[i];
					break;
				}
			}
			focus = hand->auto_focus;
		}

		assert(focus <= (mode_default == hand->mode || mode_default_label_boxes == hand->mode) && "should not focus window when not in default mode");
		parent = hand->focus;
	}

	if (NULL == parent) {
		assert(root->num_children && "no monitors");

		/* get default monitor (0th) of screen with |root_window| */
		struct body *body = bodies;
		while (root_window != body->screen->root)
			++body;
		struct box **head = root->children;
		while ((body - bodies) != (*head)->body)
			++head;

		parent = *head;
		while (parent->num_children && box_is_container(parent->children[0]))
			parent = parent->children[0];

		/* place at the end */
		pos = parent->num_children;
	}

	if (NULL != hand) {
		/* place after currently focused hand */
		for (pos = 0;
		     pos < parent->num_children &&
		     parent->children[pos++] != hand->input_focus;);
	}

	move_box(parent, pos, box);

	box->name[0] = 'a';
	if (NULL != hand && (NULL == hand->focus || focus))
		focus_box(hand, box);
	box->name[0] = 0;

	name_box(box, false);
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

	/*MAN(SIGNALS)
	 * .TP
	 * .B SIGHUP
	 * Restart program, by replacing program image with itself.
	 */
	sa.sa_handler = sigrestart;
	sigaction(SIGHUP, &sa, NULL);

	/*MAN(SIGNALS)
	 * .TP
	 * .B SIGINT, SIGTERM
	 * Terminate program gracefully.
	 */
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
update_inputs(void);

static void
connect_display(void)
{
	while (xcb_connection_has_error((conn = xcb_connect(NULL, &default_screen)))) {
		if (NULL != getenv("DISPLAY")) {
			fprintf(stderr, "could not open display %s\n",
					getenv("DISPLAY"));
		} else {
			fprintf(stderr, "DISPLAY is not set\n");

			if (!setenv("DISPLAY", ":0", 0))
				continue;
		}

		exit(EXIT_FAILURE);
	}

	root = calloc(1, sizeof *root);

	fd_set_cloexec(xcb_get_file_descriptor(conn));

	intern_atoms();

	symbols = xcb_key_symbols_alloc(conn);

	/* i3 xkb_x11_setup_xkb_extension */
	/* xcb_xkb_use_extension_reply(conn,
			xcb_xkb_use_extension_unchecked(conn, XCB_XKB_MAJOR_VERSION,
				XCB_XKB_MINOR_VERSION); */

	for (xcb_query_extension_reply_t const *const xi = xcb_get_extension_data(conn, &xcb_input_id);;) {
		if (!xi->present)
			fprintf(stderr, "XInput extension missing; multi-hand will not work\n");

		xi_opcode = xi->major_opcode;
		break;
	}

	/* XCB_XKB_NEW_KEYBOARD_NOTIFY */

	for (xcb_query_extension_reply_t const *const randr = xcb_get_extension_data(conn, &xcb_randr_id);;) {
		if (!randr->present)
			fprintf(stderr, "RandR extension missing; multi-head will not work\n");

		randr_first_event = randr->first_event;
		break;
	}


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

		if (/* should we manage it? */
		    !reply->override_redirect &&
		    /* is mapped? */
		    reply->map_state == XCB_MAP_STATE_VIEWABLE)
			box_window(screen->root, children[i]);

		free(reply);
	}

	free(cookies);

out_free_reply:
	free(reply);


	/* xcb_randr_get_screen_resources_current_cookie_t rcookie = xcb_randr_get_screen_resources_current(conn, root);
	xcb_randr_get_output_primary_cookie_t pcookie = xcb_randr_get_output_primary(conn, root); */

	/* xcb_randr_get_output_primary_reply_t *primary = NULL;
	xcb_randr_get_screen_resources_current_reply_t *res = NULL; */

	/* if ((primary = xcb_randr_get_output_primary_reply(conn, pcookie, NULL)) == NULL) {
	LOG("Could not determine the primary output.\n");
	goto free_resources;
	} */

	/* if ((res = xcb_randr_get_screen_resources_current_reply(conn, rcookie, NULL)) == NULL) {
	LOG("Could not query screen resources.\n");
	goto free_resources;
	} */


}

static void
setup_screen_cursor(xcb_screen_t *screen)
{
	xcb_cursor_context_t *ctx;

	if (0 <= xcb_cursor_context_new(conn, screen, &ctx)) {
		xcb_cursor_t const cursor = xcb_cursor_load_cursor(ctx, "default");

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
	xcb_keycode_t *const keycodes = xcb_key_symbols_get_keycode(symbols, XK_space);

	for (xcb_keycode_t const *keycode = keycodes;
	     XCB_NO_SYMBOL != *keycode;
	     ++keycode)
		xcb_grab_key(conn, 1, screen->root,
				XCB_NONE, *keycode,
				XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC);
	free(keycodes);
#endif

	/* xcb_grab_key(conn, 1, screen->root,
			0, 133,
			XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC); */
/*
	xcb_grab_key(conn, false, screen->root,
			XCB_MOD_MASK_4, XCB_GRAB_ANY,
			XCB_GRAB_MODE_ASYNC, XCB_GRAB_MODE_ASYNC);

	xcb_grab_button(conn, false, screen->root,
			XCB_EVENT_MASK_BUTTON_PRESS,
			XCB_GRAB_MODE_SYNC, XCB_GRAB_MODE_ASYNC,
			XCB_NONE,
			XCB_NONE,
			XCB_BUTTON_INDEX_1,
			XCB_BUTTON_MASK_ANY); */

}

static void
manage_body(struct body *body)
{
	xcb_screen_t *screen = body->screen;

	for (xcb_generic_error_t *error;
	     NULL != (error = xcb_request_check(conn,
			xcb_change_window_attributes_checked(conn, screen->root,
				XCB_CW_EVENT_MASK,
				&(uint32_t const []){
					XCB_EVENT_MASK_EXPOSURE |
					/* XCB_EVENT_MASK_FOCUS_CHANGE | */
					XCB_EVENT_MASK_PROPERTY_CHANGE |
					XCB_EVENT_MASK_STRUCTURE_NOTIFY |
					XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY |
					XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
			})));)
	{
		if (XCB_ACCESS == error->error_code) {
			fprintf(stderr, "fatal: running window manager detected\n");
			/* it is a fatal error because we have input devices
			 * that we do not know if that WM handles or not that
			 * could cause a conflict */
			exit(EXIT_FAILURE);
		} else {
			print_error(error, "");
		}
		return;
	}

	CHECK(xcb_input_xi_select_events, conn, screen->root, 1,
		(xcb_input_event_mask_t const *)&(struct mask_values {
				xcb_input_event_mask_t mask;
				uint32_t values[1];
			} const){
			{
				.deviceid = XCB_INPUT_DEVICE_ALL,
				.mask_len = ARRAY_SIZE(((struct mask_values *)0)->values)
			},
			{
				XCB_INPUT_XI_EVENT_MASK_HIERARCHY |
				XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS |
				XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
				XCB_INPUT_XI_EVENT_MASK_FOCUS_OUT |
				XCB_INPUT_XI_EVENT_MASK_ENTER |
				XCB_INPUT_XI_EVENT_MASK_LEAVE
			}
		}
	);

	setup_screen_cursor(screen);

	manage_screen_windows(screen);

	setup_screen_keys(screen);

}

static xcb_visualtype_t *
lookup_visual_type(xcb_screen_t const *const screen)
{
	for (xcb_depth_iterator_t depth_iter = xcb_screen_allowed_depths_iterator(screen);
	     0 < depth_iter.rem;
	     xcb_depth_next(&depth_iter))

		for (xcb_visualtype_iterator_t visual_iter = xcb_depth_visuals_iterator(depth_iter.data);
		     0 < visual_iter.rem;
		     xcb_visualtype_next(&visual_iter))

			if (screen->root_visual == visual_iter.data->visual_id)
				return visual_iter.data;

	unreachable;
	return NULL;
}

static void
update_monitors(struct body *const body)
{
	/* TODO */
	if (root->num_children > 0)
		return;

	GET_REPLY(monitors, xcb_randr_get_monitors, conn, body->screen->root, true);
	if (NULL == monitors)
		return;

	int const num_monitors = xcb_randr_get_monitors_monitors_length(monitors);

	for (xcb_randr_monitor_info_iterator_t iter = xcb_randr_get_monitors_monitors_iterator(monitors);
	     0 < iter.rem;
	     xcb_randr_monitor_info_next(&iter))
	{
		xcb_randr_monitor_info_t const *const monitor = iter.data;

		struct box *const head = calloc(1, sizeof *head);
		head->window = body->screen->root;
		head->x = monitor->x;
		head->y = monitor->y;
		head->width = monitor->width;
		head->height = monitor->height;

		GET_REPLY(name, xcb_get_atom_name, conn, monitor->name);

		int const len = xcb_get_atom_name_name_length(name);
		head->title = malloc(len + 1);
		if (NULL != head->title)
			((char *)memcpy(head->title, xcb_get_atom_name_name(name), len))[len] = '\0';

		free(name);

		move_box_(root, monitor->primary ? 0 : root->num_children, head);
	}

	free(monitors);

	assert(root->num_children);
}

static void
manage_each_screen(void)
{
	/* prevent windows from changing */
	/* CHECK(xcb_grab_server, conn); */

	xcb_setup_t const *const setup = xcb_get_setup(conn);

	num_bodies = xcb_setup_roots_length(setup);
	bodies = calloc(num_bodies, sizeof *bodies);

	uint32_t i = 0;
	for (xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);
	     0 < iter.rem;
	     ++i, xcb_screen_next(&iter))
	{
		xcb_screen_t *const screen = iter.data;
		struct body *body = &bodies[default_screen == i ? 0 : i + 1];

		body->screen = screen;
		body->visual_type = lookup_visual_type(screen);

		CHECK(xcb_randr_select_input, conn, screen->root,
				XCB_RANDR_NOTIFY_MASK_SCREEN_CHANGE);

		update_monitors(body);

		manage_body(body);
	}

	/* CHECK(xcb_ungrab_server, conn); */

	for (uint16_t i = 0; i < root->num_children; ++i)
		name_box(root->children[i], true);
}

static void
handle_error(xcb_generic_error_t const *const event)
{
	print_error(event, "(event loop)");
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
handle_input_focus_out(xcb_input_focus_out_event_t const *const event)
{
	/* assert(0); */
	printf("focus out from %d\n", event->event);
	/* CHECK(xcb_input_xi_set_focus, conn, event->event, XCB_CURRENT_TIME, event->sourceid); */
	/* xcb_input_xi_set_client_pointer(conn, focus->window, hand->pointer); */

	/* update_focus(); */
}

static void
grab_hand_keyboard(xcb_window_t root, xcb_input_device_id_t deviceid, bool grab_all)
{
	printf("GRAB: %d\n", grab_all);
	if (grab_all) {
		/* listen for every key so we can record whether input
		 * happened at the current window or not */
		xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root,
				XCB_CURSOR_NONE, XCB_GRAB_ANY,
				deviceid, 1, 1,
				XCB_INPUT_GRAB_TYPE_KEYCODE,
				XCB_INPUT_GRAB_MODE_22_SYNC,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				false,
				&(uint32_t const){
					XCB_INPUT_XI_EVENT_MASK_KEY_PRESS |
					XCB_INPUT_XI_EVENT_MASK_KEY_RELEASE
				},
				&(uint32_t const){
					XCB_INPUT_MODIFIER_MASK_ANY
				});

	} else {
		/* or just grab enter */
		/* NOTE: because we previously grabbed every key we can
		 * undo it if we ungrab all keys. however it also
		 * contains our WM specific Super+... keys too, that we
		 * do not want to remove, so we need to add them back */
		xcb_input_xi_passive_ungrab_device(conn, root, XCB_GRAB_ANY, deviceid,
			1, XCB_INPUT_GRAB_TYPE_KEYCODE, &(uint32_t const){
				XCB_INPUT_MODIFIER_MASK_ANY
			});

		xcb_keycode_t *const keycodes = xcb_key_symbols_get_keycode(symbols, XKB_KEY_Return);

		for (xcb_keycode_t const *keycode = keycodes;
		     XCB_NO_SYMBOL != *keycode;
		     ++keycode)
		{
			xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root,
					XCB_CURSOR_NONE, *keycode,
					deviceid, 1, 1,
					XCB_INPUT_GRAB_TYPE_KEYCODE,
					XCB_INPUT_GRAB_MODE_22_SYNC,
					XCB_INPUT_GRAB_MODE_22_ASYNC,
					false,
					&(uint32_t const){
						XCB_INPUT_XI_EVENT_MASK_KEY_PRESS
					},
					&(uint32_t const){
						XCB_INPUT_MODIFIER_MASK_ANY
					});
		}

		free(keycodes);

		uint32_t const MASKS[] = {
			0,
			XCB_MOD_MASK_SHIFT,
			XCB_MOD_MASK_CONTROL,
			XCB_MOD_MASK_1,
		};

		for (uint8_t i = 0; i < ARRAY_SIZE(MASKS); ++i) {
			xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root,
					XCB_CURSOR_NONE, XCB_GRAB_ANY,
					deviceid, 1, 1,
					XCB_INPUT_GRAB_TYPE_KEYCODE,
					XCB_INPUT_GRAB_MODE_22_SYNC,
					XCB_INPUT_GRAB_MODE_22_ASYNC,
					false,
					&(uint32_t const){
						XCB_INPUT_XI_EVENT_MASK_KEY_PRESS
					},
					&(uint32_t const){
						XCB_MOD_MASK_4 | MASKS[i]
					});
		}
	}
}


static void
handle_input_focus_in(xcb_input_focus_in_event_t const *const event)
{
	/* TODO */
	/* clear current box->pointers[hand index] because we do not already
	 * want to restore it to that value */

	/* printf(">>>>> focus in <<<<<<<\n"); */
	/* the first character updates head->latest_input */
	/* grab_hand_keyboard(event->root, event->deviceid, true); */
}

static void
handle_unmap_notify(xcb_unmap_notify_event_t const *const event)
{
	for (struct box *start = root, *box;
	     NULL != (box = find_box_by_window(root, start, event->window));)
	{
		start = box->parent;
		assert(NULL != start && "root box unmapped by X server");
		delete_box(box);
		break;
	}

	do_update();
}

static void
handle_map_request(xcb_map_request_event_t const *const event)
{
	box_window(event->parent, event->window);

	do_update();
}

static void
handle_configure_notify(xcb_configure_notify_event_t const *const event)
{
#if 0
	for (uint32_t i = 0; i < num_heads; ++i) {
		struct head const *const head = &heads[i];
		if (event->window != head->screen->root)
			continue;

		box_set_size(head->root, event->width, event->height);
		do_update();

		break;
	}
#endif
}

static void
handle_expose(xcb_expose_event_t const *const event)
{
	for (uint8_t i = 0; i < num_bodies; ++i) {
		struct body const *const body = &bodies[i];
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			struct label *label = &body->labels[j];

			if (label->window != event->window)
				continue;

			repaint_label(label, false);

			return;
		}
	}
}

static void
handle_mapping_notify(xcb_mapping_notify_event_t *const event)
{
	for (uint8_t i = 0; i < num_bodies; ++i) {
		struct body const *const body = &bodies[i];

		/* xcb_ungrab_key(conn, XCB_GRAB_ANY, head->screen->root, XCB_BUTTON_MASK_ANY); */

		xcb_refresh_keyboard_mapping(symbols, event);

		setup_screen_keys(body->screen);
	}
}

static void
handle_client_message(xcb_client_message_event_t const *const event)
{
	(void)event;
}

static struct hand *
get_hand_from_pointer(xcb_input_device_id_t const pointer);

static void
update_inputs(void)
{
	GET_REPLY(devices, xcb_input_xi_query_device, conn,
			XCB_INPUT_DEVICE_ALL);
	if (NULL == devices)
		return;

	/* according to specification devices above 127 are invisible to clients */
	uint8_t hand_map[128];

	/* reassign boxes of deattached hands to an invalid hand */
	memset(hand_map, 0xff, num_hands);

	uint8_t new_num_hands = 0;
	struct hand *const new_hands = calloc(xcb_input_xi_query_device_infos_length(devices), sizeof *new_hands);

	for (xcb_input_xi_device_info_iterator_t iter = xcb_input_xi_query_device_infos_iterator(devices);
	     0 < iter.rem;
	     xcb_input_xi_device_info_next(&iter))
	{
		xcb_input_xi_device_info_t *const device = iter.data;

		/* master devices are cursors on the screen. we only have to deal with them */
		/* any slave device can control master */
		if (XCB_INPUT_DEVICE_TYPE_MASTER_POINTER != device->type)
			continue;

		struct hand *const old_hand = get_hand_from_pointer(device->deviceid);

		/* they are always come in pairs */
		struct hand *const hand = &new_hands[new_num_hands++];

		if (NULL != old_hand) {
			memcpy(hand, old_hand, sizeof *hand);
			/* grabbers are already setup and consistent; focus do
			 * not have to be touched */
			hand_map[old_hand - hands] = hand - new_hands;
			continue;
		}

		hand->pointer = device->deviceid;
		hand->keyboard = device->attachment;
		hand->auto_focus = true;
		hand->focus = NULL;
		hand->input_focus = NULL;
		hand->label_boxes = true;

		for (uint8_t i = 0; i < num_bodies; ++i) {
			struct body *const body = &bodies[i];
			xcb_window_t const root_window = body->screen->root;

			grab_hand_keyboard(root_window, hand->keyboard, hand->auto_focus);
			xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root_window,
					XCB_CURSOR_NONE,
					1/* button 1 */,
					new_hands[i].pointer, 1, 1,
					XCB_INPUT_GRAB_TYPE_BUTTON,
					XCB_INPUT_GRAB_MODE_22_SYNC,
					XCB_INPUT_GRAB_MODE_22_ASYNC,
					false,
					&(uint32_t const){
						XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS
					},
					&(uint32_t const){
						XCB_MOD_MASK_4
					});
		}

		update_focus(hand);
	}

	free(devices);

	struct box *box;
	for_each_box(box, root, root, {
		box->focus_hand = hand_map[box->focus_hand];
	});

	/* refocus all boxes to forget focus of deattached hands */
	increase_focus_seq();

	free(hands), hands = new_hands;
	num_hands = new_num_hands;

	focus_all_hands(root->focus_seq);
}

static void
handle_input_hierarchy_change(xcb_input_hierarchy_event_t const *const event)
{
	if (!(event->flags & (
		XCB_INPUT_HIERARCHY_MASK_MASTER_ADDED |
		XCB_INPUT_HIERARCHY_MASK_MASTER_REMOVED
	)))
		return;

	update_inputs();
}

/*
https://www.x.org/releases/X11R7.7/doc/inputproto/XI2proto.txt
https://www.x.org/releases/X11R7.5/doc/randrproto/randrproto.txt
*/

static struct hand *
get_hand_from_keyboard(xcb_input_device_id_t const keyboard)
{
	/* FIXME: if we can receive master devices, remove this check */
	for (uint8_t i = 0; i < num_hands; ++i) {
		struct hand *hand = &hands[i];
		if (keyboard == hand->keyboard)
			return hand;
	}

	/* FIXME: ... and this */
	return NULL;
}

static struct hand *
get_hand_from_pointer(xcb_input_device_id_t const pointer)
{
	for (uint8_t i = 0; i < num_hands; ++i) {
		struct hand *hand = &hands[i];
		if (pointer == hand->pointer)
			return hand;
	}

	return NULL;
}

static void
close_box(struct box *const box)
{
	assert(!box_is_container(box));

	if (box->close_by_force) {
		CHECK(xcb_kill_client, conn, box->window);
		return;
	}

	box->close_by_force = true;

	CHECK(xcb_send_event, conn, false, box->window,
			0/* client messages cannot be masked */,
			(char const *)&(xcb_client_message_event_t){
				.response_type = XCB_CLIENT_MESSAGE,
				.format = 32,
				.window = box->window,
				.type = ATOM_WM_PROTOCOLS,
				.data = {
					.data32 = {
						ATOM_WM_DELETE_WINDOW,
					}
				}
			});
}

static struct label *
find_hand_label(struct hand *hand)
{
	uint8_t n = strnlen(hand->user_input, NAME_LEN);
	if (n < NAME_LEN)
		++n;

	for (uint8_t i = 0; i < num_bodies; ++i) {
		struct body const *const body = &bodies[i];
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			struct label *const label = &body->labels[j];
			if (0 == memcmp(label->name, hand->user_input, n))
				return label;
		}
	}

	memset(hand->user_input, 0, sizeof hand->user_input);
	return NULL;
}

static void
hand_set_timeout(struct hand *const hand, int const timeout_ms)
{
	assert(0 != timeout_ms && "timeout already elapsed you dumbass");
	printf("setto=%d\n", timeout_ms);
	if (next_timeout_ms < timeout_ms) {
		next_timeout_ms = hand->timeout_ms = timeout_ms;
	} else if (next_timeout_ms <= hand->timeout_ms) {
		next_timeout_ms = hand->timeout_ms = timeout_ms;
		for (uint8_t i = 0; i < num_hands; ++i) {
			struct hand *const hand = &hands[i];
			if (next_timeout_ms < hand->timeout_ms)
				next_timeout_ms = hand->timeout_ms;
		}
	} else {
		hand->timeout_ms = timeout_ms;
	}

}

static void
handle_input_key_release(xcb_input_key_press_event_t const *const event)
{
	xcb_keysym_t const sym = xcb_key_symbols_get_keysym(symbols, event->detail, event->mods.effective & XCB_MOD_MASK_SHIFT);

	printf("kep\n");
	struct hand *const hand = get_hand_from_keyboard(event->deviceid);
#ifndef HEAWM_NDEBUG
	/* FIXME: just under Xephyr but root key press reports slave device */
	if (!hand)
		goto out;
#endif

	if (XKB_KEY_Super_L == sym || XKB_KEY_Super_R == sym) {
		hand_set_timeout(hand, -1);
		if (mode_default_label_boxes == hand->mode)
			hand->mode = mode_default;
		do_update();
	}

out:
	xcb_input_xi_allow_events(conn, XCB_CURRENT_TIME, event->deviceid,
			XCB_INPUT_EVENT_MODE_REPLAY_DEVICE,
			0, 0);

}

static void
handle_input_key_press(xcb_input_key_press_event_t const *const event)
{
	xcb_keysym_t const sym = xcb_key_symbols_get_keysym(symbols, event->detail, event->mods.effective & XCB_MOD_MASK_SHIFT);
	printf("key=0x%x mods=%d deviceid=%d\n", sym, event->mods.base, event->deviceid);

	bool propagate;

	struct hand *const hand = get_hand_from_keyboard(event->deviceid);
#ifndef HEAWM_NDEBUG
	/* FIXME: just under Xephyr but root key press reports slave device */
	if (!hand) {
		propagate = false;
		goto out;
	}
#endif

#define KEY_MOD_MASK ( \
	XCB_MOD_MASK_SHIFT   | \
	XCB_MOD_MASK_CONTROL | \
	XCB_MOD_MASK_1       | \
	XCB_MOD_MASK_2       | \
	XCB_MOD_MASK_3       | \
	XCB_MOD_MASK_4       | \
	XCB_MOD_MASK_5       )

	printf("key press; %d; focus=%x\n", hand->mode, xcb_input_xi_get_focus_reply(conn, xcb_input_xi_get_focus(conn, event->deviceid), NULL)->focus);

	if (mode_default != hand->mode && mode_default_label_boxes != hand->mode) {
		propagate = false;

		printf("submode\n");

		if (XKB_KEY_Escape == sym) {
		leave_mode:
			hand_leave_mode(hand);
			do_update();
			goto out_check_focus;
		}

		/* RIP. dumb user get stuck and tries to escape */
		if ((KEY_MOD_MASK & ~XCB_MOD_MASK_SHIFT) & event->mods.effective)
			/* but we just let this little shit shuffering a bit */
			/* NOTE: that if we not reach this comment at
			 * runtime we can be sure user uninstalled us
			 * under one clock cycle :( */
			goto out;

		if ((XKB_KEY_a <= sym && sym <= XKB_KEY_z)) {
			hand->user_input[strnlen(hand->user_input, NAME_LEN)] = 'a' + (sym - XKB_KEY_a);
		} else if ((XKB_KEY_A <= sym && sym <= XKB_KEY_Z)) {
			hand->user_input[strnlen(hand->user_input, NAME_LEN)] = 'A' + (sym - XKB_KEY_A);
		} else if (XKB_KEY_BackSpace == sym) {
			hand->user_input[strnlen(hand->user_input + 1, NAME_LEN - 1)] = '\0';
		}

		printf("%d => inp %.*s\n", sym, NAME_LEN, hand->user_input);

		switch (hand->mode) {
		case mode_name:
		{
			if (XKB_KEY_Return == sym ||
			    NAME_LEN == strnlen(hand->user_input, NAME_LEN))
			{
				if (NULL != hand->input_focus) {
					bool const iscontainer = box_is_container(hand->input_focus);
					memcpy(hand->input_focus->name, hand->user_input, NAME_LEN);
					char *const c = &hand->input_focus->name[0];
					if (iscontainer)
						*c &= ~0x20;
					else
						*c |= 0x20;
					name_box(hand->input_focus, iscontainer);
					assert(box_is_container(hand->input_focus) == iscontainer);
				}
				goto leave_mode;
			}
		}
			break;

		case mode_setcolumns:
		{
			struct label *label = find_hand_label(hand);
		printf("mode_setcolumns %d\n", !!label);
			if (NULL != label) {
				uint16_t pos = 0;
				while (hand->mode_base.box->children[pos++] != label->base);

				if (pos == hand->mode_base.box->num_children)
				vertical_split:
					pos = UINT16_MAX;

				hand->mode_base.box->num_columns = pos;
				hand->mode_base.box->layout_changed = true;
				goto leave_mode;
			} else if (XKB_KEY_minus == sym) {
				goto vertical_split;
			} else if (XKB_KEY_equal == sym) {
				hand->mode_base.box->num_columns = 0;
				hand->mode_base.box->layout_changed = true;
				goto leave_mode;
			}
		}
			break;

		}
		goto out;
	}

	if (XKB_KEY_Super_L == sym || XKB_KEY_Super_R == sym) {
		propagate = false;
		hand_set_timeout(hand, 70);
		goto out;
	}

	hand_set_timeout(hand, -1);
	if (mode_default_label_boxes == hand->mode) {
		hand->mode = mode_default;
		grab_hand_keyboard(event->root, event->deviceid, hand->auto_focus);
	}

	/* otherwise root window receives input that sends
	 * forward to its children based on pointer location */
	/* and we cannot disable keyboard focus since then user
	 * will not be able to control wm afterwards */
	propagate = NULL != hand->input_focus;
	bool new_auto_focus;

	if (!(event->mods.effective & XCB_MOD_MASK_4)) {
		new_auto_focus = XKB_KEY_Return == sym || XKB_KEY_KP_Enter == sym;

		if (NULL != hand->input_focus) {
			hand->input_focus->close_by_force = false;

			if (hand->latest_input[0] != hand->input_focus) {
				hand->latest_input[1] = hand->latest_input[0];
				hand->latest_input[0] = hand->input_focus;
			}
		}

		goto out_check_focus;
	} else {
		new_auto_focus = true;
	}

	switch ((KEY_MOD_MASK & event->mods.effective) & ~(XCB_MOD_MASK_4 | XCB_MOD_MASK_SHIFT)) {
	case 0:
		/*MAN(Keybindings)
		 * .TP
		 * .B Mod+{a-zA-Z}...
		 * Focus window by name. If there is no such window,
		 * keybinding is treated like
		 * .B Ctrl
		 * was pressed.
		 */
		if ((XKB_KEY_a <= sym && sym <= XKB_KEY_z)) {
			hand->user_input[strlen(hand->user_input)] = 'a' + (sym - XKB_KEY_a);
			propagate = false;
		} else if ((XKB_KEY_A <= sym && sym <= XKB_KEY_Z)) {
			hand->user_input[strlen(hand->user_input)] = 'A' + (sym - XKB_KEY_A);
			propagate = false;
		} else if (XKB_KEY_Return == sym) {
			/*MAN(Keybindings)
			 * .TP
			 * .B Mod+Return
			 * Open $
			 * .B TERMINAL .
			 */
			if (0 == fork()) {
				setsid(); /* move process into its own session */
				execlp(getenv("TERMINAL"), getenv("TERMINAL"), NULL);
				perr("execlp");
				_exit(127);
			}

			break;
		} else {
			break;
		}

		struct box *box;

		printf("jump %.*s\n", 4, hand->user_input);

		if (find_box_by_name(&box, hand->user_input)) {
			memset(hand->user_input, 0, sizeof hand->user_input);

			if (NULL != box) {
				struct hand *hand = &hands[0];
				if (NULL != hand->input_focus &&
				    box == hand->input_focus)
					box = hand->latest_input[box == hand->latest_input[0]];

				if (NULL != box) {
					focus_box(hand, box);
					do_update();
				}
				break;
			}
		}
		break;

	case XCB_MOD_MASK_CONTROL:
	case XCB_MOD_MASK_1:
		switch (sym) {
		/*MAN(Keybindings)
		 * .TP
		 * .B [focus] Mod+Ctrl+c {child}
		 * Set the number of columns of the focused grid.
		 * .B child
		 * specifies the first window that should appear at the end of the first row.
		 * .IP
		 * .B =
		 * sets auto layout.
		 */
		case XKB_KEY_c:
			if (NULL == hand->input_focus)
				break;

			hand->mode = mode_setcolumns;
			hand->mode_base.box = hand->input_focus->parent;
			new_auto_focus = false;
			memset(hand->user_input, 0, sizeof hand->user_input);
			break;

		/*MAN(Keybindings)
		 * .TP
		 * .B [focus] Mod+Ctrl+r {what} {to-where}
		 * Resize a split or a side of a floating window.
		 * .IP
		 * .B =
		 * sets equalizes sizes for all splits in the grid.
		 */
		case XKB_KEY_r:
			/* TODO: take account window gravity */
			hand->mode = mode_size_side;
			new_auto_focus = false;
			break;

		/*MAN(Keybindings)
		 * .TP
		 * .B [focus] Mod+Ctrl+m {point-a} {point-b}
		 * Move focused window to the place of line from {point-a} to {point-b}.
		 */
		case XKB_KEY_m:
			hand->mode = mode_move_a;
			new_auto_focus = false;
			break;

		/*MAN(Keybindings)
		 * .TP
		 * .B [focus] Mod+Ctrl+l
		 * Lock focus position in focus. On a focused window unlock.
		 */
		case XKB_KEY_l:
		{
			struct box *box = hand->input_focus;
			if (NULL == box)
				break;

			bool const set = !box->parent->focus_lock;
			if (box->parent == hand->focus)
				/* focus lock makes sense only if we have more
				 * children, so move upwards till we set the
				 * focus lock on a container that has more than
				 * one children */
				do
					box_set_focus_lock(box, set);
				while (box->num_children <= 1 &&
				       NULL != (box = box->parent));
			else
				for (; hand->focus != box; box = box->parent)
					box_set_focus_lock(box, set);

			do_update();
		}
			break;

		/* ~fullscreen~ fuck, i can't see anything: set concealed for
		 * all upper windows till monitor level
		 *
		 * if nothing to do: unfullscreen
		 */
		case XKB_KEY_f:
			assert(!"unimplemented");
			break;

		/*MAN(Keybindings)
		 * .TP
		 * .B Mod+Ctrl+n {a-zA-Z}... Return
		 * Name window.
		 */
		case XKB_KEY_n:
			if (NULL == hand->input_focus)
				break;

			hand->mode = mode_name;
			new_auto_focus = true; /* FIXME: should be false, but keys must be grabbed */
			memset(hand->user_input, 0, sizeof hand->user_input);
			break;

		/* wlose/xlose */
		/*MAN(Keybindings)
		 * .TP
		 * .B [focus] Mod+Ctrl+w
		 * Close window(s) in focus just like user would clicked
		 * \*(lqX\*(rq in the title bar. Second press kills by force.
		 */
		case XKB_KEY_w:
			if (NULL != hand->focus) {
				struct box *box;
				for_each_box(box, hand->focus, hand->focus, {
					if (!box_is_container(box))
						close_box(box);
				})
			}
			break;
		default:
			goto out_check_focus;
		}

		propagate = false;
		do_update();
		break;
	}

out_check_focus:;
	if (NULL == hand->input_focus) {
		new_auto_focus = mode_default == hand->mode || mode_default_label_boxes == hand->mode;
		propagate = false;
	}

	if (new_auto_focus != hand->auto_focus) {
		hand->auto_focus = new_auto_focus;
		grab_hand_keyboard(event->root, event->deviceid, hand->auto_focus || (mode_default != hand->mode && mode_default_label_boxes != hand->mode));
	}

out:
	xcb_input_xi_allow_events(conn, XCB_CURRENT_TIME, event->deviceid,
			propagate
				? XCB_INPUT_EVENT_MODE_REPLAY_DEVICE
				: XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
}

static void
handle_input_button_press(xcb_input_button_press_event_t const *const event)
{
	struct hand *const hand = get_hand_from_pointer(event->deviceid);
	if (NULL == hand)
		return;

	printf("button\n");
	bool propagate = true;
	/* if (1 || XCB_MOD_MASK_4 == (event->mods.effective & MOD_MASK)) { */
	struct body *body = bodies;
	while (event->root != body->screen->root)
		++body;

	for (uint16_t i = 0; i < root->num_children; ++i) {
		struct box *const head = root->children[i];
		if ((body - bodies) != head->body)
			continue;

		struct box *box = find_box_by_window(head, head, event->child);
		if (NULL == box)
			continue;

		if (box_is_container(box))
			break;

		propagate = false;
		focus_box(hand, box);
		assert(box->num_children == 0);
		do_update();

		break;
	}
	/* } */

	xcb_input_xi_allow_events(conn, XCB_CURRENT_TIME, event->deviceid,
			propagate
				? XCB_INPUT_EVENT_MODE_REPLAY_DEVICE
				: XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
# if 0
	xcb_allow_events(conn,
			propagate
				? XCB_ALLOW_REPLAY_POINTER
				: XCB_ALLOW_SYNC_POINTER,
			XCB_CURRENT_TIME);

#endif
}

static void
handle_input_enter(xcb_input_enter_event_t const *const event)
{
	xcb_input_xi_set_client_pointer(conn, event->child, event->deviceid);
}

static void
handle_input_event(xcb_ge_generic_event_t const *const event)
{
	switch (event->event_type) {
	case XCB_INPUT_HIERARCHY:
		handle_input_hierarchy_change((void const *)event);
		break;

	case XCB_INPUT_FOCUS_OUT:
		/* handle_input_focus_out((void const *)event); */
		break;

	case XCB_INPUT_FOCUS_IN:
		/* handle_input_focus_in((void const *)event); */
		break;

	case XCB_INPUT_BUTTON_PRESS:
		handle_input_button_press((void const *)event);
		break;

	case XCB_INPUT_KEY_PRESS:
		handle_input_key_press((void const *)event);
		break;

	case XCB_INPUT_KEY_RELEASE:
		handle_input_key_release((void const *)event);
		break;

	case XCB_INPUT_UNGRAB_DEVICE_KEY:
		break;

	case XCB_INPUT_LEAVE:
	case XCB_INPUT_ENTER:
		handle_input_enter((void const *)event);
		break;

	default:
		/* assert(!"unhandled\n"); */
		break;

	}
}

static void
handle_randr_screen_change_notify(xcb_randr_screen_change_notify_event_t const *const event)
{
	struct body *body = bodies;
	while (body->screen->root != event->root)
		++body;

	update_monitors(body);
}

static bool
handle_randr_event(xcb_generic_event_t const *const event)
{
	switch (XCB_EVENT_RESPONSE_TYPE(event) - randr_first_event) {
	case XCB_RANDR_SCREEN_CHANGE_NOTIFY:
		handle_randr_screen_change_notify((void *)event);
		break;

	default:
		return false;
	}

	return true;
}

static bool
handle_extension_event(xcb_generic_event_t const *const event)
{
	return handle_randr_event(event);
}

static void
handle_generic_event(xcb_ge_generic_event_t const *const event)
{
	if (xi_opcode == event->extension)
		handle_input_event(event);
	else
		unreachable;
}

static void
hand_timed_out(struct hand *const hand)
{
	assert(mode_default_label_boxes == hand->mode || mode_default == hand->mode);
	printf("hand_timed_out(()\n");
	hand->mode = mode_default_label_boxes;
	do_update();
}

static void
hands_time_elapsed(int elapsed_ms)
{
	if (next_timeout_ms < elapsed_ms)
		elapsed_ms = next_timeout_ms;

	int new_timeout_ms = -1;

	printf("Elapsed=%dms\n", elapsed_ms);
	for (uint8_t i = 0; i < num_hands; ++i) {
		struct hand *const hand = &hands[i];
		printf("hand to=%d\n", hand->timeout_ms);
		if (hand->timeout_ms < 0)
			continue;

		if ((hand->timeout_ms -= elapsed_ms) <= 0) {
			hand->timeout_ms = -1;
			hand_timed_out(hand);
		} else if (new_timeout_ms < hand->timeout_ms) {
			new_timeout_ms = hand->timeout_ms;
		}
	}

	next_timeout_ms = new_timeout_ms;
	printf("next timeout =%d\n", next_timeout_ms);
}

int
main(int _argc, char *_argv[])
{
	argc = _argc, argv = _argv;
	atexit(quit);

	setup_sighandlers();

	for (char c; -1 != (c = getopt(argc, argv, "v"));) {
		switch (c) {
		/*MAN(OPTIONS)
		 * .TP
		 * .B \-v
		 * Show Git commit (version) and exit.
		 */
		case 'v':
			printf(VERSION "\n");
			exit(EXIT_SUCCESS);

		case '?':
		case ':':
			fprintf(stderr, "%s: Option -%c is invalid or requires an argument.\n",
					argv[0],
					optopt);
			exit(EXIT_FAILURE);

		default:
			abort();
		}
	}

	/* BROADCAST(start, &(struct start_args){0}); */

	connect_display();

	manage_each_screen();

	update_inputs();

	do_update();

	/* xcb_flush + wait for all replies */
	/* xcb_aux_sync(conn); */
	xcb_flush(conn);

	struct pollfd pfd;
	pfd.fd = xcb_get_file_descriptor(conn);
	pfd.events = POLLIN | POLLPRI;

	for (xcb_generic_event_t *event;; free(event)) {
		while (NULL == (event = xcb_poll_for_event(conn))) {
			struct timespec poll_start;

			if (0 < next_timeout_ms)
				clock_gettime(HAND_TIMEOUT_CLOCK, &poll_start);
			assert(0 != next_timeout_ms && "next_timeout_ms should never be 0");

			if (0 == poll(&pfd, 1, next_timeout_ms)) {
				hands_time_elapsed(next_timeout_ms);
				continue;
			} else if (0 < next_timeout_ms) {
				struct timespec now;
				clock_gettime(HAND_TIMEOUT_CLOCK, &now);
				hands_time_elapsed(((now.tv_sec - poll_start.tv_sec) << 10) + ((now.tv_nsec - poll_start.tv_nsec) >> 20));
				assert(0 != next_timeout_ms);
			}

			if (!(pfd.revents & POLLIN))
				return EXIT_SUCCESS;
		}

		/* printf("event = (%d)%s\n", event->response_type, xcb_event_get_label(event->response_type)); */

		switch (XCB_EVENT_RESPONSE_TYPE(event)) {
		case 0:
			handle_error((void *)event);
			continue;

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

		case XCB_PROPERTY_NOTIFY:
			handle_property_notify((void *)event);
			break;

		case XCB_CLIENT_MESSAGE:
			handle_client_message((void *)event);
			break;

		case XCB_GE_GENERIC:
			handle_generic_event((void *)event);
			break;

		default:
			if (!handle_extension_event((void *)event))
				continue;
			break;
		}

		xcb_flush(conn);
	}

	return EXIT_SUCCESS;
}
