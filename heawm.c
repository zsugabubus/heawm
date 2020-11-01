#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <math.h>
#include <memory.h>
#include <poll.h>
#include <signal.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <xcb/randr.h>
#include <xcb/shape.h>
#include <xcb/xcb_atom.h>
#include <xcb/xcb_aux.h>
#include <xcb/xcb_cursor.h>
#include <xcb/xcb_event.h>
#include <xcb/xcb.h>
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_image.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xcb_xrm.h>
#include <xcb/xinput.h>
#include <xcb/xkb.h>
#include <xcb/xproto.h>
#include <xkbcommon/xkbcommon.h>
#include <xkbcommon/xkbcommon-x11.h>

/* TEST: https://superuser.com/questions/801611/how-to-make-all-applications-respect-my-modified-xkb-layout/844673#844673 */

/* https://github.com/LemonBoy/bar */

/* motif: https://gitlab.gnome.org/GNOME/gtk/-/blob/master/gdk/x11/MwmUtil.h */
/* https://people.gnome.org/~tthurman/docs/metacity/xprops_8h-source.html */

/*
https://www.x.org/releases/X11R7.7/doc/inputproto/XI2proto.txt
https://www.x.org/releases/X11R7.5/doc/randrproto/randrproto.txt
*/

#ifdef HEAWM_NDEBUG
/** suppress debug output */
#endif

/** maximum length of names */
#ifndef NAME_LEN
# define NAME_LEN 2
#endif

#ifndef M_PHI
# define M_PHI 1.6180339887 /* Golden Ratio */
#endif

/** who we are? */
#ifndef WM_NAME
# define WM_NAME "LG3D"
#endif

#define RGB8_TO_FLOATS(color) \
	(uint8_t)((color) >> 16) / 256., \
	(uint8_t)((color) >> 8 ) / 256., \
	(uint8_t)((color)      ) / 256.

/* sizeof array... but in elements */
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(*x))
#define offsetof(type, member) (size_t)(&((type *)0)->member)
#define membersizeof(type, member) (sizeof(((type *)0)->member))

#if defined(__GNUC__)
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

# define CHECK(request, ...) check_cookie(request##_checked(__VA_ARGS__), STRINGIFY(__LINE__) ": " #request)

#ifndef HEAWM_NDEBUG
# define STRINGIFY_(x) #x
# define STRINGIFY(x) STRINGIFY_(x)
# define DEBUG_CHECK(request, ...) check_cookie(request##_checked(__VA_ARGS__), STRINGIFY(__LINE__) ": " #request)
#else
# define DEBUG_CHECK(request, ...) (request(__VA_ARGS__), true)
#endif

#define GET_REPLY(x, request, ...) request##_reply_t *const x = \
	request##_reply(conn, request##_unchecked(__VA_ARGS__), NULL)

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

/* because xcb_send_event() does not take an |event| length argument, no matter
 * what, it will copy a fixed number of bytes. to avoid reading (then sending)
 * out-of-bounds bytes we must make sure we have as many bytes as XCB needs. */
#define XCB_SEND_EVENT_EVENT(event, ...) \
	(char const *)&(alignas(membersizeof(xcb_send_event_request_t, ev##ent)) \
	struct { \
		event data; \
		char pad[MAX(membersizeof(xcb_send_event_request_t, ev##ent) - sizeof(event), 1)]; \
	}){ \
		.data = { __VA_ARGS__ }, \
		.pad = { 0 } \
	}

#define EVENT_MASK \
(	XCB_EVENT_MASK_PROPERTY_CHANGE \
|	XCB_EVENT_MASK_STRUCTURE_NOTIFY \
|	XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY \
|	XCB_EVENT_MASK_FOCUS_CHANGE \
|	XCB_EVENT_MASK_ENTER_WINDOW \
)

#define NULL_BODY 128U
#define MAX_NUM_HANDS 63U
#define NULL_HAND (MAX_NUM_HANDS + 1U)

#define LABEL_CLASS "heawm"
#define LABEL_INSTANCE LABEL_CLASS "-label"

#define for_each_template_(elem_type, list, index_type, name, extra) \
	for (index_type name##_index = 0; (extra) || name##_index < num_##list; ++name##_index) \
		for (elem_type *const name = &list[name##_index], *loop_ = (void *)true; loop_; loop_ = (void *)false)

#define for_each_body for_each_template_(Body, bodies, uint8_t, body, 0 == body_index)
#define for_each_hand for_each_template_(Hand, hands, uint8_t, hand, 0)

#define for_each_box(x, root, start, block) \
	for ((x) = (start);;) { \
		(x)->iter = 0; \
		block; \
		while ((x)->num_children <= (x)->iter) { \
			if ((x) == (root)) { \
				(x) = NULL; \
				goto out_for_each; \
			} \
			assert((x)->parent); \
			(x) = (x)->parent; \
		} \
		(x) = (x)->children[x->iter++]; \
	} \
out_for_each:;

static struct {
	char const *terminal;
	char const *shell;
	char heawm_home[PATH_MAX];
} config;

typedef struct {
	xcb_window_t window; /** base */
	xcb_input_fp1616_t x, y;
} BoxPointer;

/** a box that holds smaller boxes. you surely know such box so i do not have
 * to introduce it better */
typedef struct box Box;
struct box {
	/** box location relative to the root box */
	int16_t x, y;
	/** dimensions */
	uint16_t width, height;

	/** user requested boundaries; mainly for floating windows */
	int16_t user_x, user_y;
	uint16_t user_width, user_height;

	/** size granulaty */
	uint8_t mod_x, mod_y;

	/** client window */
	xcb_window_t window;
	/* xcb_window_t frame; */
	xcb_window_t leader;

	/** number of |children| */
	uint16_t num_children,
	/** internal variable used to track progress of recursive searching
	 *
	 * - it does not occupy too much space and we do not have to
	 *   dynamically allocate more memory for backtracking or track maximum
	 *   possible depth
	 * - it also makes very convenient to continue the walk where we left
	 *   off */
	         iter,

	/** number of columns
	 *
	 * namely:
	 * - 0: auto,
	 * - 1: column,
	 * - -1: row,
	 * - other: fixed, user set */
	         num_columns;
	/* TODO: maybe a different name; label/id? */
	/** permanent name */
	char name[NAME_LEN];
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
	bool position_changed: 1, /** box repositioned */
	     layout_changed: 1, /** any of the children's boundary box changed */
	     focus_changed: 1, /** focused child changed */
	     content_changed: 1, /** set by children to indicate parent should
	                            descend with update (means that this flag
	                            must be propagated upwards until root);
	                            for client windows indicates that name or title changed */
	     title_changed: 1, /** for containers, set by children */
	     label_changed: 1, /** label should be redrawn */
	     close_by_force: 1,
	     no_vacuum: 1, /** for containers created by user; such containers
	                       will not be vacuumed */

	     hide_label: 1,
	     /** show only when has focus; hide otherwise */
	     concealed: 1,
	     /** fix focus position on screen by always swapping newly
	      * focused window with previously focused window */
	     focus_lock: 1;
	/** last time (sequence number) when box has been focused
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
	Box *parent;

	/** contained boxes for containers */
	Box *children[];
};

/** the box that contains everything
 *
 * root's children are XRandR monitors from all screens
 */
static Box *root;

/** little angels flying around screen */
typedef struct {
	Box *base;
	uint64_t hands;
	xcb_window_t window; /** the X window */
	xcb_pixmap_t shape; /** shape of bounding box */
	int16_t x, y; /** position of label */
	bool position_changed: 1,
	     content_changed: 1;
	enum {
		/** text only */
		label_normal,
		/** draw glory if |base| focused */
		label_box,
		/** text with horizontal line */
		label_hline,
		/** text with vertical line */
		label_vline
	} type: 2;
	/** name of label, just like for boxes */
	char name[NAME_LEN];
} Label;

typedef struct {
	xcb_screen_t *screen;
	xcb_visualtype_t *visual_type;
	int screen_index;

	uint32_t num_labels_used,
	/* <= */ num_labels_mapped,
	/* <= */ num_labels;
	Label *labels;
	xcb_window_t blackhole;
} Body;

static uint8_t num_bodies;
static Body *bodies;
static int default_screen;

/* not a too exact time */
static int next_timeout_ms = -1;

typedef enum {
	mode_default,
	mode_setcolumns,
	mode_move_a,
	mode_move_b,
	mode_size_side,
	mode_size_to,
	mode_name,
} LabelMode;

/* user */
typedef struct {
	/* keyboard and pointer are always in pair */
	xcb_input_device_id_t master_pointer; /** master pointer device */
	xcb_input_device_id_t master_keyboard; /** master key device */

	/** focus freshly mapped related windows */
	bool want_focus;

	/** user input buffer for longer box names */
	char user_input[NAME_LEN];

	/** number of labels owned by this hand */
	uint32_t num_labels;

	/** time until |hand_timed_out()| is called; -1 if disarmed */
	int timeout_ms;

	/** show box labels in default mode */
	bool label_boxes;

	/** |mode| related state */
	Box *mode_box;
	LabelMode mode;

	/** box that last time received keyboard input (only non-container);
	 * we also store the box that was focused before last time since if
	 * we start typing into the currently focused window that is not
	 * really interesting to remember for */
	Box *latest_input[2];

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
	Box *input_focus;

	/* user focused box that may be a container since it will not
	 * receive keyboard events... or any at all. as soon as user starts
	 * typing focus := input_focus */
	Box *focus;

	unsigned int color;
} Hand;

/** number of |hands| */
static uint8_t num_hands;
/** stores list of humans and/or EBEs who have at least a keyboard or a pointer
 * device (a master device pair) for controlling X */
static Hand *hands;

/* NOTE: that we do not store whether it's a mouse or keyboard device since
 * caller knows that exactly */
typedef struct {
	xcb_input_device_id_t id;
	struct xkb_keymap *keymap; /* NULL if not keyboard */
	Hand *hand;
} Device;

static uint8_t num_devices;
static Device *devices;

typedef struct {
	int width;
	int height;
} Size;

static char const *label_font = "monospace";
static Size label_rect = { .width = 30, .height = 60 }; /** in pts */
static int label_stroke = /* 4 */ 2 /* .5 */;
static int label_size =
#if 1
17
#else
22
#endif
;

#include "atoms.h"

static char const *const ATOM_NAMES[] =
{
	"_NET_CLIENT_LIST",
	"_NET_CLOSE_WINDOW",
	"_NET_ACTIVE_WINDOW",
	"_NET_WM_NAME",
	"_NET_SUPPORTING_WM_DEBUG_CHECK",
	"_NET_WM_STATE",
	"_NET_WM_STATE_DEMANDS_ATTENTION",
	"_NET_WM_STATE_FULLSCREEN",
	"_NET_WM_STATE_MODAL",
	"_NET_WM_TRANSIENT_FOR",
	"UTF8_STRING",
	"WM_CLIENT_LEADER",
	"WM_NORMAL_HINTS",
	"WM_SIZE_HINTS",
	"WM_DELETE_WINDOW",
	"WM_PROTOCOLS",
	"_HEAWM_NAME",
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
static xcb_xrm_database_t *xrm;
/** user preferred screen; provided by $DISPLAY */
static int preferred_screen;
/** major opcode of XInput extension */
static uint8_t xi_opcode;
/** beginning of XRandR event range */
static uint8_t randr_base_event;
static uint8_t xkb_base_event;
static struct xkb_context *xkb_context;
static xcb_key_symbols_t *symbols;

static int argc;
static char **argv;

#define print_strerror(what) \
	fprintf(stderr, "%s: %s: %s\n", __func__, what, strerror(errno));

#define heawm_file(name) heawm_file_internal((char[PATH_MAX]){ 0 }, PATH_MAX, name)

static char const *
heawm_file_internal(char *str, size_t size, char *name)
{
	strncpy(str, config.heawm_home, size);
	strncat(str, "/", size);
	strncat(str, name, size);
	return str;
}

#define SPAWN(...) spawn((char const *[]){ __VA_ARGS__, NULL }, NULL, NULL)

static pid_t
spawn(char const *argv[], void(*fork_cb)(void *), void *arg)
{
	pid_t ret;
	sigset_t sigmask, origmask;

	/* block all signals in order to avoid executing registered signal
	 * handlers in child. */
	sigfillset(&sigmask);
	pthread_sigmask(SIG_SETMASK, &sigmask, &origmask);

	fprintf(stderr, "spawning");
	for (char const **arg = argv; *arg; ++arg)
		fprintf(stderr, " \"%s\"", *arg);
	fputc('\n', stderr);

	if (0 == (ret = fork())) {
		/* move process into its own session */
		setsid();

		/* signal handlers have to be reset to their default action
		 * before we unblock them for the new process to avoid executing
		 * them before exec() */
		struct sigaction const sa = {
			.sa_handler = SIG_DFL
		};

		for (int sig = 1; sig < SIGRTMAX; ++sig)
			sigaction(sig, &sa, NULL);

		sigemptyset(&sigmask);
		pthread_sigmask(SIG_SETMASK, &sigmask, NULL);

		/* disconnet standard io */
		dup2(open("/dev/null", O_RDONLY | O_CLOEXEC | O_CREAT, 0666), STDIN_FILENO);
		/* dup2(open("/dev/null", O_WRONLY | O_CLOEXEC | O_CREAT, 0666), STDOUT_FILENO); */
		/* dup2(STDOUT_FILENO, STDERR_FILENO); */

		if (fork_cb)
			fork_cb(arg);

		execvp(argv[0], (char **)argv);
		print_strerror("execlp");
		_exit(127);
	}

	pthread_sigmask(SIG_SETMASK, &origmask, NULL);

	return ret;
}

static Box *
find_box_by_leader(Box *const root, Box *start, xcb_window_t const window)
{
	Box *box;
	for_each_box(box, root, start, {
		if (window == box->leader)
			break;
	})
	return box;
}

static Box *
find_box_by_window(Box *const root, Box *start, xcb_window_t const window)
{
	Box *box;
	for_each_box(box, root, start, {
		if (window == box->window)
			break;
	})
	return box;
}

static bool
box_is_monitor(Box const *const box)
{
	return !box->parent->parent;
}

static bool
box_is_leg(Box const *const box)
{
	return box_is_monitor(box->parent);
}

static Box *
box_get_monitor(Box const *box)
{
	while (!box_is_monitor(box))
		box = box->parent;
	return (Box *)box;
}

static Size
monitor_convert_pt2px(Box const *const monitor, Size const points)
{
	assert(box_is_monitor(monitor));
#define C(dim) (monitor->dim * points.dim * 254 / 720 / monitor->user_##dim)
	return (Size){
		C(width),
		C(height)
	};
#undef C
}

static bool
box_is_container(Box const *const box)
{
	return !(box->name[0] & 0x20);
}

static bool
box_is_focused(Box const *const box)
{
	return root->focus_seq == box->focus_seq;
}

static void
label_repaint(Label const *const label, bool const shape)
{
	/* POSSIBLE FIXME: take into account screen rotation */

	char name[NAME_LEN + 1];

	memcpy(name, label->name, NAME_LEN);
	name[NAME_LEN] = '\0';

	Box const *const monitor = box_get_monitor(label->base);
	Size const size = monitor_convert_pt2px(monitor, label_rect);
	int const font_size = monitor_convert_pt2px(monitor,
			(Size){ 0, label_size }).height;
	int const stroke_width = monitor_convert_pt2px(monitor,
			(Size){ label_stroke, 0 }).width;

	Body const *const body = &bodies[label->base->body];
	cairo_surface_t *const surface = shape
		? cairo_xcb_surface_create_for_bitmap(conn, body->screen, label->shape, size.width, size.height)
		: cairo_xcb_surface_create(conn, label->window, body->visual_type, size.width, size.height);
	cairo_t *const cr = cairo_create(surface);
	cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	if (/* no compositor */1)
		cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);

	Hand *hand = &hands[label->base->focus_hand];

	cairo_select_font_face(cr, label_font,
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
		cairo_rectangle(cr, 0, 0, size.width, size.height);
		cairo_fill(cr);
	}

	cairo_set_font_size(cr, font_size);

	cairo_text_extents_t te;
	cairo_text_extents(cr, name, &te);

	int const radius = font_size / 2/*sqrt(te.height * te.height + te.width * te.width)*/;

	cairo_translate(cr,
			0.5 - te.x_bearing + size.width / 2,
			0.5 - te.y_bearing + size.height / 2 - te.height
	);

	switch (label->type) {
	case label_normal:
	normal:
		/* no extra stuff */
		break;

	case label_box:
	{
#define glory_radius (radius * sqrt(M_PHI))

		if (!box_is_focused(label->base) ||
		    0 == label->base->focus_seq)
			goto normal;

		uint8_t i = 0;
		uint8_t focus_hands[MAX_NUM_HANDS];
		for (uint8_t j = 0; j < num_hands; ++j) {
			Hand const *const hand = &hands[j];
			if (hand->input_focus == label->base ||
			    hand->focus == label->base)
				focus_hands[i++] = j;
		}

		/* maybe an inbetween container that contains focused box */
		if (0 == i)
			goto normal;

		if (shape) {
			/* the shape is a circle, we do not have to care about colors */
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
			cairo_arc(cr, 0, 0, glory_radius, 0, 2 * M_PI);
			cairo_fill/* _preserve */(cr);
			break;
		}

#define TWELVE_OCLOCK (-M_PI / 2)

		double angle = TWELVE_OCLOCK;
		double const slice = 2 * M_PI / i;
		for (uint8_t j = 0; j < i; ++j, angle += slice) {
			Hand const *const hand = &hands[focus_hands[j]];

			cairo_set_source_rgb(cr, RGB8_TO_FLOATS(hand->color));
			cairo_move_to(cr, 0, 0);
			cairo_arc(cr, 0, 0, glory_radius,
					angle, angle + slice);
			cairo_fill(cr);
		}

#undef TWELVE_OCLOCK
#undef glory_radius
	}
		break;

	case label_hline:
	case label_vline:
		cairo_set_line_width(cr, 1);
		if (shape)
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

		if (label_hline == label->type) {
			cairo_move_to(cr, 0, -radius * M_PHI);
			cairo_line_to(cr, 0, +radius * M_PHI);
		} else {
			cairo_move_to(cr, -radius * M_PHI, 0);
			cairo_line_to(cr, +radius * M_PHI, 0);
		}

		cairo_set_source_rgb(cr, 0, 0, 0);
		cairo_stroke(cr);
		break;
	}

	if (!shape)
		/* label stroke */
		cairo_set_source_rgb(cr, 0, 0, 0);
	else
		cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
	cairo_move_to(cr, -te.width / 2, +te.height / 2);
	cairo_text_path(cr, name);
	cairo_set_line_width(cr, stroke_width + .5);
	cairo_stroke(cr);

	if (!shape)
		/* label color */
		cairo_set_source_rgb(cr, 1, 1, 0);
	cairo_move_to(cr, -te.width / 2, +te.height / 2);
	cairo_show_text(cr, name);

	if (!shape)
		cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	cairo_surface_flush(surface);
	cairo_surface_destroy(surface);
	cairo_destroy(cr);
}

static Label *
new_label_for(Box *const box)
{
	Label *label;
	Body *const body = &bodies[box->body];

	if (body->num_labels_used == body->num_labels) {
		size_t const new_size = (body->num_labels * 8 / 5 /* golden ratio */) + 1;
		body->labels = realloc(body->labels, new_size * sizeof *label);
		body->num_labels = new_size;

		memset(&body->labels[body->num_labels_used], 0,
				(body->num_labels - body->num_labels_used) * sizeof *body->labels);
	}

	label = &body->labels[body->num_labels_used++];
	label->position_changed |= box != label->base;
	label->base = box;
	label->hands = 0;
	label->type = label_normal;
	return label;
}

static void
print_error(xcb_generic_error_t const *const error, char const *const message)
{
	fprintf(stderr, "%s:%s: X error: "
#ifndef HEAWM_NDEBUG
			"%s"
#else
			"%d"
#endif
			" (seq=%d, opcode=%d:%d)\n",
			__FILE__, message,
#ifndef HEAWM_NDEBUG
			xcb_event_get_error_label(error->error_code),
#else
			error->error_code,
#endif
			error->sequence,
			error->major_code, error->minor_code);
}

maybe_unused
static bool
check_cookie(xcb_void_cookie_t const cookie, char const *const request)
{
	xcb_generic_error_t *error;

	if ((error = xcb_request_check(conn, cookie))) {
		print_error(error, request);
		free(error);
		return false;
	} else {
		return true;
	}
}

static void
label_set_name(Label *label, char name[static NAME_LEN])
{
	label->content_changed |= !!strncmp(label->name, name, NAME_LEN);
	memcpy(label->name, name, sizeof label->name);
}

static void
label_set_position(Label *const label, int16_t const x, int16_t const y)
{
	label->position_changed |=
		x != label->x ||
		y != label->y;
	label->x = x;
	label->y = y;
}

static void
label_set_position_from_box(Label *const label, int relx, int rely)
{
	Box const *const monitor = box_get_monitor(label->base);
	Size const size = monitor_convert_pt2px(monitor, label_rect);

	uint16_t x = label->base->x + ((relx + 1) * (label->base->width  - size.width )) / 2;
	uint16_t y = label->base->y + ((rely + 1) * (label->base->height - size.height)) / 2;

#define CLAMP(d, dim) \
	if (d < monitor->d) \
		d = monitor->d; \
	else if (monitor->d + monitor->dim < d + size.dim) \
		d = monitor->d + monitor->dim - size.dim;

	CLAMP(x, width)
	CLAMP(y, height)

#undef CLAMP

	label_set_position(label, x, y);
}

static void
label_create_window(Label *label)
{
	assert(label->base);
	Body const *const body = &bodies[label->base->body];
	xcb_screen_t const *const screen = body->screen;

	label->window = xcb_generate_id(conn);

	Box const *const monitor = box_get_monitor(label->base);
	Size const size = monitor_convert_pt2px(monitor, label_rect);

	DEBUG_CHECK(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			label->window,
			screen->root,
			label->x, label->y,
			size.width, size.height,
			2,
			XCB_WINDOW_CLASS_INPUT_OUTPUT,
			screen->root_visual,
			XCB_CW_SAVE_UNDER |
			XCB_CW_OVERRIDE_REDIRECT |
			XCB_CW_EVENT_MASK,
			&(uint32_t const[]){
				true,
				true,
				/* TODO: maybe we should handle enter/leave to hide/show labels? */
				XCB_EVENT_MASK_EXPOSURE
			});

	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			label->window, XCB_ATOM_WM_CLASS ,
			XCB_ATOM_STRING, 8,
			sizeof LABEL_INSTANCE "\0" LABEL_CLASS,
			LABEL_INSTANCE "\0" LABEL_CLASS);

	/* TODO: search for labels with the same name and type and copy shape from there */
	/* setup its shape */
	label->shape = xcb_generate_id(conn);

	DEBUG_CHECK(xcb_create_pixmap, conn,
		/* mask is on or off */
		1,
		label->shape,
		label->window,
		size.width, size.height);

	/* we need a valid pixmap so we use the bounding mask but we
	 * use offsets to move it outside of the area making effective
	 * input region empty */
	DEBUG_CHECK(xcb_shape_mask, conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_INPUT,
			label->window,
			size.width, size.height,
			label->shape);
}

static void
hide_label(Label *const label)
{
	DEBUG_CHECK(xcb_unmap_window, conn, label->window);
}

static void
delete_label(Label *const label)
{
	if (XCB_WINDOW_NONE != label->window) {
		DEBUG_CHECK(xcb_destroy_window, conn, label->window);
		DEBUG_CHECK(xcb_free_pixmap, conn, label->shape);
	}
}

static void
update_label(Label *const label)
{
	Body *const body = &bodies[label->base->body];
	bool const should_map = body->num_labels_mapped <= (label - body->labels);

	if (XCB_WINDOW_NONE == label->window)
		label_create_window(label);

	if (label->position_changed) {
		label->position_changed = false;
		/* move label to its place and make sure its above base window */
		/* FIXME: base->window may not be a sibling of label (for containers without title) */
		DEBUG_CHECK(xcb_configure_window, conn, label->window,
				XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y |
				/* (!box_is_container(label->base) ? XCB_CONFIG_WINDOW_SIBLING : 0) | */
				XCB_CONFIG_WINDOW_STACK_MODE,
				&(const uint32_t[]){
					label->x, label->y,
					/* (!box_is_container(label->base) ? label->base->window : XCB_STACK_MODE_ABOVE), */
					XCB_STACK_MODE_ABOVE
				});
	}

	if (1 || label->content_changed) {
		label->content_changed = false;
		label_repaint(label, true);

		DEBUG_CHECK(xcb_shape_mask, conn,
				XCB_SHAPE_SO_SET, XCB_SHAPE_SK_BOUNDING,
				label->window,
				0, 0,
				label->shape);
	}

	if (should_map) {
		assert(body->num_labels_mapped + 1 == body->num_labels_used);
		body->num_labels_mapped = body->num_labels_used;
		DEBUG_CHECK(xcb_map_window, conn, label->window);
		/* will be repainted at exposure */
	} else {
		label_repaint(label, false);
	}
}

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
	print_strerror("execvp");
}

static void
quit(void)
{
	/* BROADCAST(quit, &(struct quit_args){0}); */

	if (conn) {
		if (xrm)
			xcb_xrm_database_free(xrm);
		if (symbols)
			xcb_key_symbols_free(symbols);

		xcb_disconnect(conn);
	}
}

static void
handle_signal_restart(int signum)
{
	(void)signum;

	restart();
}

static void
handle_signal_quit(int signum)
{
	(void)signum;

	/*MAN(HOOKS)
	 * .TP
	 * .B exit
	 * Run before exiting.
	 */
	SPAWN(heawm_file("exit"));

	exit(EXIT_SUCCESS);
}

static void
init_atoms(void)
{
	xcb_intern_atom_cookie_t cookies[ARRAY_SIZE(ATOM_NAMES)];

	for (unsigned i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		char const *const name = ATOM_NAMES[i];
		cookies[i] = xcb_intern_atom_unchecked(conn, false, strlen(name), name);
	}

	for (unsigned i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		xcb_intern_atom_reply_t *const reply =
			xcb_intern_atom_reply(conn, cookies[i], NULL);

		if (!reply)
			continue;

		atoms[i] = reply->atom;
		free(reply);
	}
}

/* return whether search finished */
static bool
find_box_by_name(Box **optimum, char name[static NAME_LEN])
{
	*optimum = NULL;

	uint8_t const n = strnlen(name, NAME_LEN);
	if (0 == n)
		return false;

	bool complete = true;
	Box *box;
	for_each_box(box, root, root, {
		if (0 == memcmp(name, box->name, n)) {
			complete &= NAME_LEN <= n || '\0' == box->name[n];
			if (!*optimum || (*optimum)->focus_seq < box->focus_seq)
				*optimum = box;
		}
	})

	return complete;
}

static Box *
box_propagate_change(Box *box)
{
	Box *const ret = box;
	while ((box = box->parent))
		box->content_changed = true;
	return ret;
}

static bool
name_box(Box *const box, bool const iscontainer)
{
	/* collect the min distance, max focus_seq */
	struct {
		uint32_t focus_seq;
	} letters[127];
	/* then search for max distance, min focus_seq */

	memset(letters, 0, sizeof letters);

	uint8_t const n = strnlen(box->name, NAME_LEN);

	/* TODO: if box has name, leave it and check if there are conflicts in its parent */
	if (0 < n) {
		return true;
	}

	Box *to = box->parent;

	/* prohibit same name among children so we can always move horizontally */
	if (to)
		for (uint16_t i = 0; i < to->num_children; ++i) {
			Box const *const child = to->children[i];
			if (0 == memcmp(child->name, box->name, n))
				letters[(unsigned char)child->name[n]].focus_seq = -1;
		}

	/* prohibit same name in subtree so we can always move vertically */
	for (to = box; (to = to->parent);)
		if (0 == memcmp(to->name, box->name, n))
			letters[(unsigned char)to->name[n]].focus_seq = -1;

	unsigned char optimum = '\0';
	letters[optimum].focus_seq = -1;

	for_each_box(to, root, root, {
		if (0 == memcmp(to->name, box->name, n)) {
			uint32_t *const p = &letters[(unsigned char)to->name[n]].focus_seq;
			if (*p < to->focus_seq + 1)
				*p = to->focus_seq + 1;
		}
	})

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

	if (!box_is_container(box))
		DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				box->window, ATOM(_HEAWM_NAME),
				XCB_ATOM_STRING, 8,
				n + 1, box->name);

	/* name changed, labels need to be updated */
	box_propagate_change(box)->content_changed = true;

	return true;
}

static bool
box_is_floating(Box const *const box)
{
	return 0 < box->user_width;
}

static uint16_t
box_compute_num_columns(Box const *const box, uint16_t num_tiles)
{
	if (0 == box->num_columns) {
		/* compare whether adding a new row or a new column will result
		 * in more squary tiles. the process repats until we can place
		 * all tiles. */
		uint_fast16_t cols = 1, rows = 1;
		while (cols * rows < num_tiles) {
#define R(a, b) ((a) < (b) ? (uint32_t)(b) << 16 / (a) : (uint32_t)(a) << 16 / (b))

			if (R(box->width / (cols + 1), box->height / rows) <
			    R(box->width / cols,       box->height / (rows + 1)))
				++cols;
			else
				++rows;

#undef R
		}

		return cols;
	} else {
		return box->num_columns <= num_tiles
			? box->num_columns
			: num_tiles;
	}
}

static bool
box_is_split(Box const *const box)
{
	uint16_t const num_columns = box_compute_num_columns(box, box->num_children);
	return 1 == num_columns || box->num_children <= num_columns;
}

static void
box_set_position(Box *const box, int16_t x, int16_t y)
{
	box->position_changed |=
		x != box->x ||
		y != box->y;
	box->x = x;
	box->y = y;
}

static void
box_set_size(Box *const box, uint16_t const width, uint16_t const height)
{
	box->layout_changed |=
		width != box->width ||
		height != box->height;
	box->width = width;
	box->height = height;
}

static bool
box_get_focus(Box *const box, Box **const focus)
{
	if (0 == box->num_children)
		return false;

	Box *const *child = box->children;
	while (box->focus_seq != (*child)->focus_seq)
		++child;

	*focus = *child;
	return true;
}

static xcb_window_t
hand_get_wanted_focus(Hand const *const hand)
{
	return hand->input_focus
		? hand->input_focus->window
		: bodies[0].screen->root;
}

#define Box_pointers(box) ((BoxPointer *)&box->children)

static void
box_save_pointer(Box *const box, Hand const *const hand)
{
	GET_REPLY(reply, xcb_input_xi_query_pointer, conn,
			bodies[box->body].screen->root, hand->master_pointer);
	if (!reply)
		return;

	if (!reply->same_screen)
		goto out;

	/* only care about cases when pointer is above a window that we manage
	 * (potentially a non-temporal window) */
	Box const *const base = find_box_by_window(root, root, reply->child);
	if (!base)
		goto out;

	Box_pointers(box)[hand - hands] = (BoxPointer){
		.window = base->window,
		.x = reply->root_x - (base->x << 16),
		.y = reply->root_y - (base->y << 16),
	};

out:
	free(reply);
}

static void
box_restore_pointer(Box const *const box, Hand const *const hand)
{
	BoxPointer pointer = Box_pointers(box)[hand - hands];

	/* center pointer for new boxes */
	if (XCB_WINDOW_NONE == pointer.window) {
		pointer.window = bodies[box->body].screen->root;
		pointer.x = (xcb_input_fp1616_t)(box->x + box->width / 2) << 16;
		pointer.y = (xcb_input_fp1616_t)(box->y + box->height / M_PHI) << 16;
	}

	DEBUG_CHECK(xcb_input_xi_warp_pointer, conn,
			XCB_WINDOW_NONE,
			pointer.window,
			0, 0, 0, 0,
			pointer.x, pointer.y,
			hand->master_pointer);
}

static void
hand_refocus(Hand const *const hand)
{
	Box const *const focus = hand->input_focus;
	assert(!focus || !box_is_container(focus));
	assert(!focus || box_is_focused(focus));

	DEBUG_CHECK(xcb_input_xi_set_focus, conn,
			hand_get_wanted_focus(hand),
			XCB_CURRENT_TIME, hand->master_keyboard);
	DEBUG_CHECK(xcb_input_xi_set_client_pointer, conn,
			focus ? focus->window : XCB_WINDOW_NONE,
			hand->master_pointer);
}

static void
hand_focus(Hand const *const hand)
{
	hand_refocus(hand);

	if (hand->input_focus)
		box_restore_pointer(hand->input_focus, hand);
}

enum {
	left = -1,
	top = -1,
	center = 0,
	right = 1,
	bottom = 1,
};

static bool
box_is_visible(Box *const box)
{
	return !box->concealed || box->parent->focus_seq == box->focus_seq;
}

static bool
box_is_tiled(Box *const box)
{
	return box_is_visible(box) && !box_is_floating(box);
}

static uint16_t
box_get_num_tiles(Box *const box)
{
	uint16_t ret = 0;

	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box *child = box->children[i];
		if (box_is_tiled(child))
			++ret;
	}

	return ret;
}

static void
label_assign_hand(Label *const label, Hand const *const hand)
{
	label->hands |= 1 << (hand - hands);
}

static void
generate_name(char name[static NAME_LEN], uint32_t number)
{
	enum { N_SYMBOLS = 'z' - 'a' + 1 };

	memset(name, 0, NAME_LEN);

	char *p = name;
	do {
		*p++ = 'a' + (number % N_SYMBOLS);
	} while (0 < (number /= N_SYMBOLS));

	if (p < name + NAME_LEN)
		*p = '\0';

	for (char *q = name; q < p;)
		*q++ = *--p;
}

#if 0
/* TODO: do not regenerate labels on focus change */
static void
box_repaint_labels(Box const *const box)
{
	Body *body = &bodies[box->body];
	for (uint32_t j = body->num_labels_used; 0 < j;) {
		Label *const label = &body->labels[--j];
		if (box == label->base)
			label_repaint(label, false);
	}
}
#endif

static void
box_delete_labels(Box const *const box)
{
	Body *body = &bodies[box->body];
	for (uint32_t j = body->num_labels_used; 0 < j;) {
		Label *const label = &body->labels[--j];
		if (box == label->base) {
			Label const tmp = *label;
			*label = body->labels[--body->num_labels_used];
			body->labels[body->num_labels_used] = tmp;
		}
	}
}

static void
box_update_label(Box *const box)
{
	if (box == root)
		return;

	box_delete_labels(box);

	for_each_hand {
		char name[NAME_LEN];
		Label *label;

		switch (hand->mode) {
		case mode_default:
			if (box->hide_label)
				break;
			/* fall through */
		case mode_name:
			label = new_label_for(box);
			if (mode_name == hand->mode && hand->mode_box == box) {
				char display_name[NAME_LEN + 1];
				memcpy(display_name, hand->user_input, sizeof display_name);
				display_name[strnlen(hand->user_input, NAME_LEN)] = '?';
				label_set_name(label, display_name);
			} else {
				label_set_name(label, box->name);
			}
			label_set_position_from_box(label, box_is_container(box) ? center : right, top);
			label->type = label_box;
			label_assign_hand(label, hand);
			update_label(label);
			break;

		case mode_move_a:
		case mode_move_b:
		{
			/* not a real child */
			if (box_is_monitor(box))
				break;

			/* FIXME: check if hand really focuses */
			bool const focused = box_is_focused(box) && !box_is_container(box);

			generate_name(name, focused ? 'h' - 'a' : hand->num_labels++);

			label = new_label_for(box);
			label_assign_hand(label, hand);
			label_set_name(label, name);
			label_set_position_from_box(label, left, center);
			update_label(label);

			generate_name(name, focused ? 'j' - 'a' : hand->num_labels++);
			label = new_label_for(box);
			label_assign_hand(label, hand);
			label_set_name(label, name);
			label_set_position_from_box(label, center, bottom);
			update_label(label);

			generate_name(name, focused ? 'k' - 'a' : hand->num_labels++);
			label = new_label_for(box);
			label_assign_hand(label, hand);
			label_set_name(label, name);
			label_set_position_from_box(label, center, top);
			update_label(label);

			generate_name(name, focused ? 'l' - 'a' : hand->num_labels++);
			label = new_label_for(box);
			label_assign_hand(label, hand);
			label_set_name(label, name);
			label_set_position_from_box(label, right, center);
			update_label(label);
		}
			break;

		case mode_setcolumns:
		{
			if (box->parent != hand->mode_box->parent)
				break;

			label = new_label_for(box);
			name[0] = 'a' + hand->num_labels++;
			name[1] = '\0';
			label_set_name(label, name);
			label_set_position_from_box(label, center, center);
			update_label(label);
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
			strncpy(name, "h", sizeof name);
			label_set_name(label, name);
			label_set_position_from_box(label, left, center);
			update_label(label);

			label = new_label_for(box);
			strncpy(name, "j", sizeof name);
			label_set_name(label, name);
			label_set_position_from_box(label, center, bottom);
			update_label(label);

			label = new_label_for(box);
			strncpy(name, "k", sizeof name);
			label_set_name(label, name);
			label_set_position_from_box(label, center, top);
			update_label(label);

			label = new_label_for(box);
			strncpy(name, "l", sizeof name);
			label_set_name(label, name);
			label_set_position_from_box(label, right, center);
			update_label(label);

		}
			break;

		case mode_size_to:
		{
			if (box != hand->mode_box)
				break;

			Box const *const monitor = box_get_monitor(box);
			Size const size = monitor_convert_pt2px(monitor, label_rect);

			for (char ch = 'a'; ch <= 'z'; ++ch) {
				label = new_label_for(box);
				name[0] = ch;
				name[1] = '\0';
				label_set_name(label, name);
				label_set_position(label, box->x + (ch - 'a') * size.width, box->y + box->height / 2);
				update_label(label);
			}
		}
			break;
		}
	}
}

static void
delete_box(Box *box);

static void
update_box(Box *const box)
{
	static int depth = 0;

#ifndef HEAWM_DEBUG_UPDATE
#define printf(...) (0)
#endif

	if (box != root)
	printf("%*.sbox %p { N=%d title=\"%s\" %sfocus=#%d/%d/%d name=%.*s %s%s}\n",
			depth, "",
			(void *)box,
			box->num_children,
			box->title,
			box->focus_lock ? "focus_lock " : "",
			box->focus_seq, box->focus_hand, root->focus_seq,
			NAME_LEN, box->name,
			box->concealed ? "concealed " : "",
			box_is_floating(box) ? "floating " : "");

	depth += 3;

	uint32_t mask = 0;
	uint32_t list[7];
	uint8_t i = 0;

	if (box != root)
	printf("%*.sw=%x pos=%s(%d,%d) size=%s(%d,%d)\n", depth, "",
			box->window,
			box->position_changed ? "*" : "",
			box->x, box->y,
			box->layout_changed ? "*" : "",
			box->width, box->height);

	bool const content_changed = box->content_changed;
	bool const position_changed = box->position_changed;
	/* because of relative position values, layout change must be
	 * propagated so children will be repositioned correctly */
	bool const focus_changed = box->focus_changed;
	bool const layout_changed = box->layout_changed || position_changed || focus_changed;
	bool const label_changed = box->label_changed || position_changed;
	bool should_map = false;

	box->layout_changed = false;
	box->content_changed = false;
	box->position_changed = false;
	box->focus_changed = false;
	box->title_changed = false;
	box->label_changed = false;

	if (position_changed) {
		mask |=
			XCB_CONFIG_WINDOW_X |
			XCB_CONFIG_WINDOW_Y;
		list[i++] = box->x;
		list[i++] = box->y;
	}

	if (layout_changed && box->parent) {
		if (0 < box->width) {
			mask |=
				XCB_CONFIG_WINDOW_WIDTH |
				XCB_CONFIG_WINDOW_HEIGHT;
			list[i++] = box->width;
			list[i++] = box->height;

			if (0 == box->num_children && box->parent && box->parent->parent)
				goto not_a_container;

			uint32_t total_weight = 0;
			uint16_t tiled_children = 0;

			for (uint16_t i = 0; i < box->num_children; ++i) {
				Box *child = box->children[i];
				if (!box_is_tiled(child))
					continue;

				total_weight += child->weight;
				++tiled_children;
			}

			uint16_t num_columns = box_compute_num_columns(box, tiled_children);
			uint16_t num_rows = (tiled_children + num_columns - 1) / num_columns, col = 0;
			int16_t y = 0, x = 0;
			uint16_t height = box->height / (!num_rows ? 1 : num_rows);
			uint32_t i = 0;

			printf("%*.sgrid=nchildren=%d; ntiles=%d; cols=%d\n",
					depth, "", box->num_children, tiled_children, num_columns);
			uint16_t width = box->width / num_columns;
			for (uint16_t i = 0; i < box->num_children; ++i) {
				Box *child = box->children[i];

				/* printf("%*.s[#%d]:\n", depth, "", i); */

				if (box_is_tiled(child)) {
					struct {
						uint16_t left;
						uint16_t top;
						uint16_t right;
						uint16_t bottom;
					} gap = { box_is_container(child) || box->focus_lock ? 0 : 0, 0, 0, 0 };
					gap.top = gap.left;
					gap.right = gap.left;
					gap.bottom = gap.left;
					/* per window width/height:
					 * - last children in column or row eats up all space,
					 * - size is rounded to the nearest mod_{x,y}
					 */
					uint16_t const wwidth =
						col + 1 == num_columns
						? box->width - x
						: width + (1 == num_rows && 1 < tiled_children && 0 < child->mod_x ? child->mod_x / 2 - (width + child->mod_x / 2) % child->mod_x : 0);
					uint16_t const wheight =
						1 == tiled_children
						? box->height - y
						: height + (1 == num_columns && 1 < tiled_children && 0 < child->mod_y ? child->mod_y / 2 - (height + child->mod_y / 2) % child->mod_y : 0);
					box_set_position(child, box->x + x + gap.left, box->y + y + gap.top);
					box_set_size(child, wwidth - (gap.left + gap.right), wheight - (gap.top + gap.bottom));

					--tiled_children;
					if (++col < num_columns) {
						x += wwidth;
					} else {
						col = 0;
						x = 0;
						y += wheight;

						if (0 < tiled_children && tiled_children < num_columns) {
							num_columns = tiled_children;
							width = box->width / num_columns;
						}
					}
				} else if (!box_is_visible(child)) {
					box_set_size(child, 0, 0);
				}

				update_box(child);
			}
		not_a_container:;
			  should_map = true;
		} else {
			if (box->window != XCB_WINDOW_NONE) {
				printf("%*.s unmap %x\n", depth, "", box->window);
				box_delete_labels(box);
				xcb_unmap_window(conn, box->window);
			}

			for (uint16_t i = 0; i < box->num_children; ++i) {
				Box *child = box->children[i];
				box_set_size(child, 0, 0);
			}

			depth -= 3;
			return;
		}
	} else if (content_changed) {
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box *const child = box->children[i];
			update_box(child);
		}
	}

	/* if (e->value_mask & XCB_CONFIG_WINDOW_SIBLING) */
	/* if (e->value_mask & XCB_CONFIG_WINDOW_STACK_MODE) */

# if 0
					DEBUG_CHECK(xcb_change_window_attributes, conn, box->window,
							XCB_CW_BORDER_PIXEL, &(uint32_t const){
								hand->color
							});
			DEBUG_CHECK(xcb_change_window_attributes, conn, box->window,
					XCB_CW_BORDER_PIXEL, &(uint32_t const){
						0xff0000
					});

		DEBUG_CHECK(xcb_configure_window, conn, box->window, XCB_CONFIG_WINDOW_BORDER_WIDTH, &(uint32_t const){ 0 });
#endif

	if (box->window != XCB_WINDOW_NONE && 0 < box->width) {
		if (0 < i) {
			mask |= XCB_CONFIG_WINDOW_STACK_MODE;
			list[i++] = XCB_STACK_MODE_BELOW;
			printf("%*.s configure %d\n", depth, "", i);
			DEBUG_CHECK(xcb_configure_window, conn, box->window, mask, list);
		}

		/* map only after configured */
		if (should_map) {
			printf("%*.s map\n", depth, "");
			if (!CHECK(xcb_map_window, conn, box->window))
			{
				/* if we could not map the window that means
				 * that something bad happened with the window
				 * since we received map_request. we will not
				 * receive unmap_notify to delete the box
				 * because we have not yet mapped it, so we
				 * must act now. */
				delete_box(box);
				goto out;
			}
		}

		/* xcb_configure_window(conn, box->window, XCB_CONFIG_WINDOW_BORDER_WIDTH, &(uint32_t const){ 4 }); */

		if (box_is_focused(box) && !box_is_container(box) && focus_changed) {
			/* hand focus can only be updated after window is mapped */
			for_each_hand {
				Box const *const focus = hand->input_focus;
				if (box == focus)
					hand_focus(hand);
			}

#if 0
			if (!(should_map || label_changed))
				box_repaint_labels(box);
#endif
		}

		if (should_map || label_changed) {
			box_update_label(box);
		}
	}

out:
	depth -= 3;

#ifndef HEAWM_DEBUG_UPDATE
#undef printf
#endif
}

/* realize our virtual boxes for the X server and flush */
static void
do_update(void)
{
	do
		update_box(root);
	while (root->content_changed);

	for_each_body {
		assert(body->num_labels_used <= body->num_labels_mapped);
		while (body->num_labels_used < body->num_labels_mapped) {
			Label *label = &body->labels[--body->num_labels_mapped];
			hide_label(label);
		}
	}
}

/* ensure that parent maximum */
static void
box_update_focus_seq(Box *box)
{
	assert(box_is_container(box));
	uint32_t old_focus_idx = box->focus_seq;

	do {
		uint32_t max_focus_seq = 0;
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box *const child = box->children[i];
			if (max_focus_seq < child->focus_seq)
				max_focus_seq = child->focus_seq;
		}
		/* we unparented the focused box so maximum dropped */
		if (box->focus_seq == max_focus_seq)
			break;

		box->content_changed = true;
		/* maximum may changed but not the focused children */
		box->focus_changed = box->focus_seq != old_focus_idx;
		/* if (box->parent)
			box->parent->layout_changed = true; */

		old_focus_idx = box->focus_seq;
		box->focus_seq = max_focus_seq;
	} while ((box = box->parent));

	for (; box; box = box->parent)
		box->content_changed = true;
}

static void
take_box(Box *const box, bool const out_of_parent)
{
	Box *parent = box->parent;
	Box **child = &parent->children[0];
	while (box != *child)
		++child;

	/* pos of |box| in its parent */
	uint32_t const pos = child - parent->children;

	memmove(
		child,
		child + 1,
		(--parent->num_children - pos) * sizeof *child
	);

	if (!out_of_parent)
		return;

	box_update_focus_seq(parent);
}

/* called after box lost a child */
static void vacuum_box(Box *box);

static void
hand_focus_box(Hand *hand, Box *box);

static void
find_most_recent_box(Hand *hand, Box *root, uint32_t const focus_seq, Box **boxes, uint32_t n)
{
	static Box const EMPTY_BOX;

	/* use a dummy box so we do not have to check for NULL */
	for (uint32_t i = 0; i < n; ++i)
		/* cast is safe because we do not write boxes and they
		 * will be NULLed out at the end */
		boxes[i] = (Box *)&EMPTY_BOX;

	Box *box;
	for_each_box(box, root, root, {
		if (/* is it a client window? */
		    !box_is_container(box) &&
		    /* if not find a box that... */
		    (box->focus_hand == NULL_HAND || (/* is not focused by any hand; so we do not interfere */
		     focus_seq != box->focus_seq &&
		     /* but was focused by the current hand; it's in the history */
		     box->focus_hand == hand - hands) || (box == hand->input_focus)))
		{
			uint32_t i = n;
			while (i > 0 && boxes[i - 1]->focus_seq <= box->focus_seq)
				--i;
			if (i < n) {
				memmove(&boxes[i], &boxes[i + 1], (n - i - 1) * sizeof *boxes);
				boxes[i] = box;
			}
		}
	})

	for (uint32_t i = n; 0 < i && &EMPTY_BOX == boxes[--i];)
		boxes[i] = NULL;
}

/* focus something */
static void
focus_all_hands(uint32_t focus_seq)
{
	for_each_hand {
		if (hand->input_focus)
			continue;

		Box *optimum;
		find_most_recent_box(hand, root, focus_seq, &optimum, 1);

		if (optimum) {
			hand_focus_box(hand, optimum);
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
hand_do_mode_changes(Hand *const hand)
{
	switch (hand->mode) {
	case mode_default:
		/* noop */
		break;

	case mode_size_side:
	case mode_size_to:
		break;

	case mode_move_a:
	case mode_move_b:
	{
		Box *box;
		for_each_box(box, root, root, {
			box->label_changed = true;
			box->content_changed = true;
		})
	}
		break;

	case mode_setcolumns:
		box_propagate_change(hand->mode_box);
		for (uint16_t i = 0; i < hand->mode_box->parent->num_children; ++i) {
			Box *const child = hand->mode_box->parent->children[i];
			child->label_changed = true;
		}
		break;

	case mode_name:
		box_propagate_change(hand->mode_box)->label_changed = true;
		break;
	}

	hand->num_labels = 0;

	uint64_t const hand_anti_mask = ~(1 << (hand - hands));
	for_each_body {
		for (uint32_t j = body->num_labels_used; 0 < j;) {
			Label *const label = &body->labels[--j];
			if ((label->hands &= hand_anti_mask))
				continue;

			Label const tmp = *label;
			*label = body->labels[--body->num_labels_used];
			body->labels[body->num_labels_used] = tmp;
		}
	}
}

static void
hand_leave_mode(Hand *const hand)
{
	hand_do_mode_changes(hand);

	hand->mode = mode_default;
	hand->mode_box = NULL;
}

static void
ewmh_append_client_list(Box const *const box)
{
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_APPEND,
			bodies[box->body].screen->root, ATOM(_NET_CLIENT_LIST),
			XCB_ATOM_WINDOW, 32,
			1, &box->window);
}

static void
ewmh_delete_client_list(Body const *const body)
{
	DEBUG_CHECK(xcb_delete_property, conn, body->screen->root, ATOM(_NET_CLIENT_LIST));
}

static void
ewmh_update_client_list(Body const *const body)
{
	/* FIXME: maybe we hit max message len? */
	xcb_window_t windows[1000];
	uint32_t num_windows = 0;
	xcb_prop_mode_t mode = XCB_PROP_MODE_REPLACE;

	Box *box;
	for_each_box(box, root, root, {
		if (box->body == (body - bodies) && !box_is_container(box)) {
			windows[num_windows++] = box->window;
			if (ARRAY_SIZE(windows) == num_windows) {
				DEBUG_CHECK(xcb_change_property, conn, mode,
						body->screen->root, ATOM(_NET_CLIENT_LIST),
						XCB_ATOM_WINDOW, 32,
						num_windows, windows);
				num_windows = 0;
				mode = XCB_PROP_MODE_APPEND;
			}
		}
	})

	if (0 < num_windows)
		DEBUG_CHECK(xcb_change_property, conn, mode,
				body->screen->root, ATOM(_NET_CLIENT_LIST),
				XCB_ATOM_WINDOW, 32,
				num_windows, windows);
}

static void
free_box(Box *box)
{
	/* if (XCB_WINDOW_NONE != box->frame)
		xcb_destroy_window(conn, box->frame); */

	free(box->title);
	free(box);
}

static void
delete_box(Box *box)
{
	/* if we removed the currently selected box, and there is only one
	 * hand, root->focus_seq will dropped that could make a previously
	 * selected seems like it's selected (possibly by someone other) */
	uint32_t const real_focus_seq = root->focus_seq;
	bool focus_changed = false;

	for_each_hand {
		/* forget current focus */
		if (box == hand->focus)
			hand->focus = NULL;

		if (box == hand->input_focus) {
			focus_changed = true;
			hand->input_focus = NULL;
			assert(box_is_focused(box));
		}

		if (box == hand->latest_input[0]) {
			hand->latest_input[0] = hand->latest_input[1];
			hand->latest_input[1] = NULL;
			assert(hand->latest_input[0] != box);
		} else if (box == hand->latest_input[1]) {
			hand->latest_input[1] = NULL;
		}

		if (box == hand->mode_box)
			hand_leave_mode(hand);
	}

	box_delete_labels(box);

	if (focus_changed)
		increase_focus_seq();

	take_box(box, true);

	if (focus_changed)
		focus_all_hands(real_focus_seq);

	if (!box_is_container(box))
		/* TODO: do not update immediately */
		ewmh_update_client_list(&bodies[box->body]);

	vacuum_box(box->parent);
	free_box(box);
}

static uint16_t
box_get_pos(Box const *const box)
{
	uint16_t pos = 0;
	while (box != box->parent->children[pos])
		++pos;
	return pos;
}

static void
put_box(Box *into, uint16_t const pos, Box *box);

static void
vacuum_box(Box *box)
{
	assert(box_is_container(box) && "for containers only");

	if (box == root ||
	    box_is_monitor(box))
		goto not_empty;

	if (1 == box->num_children && !box_is_leg(box) && !box->no_vacuum) {
		/* move the only children of box up one, at the place of the box */
		put_box(box->parent, box_get_pos(box), box->children[0]);
		return;
	}

	if (0 < box->num_children)
		goto not_empty;

	/* body boxes are special because they can be deleted only if their
	 * twin boxes on heads are empty too */
	if (box_is_leg(box)) {
		uint16_t const pos = box_get_pos(box);

		for (uint16_t i = 0; i < root->num_children; ++i) {
			Box const *const head = root->children[i];
			Box const *const leg = head->children[pos];
			if (0 < leg->num_children)
				goto not_empty;
		}

		for (uint16_t i = 0; i < root->num_children; ++i) {
			Box const *const head = root->children[i];
			Box *const leg = head->children[pos];
			delete_box(leg);
		}
	} else {
		delete_box(box);
	}

	return;

not_empty:
	/* ...but still changed */
	box_propagate_change(box)->layout_changed = true;
}

static Box *
move_box(Box *const new, void const *const old)
{
	if (old == new)
		return new;

#define UPDATE_REF(var) if (old == (var)) var = new;

	for_each_hand {
		UPDATE_REF(hand->latest_input[0]);
		UPDATE_REF(hand->latest_input[1]);
		UPDATE_REF(hand->focus);
		/* NOTE: |input_focus| do not need to be updated since |old| is a container */
		UPDATE_REF(hand->mode_box);
	}

	if (new->parent) {
		Box **child = new->parent->children;

		while (old != *child)
			++child;

		*child = new;
	} else {
		root = new;
	}

	for (uint16_t i = 0; i < new->num_children; ++i) {
		Box *const child = new->children[i];
		child->parent = new;
	}

	for_each_body {
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			Label *const label = &body->labels[j];
			UPDATE_REF(label->base);
		}
	}

#undef UPDATE_REF

	return new;
}

/* box must not be a root because box->parent == NULL => new box */
static void
put_box(Box *into, uint16_t const pos, Box *box)
{
	assert(box_is_container(into));

	Box *const boxparent = box->parent;

	/* unparent |box| */
	if (boxparent)
		take_box(box, into != boxparent);

	/* already its parent */
	if (into == boxparent)
		goto insert;

	/* first check if we can make place for |box| in |into| */
	{
		Box *const new = realloc(into, offsetof(Box, children[into->num_children + 1]));
		into = move_box(new, into);
	}

	box->parent = into;
	++into->num_children;
	/* inherit some values from parent */
	if (into->parent) {
		box->focus_lock = into->parent->focus_lock;
		box->body = into->parent->body;
	}

insert:
	assert(pos <= into->num_children - 1);

	memmove(
		into->children + pos + 1,
		into->children + pos,
		(into->num_children - 1 - pos) * sizeof *into->children
	);
	into->children[pos] = box;
	into->layout_changed = true;

	if (boxparent)
		vacuum_box(boxparent);

	if (into != boxparent)
		box_update_focus_seq(into);
}

static Box *
new_box(void)
{
	Box *box = calloc(1, sizeof *box + num_hands * sizeof(BoxPointer));
	box->focus_hand = NULL_HAND;
	/* an initially zeroed out box->{x,y} does not mean box->window is
	 * really at 0,0 and without it no reconfiguration would happen */
	box->position_changed = true;
	return box;
}

static uint32_t
box_get_depth(Box const *box)
{
	uint32_t i = 0;
	for (; box->parent; ++i)
		box = box->parent;
	return i;
}

#if 0
static void
user_place_box(
		Box *const basea, int const relbasea,
		Box *const baseb, int const relbaseb,
		Box *box
) {
	uint32_t const deptha = box_get_depth(basea);
	uint32_t const depthb = box_get_depth(baseb);
	uint32_t depthdiff;

#define SWAP(x, y) (x ^= y, y ^= x, x ^= y)

	if (depthb < deptha) {
		depthdiff = deptha - depthb;
		SWAP(basea, baseb);
		SWAP(relbasea, relbaseb);
	} else {
		depthdiff = deptha - depthb;
	}

	do {

	} while (0 < depthdiff--);

#undef SWAP

}
#endif

static void
user_put_box(Box *into, uint32_t const pos, Box *box)
{
	if (!box_is_monitor(into)) {
		put_box(into, pos, box);
		return;
	}

	/* if parent box is a monitor it needs special handling, namely:
	 * - first,
	 * - second.
	 */

	/* alread there */
	if (into == box->parent) {
		uint16_t const pos = box_get_pos(box);

		for (uint16_t i = 0; i < root->num_children; ++i) {
			Box *const head = root->children[i];
			put_box(head, pos, head->children[pos]);
		}

		return;
	}

	char *name;
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		Box *const leg = new_box();

		if (0 == i) {
			name_box(leg, true);
			name = leg->name;
		} else {
			/* copy assigned name for further body_roots */
			memcpy(leg->name, name, sizeof leg->name);
		}

		/* leg->concealed = true; */

		put_box(head, pos, leg);

		if (into == head) {
			/* NOTE: |head| may be moved */
			Box *const head = root->children[i];
			put_box(head->children[pos], 0, box);
		}
	}
}

static void
box_set_focus_lock(Box *const box, bool const enable)
{
	if (enable != box->focus_lock) {
		box->focus_lock = enable;
		box->layout_changed = true;
	}
}

#if 0
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
#endif

static void
increase_focus_seq(void)
{
	uint32_t const old_focus_seq = root->focus_seq;
	++root->focus_seq;

	for_each_hand {
		Box *box = hand->input_focus;

		if (!box)
			continue;

		box->focus_hand = hand_index;

		do {
			box->content_changed = true;
			box->focus_seq = root->focus_seq;
			box->focus_changed |= box->focus_seq != old_focus_seq;
		} while ((box = box->parent));
	}
}

static void
swap_boxes(Box *one, Box *other)
{
	if (!one || !other)
		return;

	if (one == other)
		return;

	Box **pone;
	Box **pother;

	for (pone = one->parent->children; *pone != one; ++pone);
	for (pother = other->parent->children; *pother != other; ++pother);

	Box *const other_parent = other->parent;
	(*pone = other)->parent = one->parent;
	(*pother = one)->parent = other_parent;

	one->parent->layout_changed = true;
	other->parent->layout_changed = true;

	box_update_focus_seq(one->parent);
	box_update_focus_seq(other->parent);
}

static void
hand_focus_box(Hand *hand, Box *box)
{
	if (hand->input_focus)
		box_save_pointer(hand->input_focus, hand);

	if (hand->input_focus)
		box_propagate_change(hand->input_focus)->label_changed = true;
	else if (hand->focus)
		box_propagate_change(hand->focus)->label_changed = true;
	box_propagate_change(box)->label_changed = true;

	/* find most upper locked box */
	Box *locked = box;
	{
		Box *test = box;
		while (test->parent) {
			test = test->parent;
			if (test->focus_lock)
				locked = test;
		}
	}

	Box *recents[2];
	find_most_recent_box(hand, locked, root->focus_seq, recents, ARRAY_SIZE(recents));
	if (locked != box && recents[0] != box)
		swap_boxes(recents[1], recents[0]);

	hand->focus = box;

	Box *const new_input_focus = !box_is_container(box)
		? box
		: recents[0];

	/* Avoid moving pointer if the window under it is going to be focused.
	 *
	 * Rationale: User has moved out the pointer of the currently focused
	 * window and now wants to focus window under the cursor. User already
	 * knows the pointer location so it would be very unintuitive if it
	 * would be randomly moved out of sight. */
	if (hand->input_focus && new_input_focus->window == Box_pointers(hand->input_focus)[hand - hands].window)
		memcpy(&Box_pointers(new_input_focus)[hand - hands],
		       &Box_pointers(hand->input_focus)[hand - hands],
		       sizeof(BoxPointer));

	hand->input_focus = new_input_focus;

	uint32_t const old_focus_seq = root->focus_seq;

	increase_focus_seq();

	if (locked != box) {
		swap_boxes(box, recents[1]);
		/* if there was no such box, that's not a problem: |box| will
		 * be that beginning from now */
	}
}

static Body *
get_body_by_root(xcb_window_t const root_window)
{
	Body *body = bodies;
	while (root_window != body->screen->root)
		++body;
	return body;
}

static void
box_update_size_hints(Box *const box, xcb_size_hints_t const *const hints)
{
	if (XCB_ICCCM_SIZE_HINT_P_RESIZE_INC & hints->flags) {
		box->mod_y = hints->height_inc;
		box->mod_x = hints->width_inc;
	}
}

#define XI_EVENT_MASK(deviceid, mask) \
	(xcb_input_event_mask_t const *)&(struct xcb_input_event_mask1 { \
		xcb_input_event_mask_t ma##sk; \
		uint32_t values[1]; \
	} const){ \
		{ \
			.device##id = deviceid, \
			.mask_len = ARRAY_SIZE(((struct xcb_input_event_mask1 *)0)->values) \
		}, \
		{ mask } \
	}

/* manage window */
static Box *
box_window(xcb_window_t const root_window, xcb_window_t const window)
{
	if (!CHECK(xcb_change_window_attributes, conn, window,
				XCB_CW_EVENT_MASK,
				&(uint32_t const []){ EVENT_MASK }))
		return NULL;

	DEBUG_CHECK(xcb_input_xi_select_events, conn, window, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
					XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
					XCB_INPUT_XI_EVENT_MASK_ENTER));

	xcb_get_property_cookie_t cookies[] = {
		xcb_get_property(conn, 0, window, ATOM(_NET_WM_STATE),       XCB_ATOM_ATOM,       0, 1),
		xcb_get_property(conn, 0, window, XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW,     0, 1),
		xcb_get_property(conn, 0, window, ATOM(WM_CLIENT_LEADER),    XCB_ATOM_WINDOW,     0, 1),
		xcb_get_property(conn, 0, window, ATOM(WM_NORMAL_HINTS),     ATOM(WM_SIZE_HINTS), 0, 1),
		xcb_get_property(conn, 0, window, ATOM(_HEAWM_NAME),         XCB_ATOM_STRING,     0, NAME_LEN),
		/* cookies[2] = xcb_get_property(conn, 0, window, XCB_ATOM_WM_CLASS, XCB_ATOM_STRING, 0, 1); */
	};

	Box *box = new_box();
	box->window = window;

	xcb_get_property_reply_t *reply;

	xcb_atom_t state = XCB_ATOM_NONE;
	if ((reply = xcb_get_property_reply(conn, cookies[0], NULL))) {
		state = *(xcb_atom_t const *)xcb_get_property_value(reply);
		free(reply);
	}

	xcb_window_t transient_for = XCB_WINDOW_NONE;
	if ((reply = xcb_get_property_reply(conn, cookies[1], NULL))) {
		transient_for = *(xcb_window_t const *)xcb_get_property_value(reply);
		free(reply);
	}

	if ((reply = xcb_get_property_reply(conn, cookies[2], NULL))) {
		box->leader = *(xcb_window_t const *)xcb_get_property_value(reply);
		free(reply);
	}

	if ((reply = xcb_get_property_reply(conn, cookies[3], NULL))) {
		box_update_size_hints(box, xcb_get_property_value(reply));
		free(reply);
	}

	if ((reply = xcb_get_property_reply(conn, cookies[4], NULL))) {
		int const len = xcb_get_property_value_length(reply);
		memcpy(box->name, xcb_get_property_value(reply), len);
		free(reply);
	}

	Hand *hand = NULL;
	Box *parent = NULL;
	uint32_t pos;
	bool focus = false;

	if (0 < num_hands) {
		Box *origin = NULL;
		if (!origin && XCB_WINDOW_NONE != transient_for)
			origin = find_box_by_window(root, root, transient_for);
		if (!origin && XCB_WINDOW_NONE != box->leader)
			origin = find_box_by_leader(root, root, box->leader);

		/* find the hand that could possibly create this window */
		if (origin) {
			if (box_is_focused(origin)) {
				for (uint8_t i = 0; i < num_hands; ++i) {
					Hand *h = &hands[i];
					if (origin == h->input_focus) {
						if (h->want_focus) {
							hand = h;
							break;
						}
						if (!hand)
							hand = h;
					}
				}
				assert(hand && "box focused but no hands have it");
				focus = true;
			} else {
				hand = &hands[origin->focus_hand];
				focus = false;
			}
			pos = box_get_pos(origin) + 1;
			parent = origin->parent;
		} else {
			/* we use first hand even if its auto_focus is off */
			hand = &hands[0];
			for (uint8_t i = 0; i < num_hands; ++i) {
				if (hands[i].want_focus) {
					hand = &hands[i];
					break;
				}
			}
			focus = hand->want_focus;
			parent = hand->focus;

			if (parent) {
				if (!box_is_container(parent))
					parent = parent->parent;
				pos = 0;

				Box *child = hand->input_focus;
				if (child) {
					/* place after currently focused hand */
					while (child->parent != parent)
						child = child->parent;

					pos = box_get_pos(child) + 1;
				}
			}
		}
	}

	if (!parent) {
		assert(root->num_children && "no monitors");

		/* get primary monitor (will be the first) of screen with root_window */
		Body *body = get_body_by_root(root_window);
		Box **head = root->children;
		while ((body - bodies) != (*head)->body)
			++head;

		parent = *head;
		while (parent->num_children && box_is_container(parent->children[0]))
			parent = parent->children[0];

		/* place at the end */
		pos = parent->num_children;
	}

	/* we insert first */
	user_put_box(parent, pos, box);

	/* then we can name it */
	name_box(box, false);

	/* and just after we can focus, since focus needs to know whether box
	 * is container or not */
	if (hand) {
		hand->want_focus = false;
		if (focus && mode_default == hand->mode)
			hand_focus_box(hand, box);
	}

	ewmh_append_client_list(box);

	return box;
#if 0
	xcb_change_save_set(conn, XCB_SET_MODE_INSERT, window);
	cookie = xcb_reparent_window_checked(conn, window, box->window, 0, 0);
#endif
}

static void
setup_signals(void)
{
	struct sigaction sa;

	sigfillset(&sa.sa_mask);
	sa.sa_flags = SA_RESTART | SA_NOCLDSTOP | SA_NOCLDWAIT;

	/*MAN(SIGNALS)
	 * .TP
	 * .B SIGHUP
	 * Restart program, by replacing program image with itself.
	 */
	sa.sa_handler = handle_signal_restart;
	sigaction(SIGHUP, &sa, NULL);

	/*MAN(SIGNALS)
	 * .TP
	 * .B SIGINT, SIGTERM, SIGQUIT
	 * Terminate program gracefully.
	 */
	sa.sa_handler = handle_signal_quit;
	sigaction(SIGINT, &sa, NULL);
	sigaction(SIGTERM, &sa, NULL);
	sigaction(SIGQUIT, &sa, NULL);

	/* automatically clean up children */
	sa.sa_handler = SIG_DFL;
	sigaction(SIGCHLD, &sa, NULL);

	/* allow receiving every signal */
	sigemptyset(&sa.sa_mask);
	pthread_sigmask(SIG_SETMASK, &sa.sa_mask, NULL);
}

static void
update_hands(void);

static void
init_extensions(void)
{
	xcb_query_extension_reply_t const *ext;

	xcb_prefetch_extension_data(conn, &xcb_input_id);
	xcb_prefetch_extension_data(conn, &xcb_randr_id);
	xcb_prefetch_extension_data(conn, &xcb_shape_id);
	xcb_prefetch_extension_data(conn, &xcb_xkb_id);

	ext = xcb_get_extension_data(conn, &xcb_input_id);
	if (!ext->present)
		fprintf(stderr, "XInput extension missing."
				" Input will not work.\n");
	else
		xi_opcode = ext->major_opcode;

	ext = xcb_get_extension_data(conn, &xcb_randr_id);
	if (!ext->present)
		fprintf(stderr, "RandR extension missing."
				" Multi-head display will not work properly.\n");
	else
		randr_base_event = ext->first_event;

	ext = xcb_get_extension_data(conn, &xcb_shape_id);
	if (!ext->present)
		fprintf(stderr, "Shape extension missing. "
				" Labels will look crappy.\n");

	ext = xcb_get_extension_data(conn, &xcb_xkb_id);
	if (!ext->present) {
		fprintf(stderr, "XKB extension missing."
				" Keyboard input will not work.\n");
	} else {
		xkb_base_event = ext->first_event;

		/* initialize */
		xcb_xkb_use_extension_reply_t *const reply = xcb_xkb_use_extension_reply(conn,
				xcb_xkb_use_extension_unchecked(conn, XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION),
				NULL);
		if (!reply->supported) {
			fprintf(stderr, "Requested XKB version %d.%d not supported by server; got %d.%d."
					" Keyboard input may not work.\n",
					XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION,
					reply->serverMajor, reply->serverMinor);
		}

		free(reply);

		xkb_context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
	}
}

static void
connect_display(void)
{
	static char DEFAULT_DISPLAY[] = ":0";

	while (xcb_connection_has_error((conn = xcb_connect(NULL, &default_screen)))) {
		if (getenv("DISPLAY")) {
			fprintf(stderr, "Could not open display %s.\n",
					getenv("DISPLAY"));
		} else {
			fprintf(stderr, "DISPLAY is not set. Default to %s.\n",
					DEFAULT_DISPLAY);

			if (!setenv("DISPLAY", DEFAULT_DISPLAY, 0))
				continue;
		}

		exit(EXIT_FAILURE);
	}

	init_atoms();

	root = new_box();

	symbols = xcb_key_symbols_alloc(conn);

	init_extensions();

	if (!(xrm = xcb_xrm_database_from_default(conn)))
		fprintf(stderr, "Could not load X resources.\n");

	/* BROADCAST(connected, &(struct connected_args){ }); */
}

static bool
load_resource(char **const out, char const *const format, ...)
{
	if (!xrm)
		return false;

	va_list argp;
	char name[128];

	va_start(argp, format);
	vsnprintf(name, sizeof(name), format, argp);
	va_end(argp);

	return 0 == xcb_xrm_resource_get_string(xrm, name, NULL, out);
}

static void
setup_xrm(void)
{
	char *value;

	if (load_resource(&value, "heawm.label.fontFamily"))
		label_font = value;

	if (load_resource(&value, "heawm.label.fontSize"))
		label_size = strtol(value, NULL, 10);

	if (load_resource(&value, "heawm.label.strokeWidth"))
		label_stroke = strtol(value, NULL, 10);
}

static void
screen_query_windows(xcb_screen_t *screen)
{
	xcb_query_tree_reply_t *const reply =
		xcb_query_tree_reply(conn,
				xcb_query_tree_unchecked(conn, screen->root),
				NULL);
	if (!reply)
		return;

	xcb_window_t const *const children = xcb_query_tree_children(reply);
	int const num_children = xcb_query_tree_children_length(reply);

	xcb_get_window_attributes_cookie_t *const cookies =
		malloc(num_children * sizeof *cookies);

	for (int i = 0; i < num_children; ++i)
		cookies[i] = xcb_get_window_attributes_unchecked(conn, children[i]);

	for (int i = 0; i < num_children; ++i) {
		xcb_get_window_attributes_reply_t *const reply =
			xcb_get_window_attributes_reply(conn, cookies[i], NULL);
		if (!reply)
			continue;

		if (/* should we manage it? */
		    !reply->override_redirect &&
		    /* is mapped? */
		    reply->map_state == XCB_MAP_STATE_VIEWABLE)
			box_window(screen->root, children[i]);

		free(reply);
	}

	free(cookies);
	free(reply);
}

static void
body_setup_cursor(Body const *const body, char const *const cursor_name)
{
	xcb_cursor_context_t *ctx;
	xcb_screen_t *const screen = body->screen;

	if (xcb_cursor_context_new(conn, screen, &ctx) < 0)
		return;

	xcb_cursor_t const cursor = xcb_cursor_load_cursor(ctx, cursor_name);

	DEBUG_CHECK(xcb_change_window_attributes, conn, screen->root,
			XCB_CW_CURSOR, &cursor);

	xcb_free_cursor(conn, cursor);
	xcb_cursor_context_free(ctx);
}

static void
body_setup_wmname(Body const *const body, char const *const name)
{
	xcb_window_t const child = body->blackhole;

	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			child, ATOM(_NET_WM_NAME),
			ATOM(UTF8_STRING), 8,
			strlen(name), name);
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			child, ATOM(_NET_SUPPORTING_WM_DEBUG_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &child);

	/* link child window to root */
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->screen->root, ATOM(_NET_SUPPORTING_WM_DEBUG_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &child);
}

static void
body_setup_screen(Body *body)
{
	xcb_screen_t *const screen = body->screen;

	if (!CHECK(xcb_change_window_attributes, conn, screen->root,
				XCB_CW_EVENT_MASK,
				&(uint32_t const []){
					EVENT_MASK |
					XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
				}))
	{
		/* to be honest, this message should only be displayed for
		 * XCB_ACCESS */
		fprintf(stderr, "Maybe another window manager is running?\n");
		/* it is a fatal error because we have input devices that we do
		 * not know if that WM handles or not that will surely cause
		 * conflict */
		/* also: it can crashes X */
		exit(EXIT_FAILURE);
	}

	ewmh_delete_client_list(body);

	body->blackhole = xcb_generate_id(conn);
	DEBUG_CHECK(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			body->blackhole,
			screen->root,
			0, 0,
			1, 1,
			0,
			XCB_WINDOW_CLASS_INPUT_ONLY,
			screen->root_visual,
			0, NULL);

	body_setup_cursor(body, "default");
	body_setup_wmname(body, WM_NAME);
	screen_query_windows(screen);

	DEBUG_CHECK(xcb_input_xi_select_events, conn, screen->root, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
						XCB_INPUT_XI_EVENT_MASK_HIERARCHY |
						XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS |
						XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
						XCB_INPUT_XI_EVENT_MASK_ENTER));
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

static Box *
find_head_by_name(uint8_t const body, char const *const name)
{
	if (!name)
		return NULL;

	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (body != head->body)
			continue;

		if (!head->title)
			continue;

		if (0 != strcmp(head->title, name))
			continue;

		return head;
	}

	return NULL;
}

static bool
box_has_name(Box const *const box)
{
	return !!box->name[0];
}

static void
kill_head(Box *const head)
{
	head->user_width = 0;
}

static bool
head_is_alive(Box const *const head)
{
	return 0 < head->user_width;
}

static void
body_set_env(void *arg)
{
	Body const *const body = arg;
	char *host;
	int display;
	int screen;

	if (!xcb_parse_display(NULL, &host, &display, &screen))
		return;

	char display_screen[1 + 20 + 1 + 20 + 1];
	size_t const host_size = strlen(host);

	char *p;
	if (!(p = realloc(host, host_size + sizeof display_screen)))
		return;
	host = p;

	sprintf(p + host_size, ":%d.%d", display, body->screen_index) + 1;

	setenv("DISPLAY", host, true);
	free(host);
}

/* FIXME: also run when XRandR unavailable and root size changed */
static void
body_update_heads(Body *const body)
{
	/*MAN(HOOKS)
	 * .TP
	 * .B displaychange
	 * Run whenever display configuration changes, e.g. monitor
	 * connected/disconnected, resolution changed. See xrandr(1).
	 */
	spawn((char const *[]){ heawm_file("displaychange"), NULL }, body_set_env, body);

	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if ((body - bodies) != head->body)
			continue;

		kill_head(head);
	}

	GET_REPLY(monitors, xcb_randr_get_monitors, conn, body->screen->root, true);
	if (!monitors) {
		fprintf(stderr, "Failed to query RandR monitors.\n");

		Box *const head = new_box();
		head->window = body->screen->root;
		head->title = "";
		put_box(root, 0, head);

		name_box(head, true);

		head->body = body - bodies;
		head->user_width = body->screen->width_in_millimeters;
		head->user_height = body->screen->height_in_millimeters;
		box_set_size(head, body->screen->width_in_pixels, body->screen->height_in_pixels);
		box_set_position(head, 0, 0);
		goto vacuum;
	}

	int const num_monitors = xcb_randr_get_monitors_monitors_length(monitors);
	assert(0 < num_monitors);

	xcb_get_atom_name_cookie_t *cookies = malloc(num_monitors * sizeof *cookies);

	for (xcb_randr_monitor_info_iterator_t iter = xcb_randr_get_monitors_monitors_iterator(monitors);
	     0 < iter.rem;
	     xcb_randr_monitor_info_next(&iter))
	{
		xcb_randr_monitor_info_t const *const monitor = iter.data;
		cookies[iter.rem - 1] = xcb_get_atom_name_unchecked(conn, monitor->name);
	}

	for (xcb_randr_monitor_info_iterator_t iter = xcb_randr_get_monitors_monitors_iterator(monitors);
	     0 < iter.rem;
	     xcb_randr_monitor_info_next(&iter))
	{
		xcb_randr_monitor_info_t const *const monitor = iter.data;

		xcb_get_atom_name_reply_t *const name_reply =
			xcb_get_atom_name_reply(conn, cookies[iter.rem - 1], NULL);
		char *name = NULL;
		if (name_reply) {
			int const len = xcb_get_atom_name_name_length(name_reply);
			name = malloc(len + 1);
			memcpy(name, xcb_get_atom_name_name(name_reply), len);
			name[len] = '\0';
		}

		free(name_reply);

		Box *head = find_head_by_name(NULL_BODY, name);
		if (!head) {
			head = new_box();
			head->window = body->screen->root;
			head->title = name, name = NULL; /* move */
			put_box(root, monitor->primary ? 0 : root->num_children, head);

			/* if it is a primary monitor name it immediately;
			 * non-primary monitors will be named in a second pass */
			if (monitor->primary)
				name_box(head, true);
		}

		head->body = body - bodies;
		head->user_width = monitor->width_in_millimeters;
		head->user_height = monitor->height_in_millimeters;
		box_set_size(head, monitor->width, monitor->height);
		box_set_position(head, monitor->x, monitor->y);

		free(name);
	}

	free(cookies);
	free(monitors);

vacuum:
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];

		if (!box_has_name(head)) {
			assert(head->body == (body - bodies));
			name_box(head, true);
		}

		if (head_is_alive(head))
			continue;

		/* make it floating */
		/* TODO */
		head->x = 0;
		head->y = 0;
		head->user_width = 300;
		head->user_height = 400;
	}

	root->content_changed = true;

	assert(root->num_children);
}

static void
setup_display(void)
{
	/* prevent windows from changing */
	DEBUG_CHECK(xcb_grab_server, conn);

	xcb_setup_t const *const setup = xcb_get_setup(conn);

	num_bodies = xcb_setup_roots_length(setup);
	bodies = calloc(num_bodies, sizeof *bodies);

	int i = 0;
	for (xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);
	     0 < iter.rem;
	     ++i, xcb_screen_next(&iter))
	{
		xcb_screen_t *const screen = iter.data;
		Body *body = &bodies[default_screen == i ? 0 : i + 1];

		body->screen_index = i;
		body->screen = screen;
		body->visual_type = lookup_visual_type(screen);

		DEBUG_CHECK(xcb_randr_select_input, conn, screen->root,
				XCB_RANDR_NOTIFY_MASK_SCREEN_CHANGE);

		/* we can manage windows only if we have some heads */
		body_update_heads(body);
		body_setup_screen(body);
	}

	DEBUG_CHECK(xcb_ungrab_server, conn);

	update_hands();
}

static void
handle_error(xcb_generic_error_t const *const event)
{
	print_error(event, "(event loop)");
}

static void
handle_property_notify(xcb_property_notify_event_t const *const event)
{
	xcb_get_property_reply_t *reply;

#define REQUEST_PROPERTY(object, type, length) do { \
	if (!(reply = xcb_get_property_reply(conn, xcb_get_property(conn, 0, (object)->window, event->atom, (type), 0, (length)), NULL))) \
		return; \
} while (0)

#define FIND_BOX \
	Box *const box = find_box_by_window(root, root, event->window); \
	if (!box) \
		goto out;

	if (ATOM(WM_CLIENT_LEADER) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_WINDOW, sizeof(xcb_window_t) / sizeof(uint32_t));

		box->leader = *(xcb_window_t *)xcb_get_property_value(reply);
	} else if (ATOM(_NET_WM_NAME) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_STRING, 128 / sizeof(uint32_t));

		int const len = xcb_get_property_value_length(reply);
		box->title = realloc(box->title, (len + 1) * sizeof(char));
		memcpy(box->title, xcb_get_property_value(reply), len);
		box->title[len] = '\0';
	} else if (ATOM(WM_NORMAL_HINTS) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, ATOM(WM_SIZE_HINTS), sizeof(xcb_size_hints_t) / sizeof(uint32_t));

		box_update_size_hints(box, xcb_get_property_value(reply));
	} else if (ATOM(_HEAWM_NAME) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_STRING, NAME_LEN);

		int const len = xcb_get_property_value_length(reply);
		if (NAME_LEN < len) {
			fprintf(stderr, "0x%x._HEAWM_NAME = \"%.*s\" is too long."
					" Maximum allowed size is %u.\n",
					box->window,
					len, (char *)xcb_get_property_value(reply),
					NAME_LEN);
			goto out;
		}

		memcpy(box->name, xcb_get_property_value(reply), len);
		if (len < NAME_LEN)
			box->name[len] = '\0';
	} else {
		return;
	}

out:
	free(reply);

#undef FIND_BOX
#undef REQUEST_PROPERTY
}

static void
hand_grab_pointer(Hand const *const hand)
{
	for_each_body {
		xcb_window_t const root_window = body->screen->root;

		xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root_window,
				XCB_CURSOR_NONE,
				/*button */1,
				hand->master_pointer, 1, 1,
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
}

static void
hand_grab_keyboard(Hand const *const hand)
{
	for_each_body {
		xcb_window_t const root_window = body->screen->root;

		if (mode_default == hand->mode) {
			/* or just grab enter */
			/* NOTE: because we previously grabbed every key we can
			 * undo it if we ungrab all keys. however it also
			 * contains our WM specific Super+... keys too, that we
			 * do not want to remove, so we need to add them back */
			xcb_input_xi_passive_ungrab_device(conn, root_window, XCB_GRAB_ANY, hand->master_keyboard,
					1, XCB_INPUT_GRAB_TYPE_KEYCODE, &(uint32_t const){
						XCB_INPUT_MODIFIER_MASK_ANY
					});

#if 0
			xcb_keycode_t *const keycodes = xcb_key_symbols_get_keycode(symbols, XKB_KEY_Return);

			for (xcb_keycode_t const *keycode = keycodes;
			     0 && XCB_NO_SYMBOL != *keycode;
			     ++keycode)
			{
				xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root_window,
						XCB_CURSOR_NONE, *keycode,
						hand->master_keyboard, 1, 1,
						XCB_INPUT_GRAB_TYPE_KEYCODE,
						XCB_INPUT_GRAB_MODE_22_SYNC,
						XCB_INPUT_GRAB_MODE_22_ASYNC,
						false,
						&(uint32_t const){
							XCB_INPUT_XI_EVENT_MASK_KEY_RELEASE
						},
						&(uint32_t const){
							XCB_INPUT_MODIFIER_MASK_ANY
						});
			}

			free(keycodes);
#endif

			uint32_t const MASKS[] = {
				0,
				XCB_MOD_MASK_SHIFT,
				XCB_MOD_MASK_CONTROL,
				XCB_MOD_MASK_1,
			};

			for (uint8_t i = 0; i < ARRAY_SIZE(MASKS); ++i)
				xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root_window,
						XCB_CURSOR_NONE, XCB_GRAB_ANY,
						hand->master_keyboard, 1, 1,
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
		} else {
			xcb_input_xi_passive_grab_device(conn, XCB_CURRENT_TIME, root_window,
					XCB_CURSOR_NONE, XCB_GRAB_ANY,
					hand->master_keyboard, 1, 1,
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
	}
}

static Hand *
get_hand_by_master_keyboard(xcb_input_device_id_t const master_keyboard)
{
	/* FIXME: if we can receive master devices, remove this check */
	for_each_hand {
		if (master_keyboard == hand->master_keyboard)
			return hand;
	}

	/* FIXME: ... and this */
	return NULL;
}

static Device *
get_device_by_id(xcb_input_device_id_t const deviceid)
{
	for (uint8_t i = 0; i < num_devices; ++i) {
		Device const *const device = &devices[i];
		if (deviceid == device->id)
			return (Device *)device;
	}

	return NULL;
}

static void
handle_input_focus_in(xcb_input_focus_in_event_t const *const event)
{
	if (XCB_INPUT_NOTIFY_MODE_NORMAL != event->mode)
		return;

	Hand *const hand = get_hand_by_master_keyboard(event->deviceid);
	if (event->child != hand_get_wanted_focus(hand))
		hand_refocus(hand);
}

static Box *
find_box_in_body_by_window(Body *const body, xcb_window_t const window)
{
	uint8_t const body_pos = body - bodies;

	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (body_pos != head->body)
			continue;

		Box *box = find_box_by_window(head, head, window);
		if (box)
			return box;
	}

	return NULL;
}

static void
handle_destroy_notify(xcb_destroy_notify_event_t const *const event)
{
	Box *box = find_box_by_window(root, root, event->window);
	/* printf("destroy notify %x event=%x\n", event->window, event->event); */
	if (box)
		delete_box(box);
}

static void
handle_unmap_notify(xcb_unmap_notify_event_t const *const event)
{
	/* printf("unmap notify %x event=%x, from_configure=%d\n", event->window, event->event, event->from_configure); */
}

static void
handle_map_request(xcb_map_request_event_t const *const event)
{
	box_window(event->parent, event->window);
}

static void
handle_configure_request(xcb_configure_request_event_t const *const event)
{
	Box *box;
	if ((box = find_box_by_window(root, root, event->window))) {
		/* GPLv3 Annex 1: DO NOT FUCKING TOUCH IT. PLEASE. */
		DEBUG_CHECK(xcb_send_event, conn, false, box->window,
				XCB_EVENT_MASK_STRUCTURE_NOTIFY,
				XCB_SEND_EVENT_EVENT(xcb_configure_notify_event_t,
					.response_type = XCB_CONFIGURE_NOTIFY,
					.event = box->window,
					.window = box->window,
					.above_sibling = XCB_WINDOW_NONE,

					.x = box->x,
					.y = box->y,
					.width = box->width,
					.height = box->height,

					.border_width = 0,
					/* surely not if request reached us */
					.override_redirect = false,
				));
	}
	/* 	DEBUG_CHECK(xcb_configure_window, conn, event->window,
				XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y |
				XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
				&(const uint32_t[]){
					40, 0,
					300, 400
				});
		xcb_flush(conn);
	} */
}

static void
handle_expose(xcb_expose_event_t const *const event)
{
	/* ignore events that will surely be followed by another expose event.
	 * we only repaint labels at the last one in the sequence. */
	if (0 < event->count)
		return;

	for_each_body {
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			Label const *const label = &body->labels[j];
			if (label->window == event->window) {
				label_repaint(label, false);
				return;
			}
		}
	}
}

static void
close_box(Box *const box)
{
	assert(!box_is_container(box));

	if (box->close_by_force) {
		DEBUG_CHECK(xcb_kill_client, conn, box->window);
		return;
	}

	box->close_by_force = true;

	DEBUG_CHECK(xcb_send_event, conn, false, box->window,
			XCB_EVENT_MASK_NO_EVENT/* client messages cannot be masked */,
			(char const *)&(xcb_client_message_event_t){
				.response_type = XCB_CLIENT_MESSAGE,
				.format = 32,
				.window = box->window,
				.type = ATOM(WM_PROTOCOLS),
				.data = {
					.data32 = {
						ATOM(WM_DELETE_WINDOW),
					}
				}
			});
}

static void
handle_client_message(xcb_client_message_event_t const *const event)
{
	if (ATOM(_NET_CLOSE_WINDOW) == event->type) {
		Box *const box = find_box_by_window(root, root, event->window);
		if (box)
			close_box(box);
	} else if (ATOM(_NET_ACTIVE_WINDOW) == event->type) {
		if (2 != event->data.data32[0])
			return;

		Box *const box = find_box_by_window(root, root, event->window);
		if (box) {
			Hand *hand;
			if (NULL_HAND == box->focus_hand) {
				if (1 == num_hands)
					hand = &hands[0];
				else
					return;
			} else {
				hand = &hands[box->focus_hand];
			}
			hand_focus_box(hand, box);
		}
	}
}

static Hand *
find_hand_by_master_pointer(xcb_input_device_id_t const pointer);

static void
update_hands(void)
{
	GET_REPLY(reply, xcb_input_xi_query_device, conn,
			XCB_INPUT_DEVICE_ALL);
	if (!reply)
		return;

	/*MAN(HOOKS)
	 * .TP
	 * .B inputchange
	 * Run whenever input devices change, e.g. keyboard plugged/unplugged,
	 * master device added. See xinput(1).
	 */
	SPAWN(heawm_file("inputchange"));

	/* map old hand indexes to new hand indexes for correct stack history */
	/* according to specification devices above 127 are invisible to clients */
	uint8_t hand_map[NULL_HAND + 1];

	/* reassign boxes of deattached hands to an invalid hand */
	memset(hand_map, NULL_HAND, num_hands);
	hand_map[NULL_HAND] = NULL_HAND;

	uint8_t new_num_hands = 0;
	for (xcb_input_xi_device_info_iterator_t iter = xcb_input_xi_query_device_infos_iterator(reply);
	     0 < iter.rem;
	     xcb_input_xi_device_info_next(&iter))
	{
		xcb_input_xi_device_info_t const *const input_device = iter.data;
		new_num_hands += XCB_INPUT_DEVICE_TYPE_MASTER_POINTER == input_device->type;
	}

	Hand *const new_hands = calloc(new_num_hands, sizeof *new_hands);
	Hand *hand = new_hands;

	for (uint8_t i = 0; i < num_devices; ++i) {
		Device *const device = &devices[i];
		xkb_keymap_unref(device->keymap);
	}

	num_devices = xcb_input_xi_query_device_infos_length(reply);
	devices = realloc(devices, num_devices * sizeof(*devices));
	Device *device = devices;

	for (xcb_input_xi_device_info_iterator_t iter = xcb_input_xi_query_device_infos_iterator(reply);
	     0 < iter.rem;
	     xcb_input_xi_device_info_next(&iter))
	{
		xcb_input_xi_device_info_t const *const input_device = iter.data;

		/* master devices are cursors on the screen. we only have to deal with them */
		/* any slave device can control master */
		if (XCB_INPUT_DEVICE_TYPE_MASTER_POINTER != input_device->type)
			goto add_device;

		int const len = xcb_input_xi_device_info_name_length(input_device) - strlen(" pointer");
		char *name = malloc(len + 1/* NULL */);
		memcpy(name, xcb_input_xi_device_info_name(input_device), len);
		name[len] = '\0';

		Hand *const old_hand = find_hand_by_master_pointer(input_device->deviceid);

		/*MAN(RESOURCES)
		 * .SS "Hand Resources"
		 * The following resources can be used to customize
		 * hands. They are specified by the following patterns
		 * and checked in order:
		 * .
		 * .IP \(bu
		 * \fB\(lqheawm.hand.\fINAME\fB.*\(rq\fR: Match hand by
		 * its \fINAME\fR, e.g. \(lqVirtual core\(rq. \fINAME\fR
		 * is coming from
		 * .B XInput
		 * \(lq\fINAME\fR pointer\(rq and
		 * \(lq\fINAME\fR keyboard\(rq master device pair.
		 * .
		 * .IP \(bu
		 * \fB\(lqheawm.hand.\fIINDEX\fB.*\(rq\fR: Match hand by its 1-based \fIINDEX\fR.
		 * .
		 * .IP \(bu
		 * \fB\(lqheawm.hand.*\(rq\fR: Match all hands. Can be useful for specifying default settings.
		 * .
		 * .TP
		 * .B color
		 * Specifies focus color in
		 * .B \(lq0xRRGGBB\(rq
		 * or
		 * .B \(lq#RRGGBB\(rq
		 * format.
		 * .RE
		 * .
		 * .PP
		 * Example:
		 * .sp
		 * .EX
		 * ! Default input pair.
		 * heawm.hand.Virtual core.color: #fe0202
		 * ! Master devices \(lqmom pointer\(rq and \(lqmom keyboard\(rq.
		 * heawm.hand.mom.color: #ff00ff
		 * ! 17th device pair.
		 * heawm.hand.17.color: 0xffaf5f
		 * ! Default color.
		 * heawm.hand.color: 0xffff00
		 * .EE
		 */
		hand->color = 0xfe0202, 0xffaf5f;
		for (char *value;
		     load_resource(&value, "heawm.hand.%s.color", name) ||
		     load_resource(&value, "heawm.hand.%u.color", new_num_hands) ||
		     load_resource(&value, "heawm.hand.color", new_num_hands);)
		{
			if (1 != sscanf(value, "0x%6x", &hand->color) &&
			    1 != sscanf(value, "#%6x", &hand->color))
				fprintf(stderr, "Invalid color resource value: %s.\n",
						value);
			free(value);
			break;
		}

		if (old_hand) {
			memcpy(hand, old_hand, sizeof *hand);
			/* grabbers are already setup and consistent; focus do
			 * not have to be touched */
			hand_map[old_hand - hands] = hand - new_hands;
			/* NOTE: grabbing have been already set up */
			goto add_device;
		}

		hand->master_pointer = input_device->deviceid;
		hand->master_keyboard = input_device->attachment;
		hand->want_focus = true;
		hand->focus = NULL;
		hand->input_focus = NULL;
		hand->label_boxes = true;

		assert(0 < num_bodies);

		hand_grab_pointer(hand);
		hand_grab_keyboard(hand);

		uint32_t const MASK =
			/* so we can check for repeating */
			XCB_XKB_PER_CLIENT_FLAG_DETECTABLE_AUTO_REPEAT |
			/* send good state in events */
			XCB_XKB_PER_CLIENT_FLAG_GRABS_USE_XKB_STATE |
			XCB_XKB_PER_CLIENT_FLAG_LOOKUP_STATE_WHEN_GRABBED;

		xcb_xkb_per_client_flags(conn,
				hand->master_keyboard,
				MASK,
				MASK,
				0, 0, 0);

		/* xkbcommon-x11 */
		uint16_t const REQUIRED_EVENTS =
			XCB_XKB_EVENT_TYPE_MAP_NOTIFY |
			XCB_XKB_EVENT_TYPE_NEW_KEYBOARD_NOTIFY;
		uint16_t const REQUIRED_MAP_PARTS =
			XCB_XKB_MAP_PART_KEY_TYPES |
			XCB_XKB_MAP_PART_KEY_SYMS |
			XCB_XKB_MAP_PART_MODIFIER_MAP |
			XCB_XKB_MAP_PART_EXPLICIT_COMPONENTS |
			XCB_XKB_MAP_PART_KEY_ACTIONS |
			XCB_XKB_MAP_PART_VIRTUAL_MODS |
			XCB_XKB_MAP_PART_VIRTUAL_MOD_MAP;

		DEBUG_CHECK(xcb_xkb_select_events, conn, hand->master_keyboard,
				REQUIRED_EVENTS,
				0,
				REQUIRED_EVENTS,
				REQUIRED_MAP_PARTS,
				REQUIRED_MAP_PARTS,
				NULL);

		/* it is safe to call because we know that hand currently
		 * has no focus */
		hand_refocus(hand);

		free(name);

		++hand;

	add_device:
		device->id = input_device->deviceid;

		device->hand = new_hands;
		while (device->hand->master_pointer != input_device->attachment &&
		       device->hand->master_keyboard != input_device->attachment)
			++device->hand;

		device->keymap = NULL;

		for_each_body {
			DEBUG_CHECK(xcb_input_xi_select_events, conn, body->screen->root, 1,
					XI_EVENT_MASK(device->id,
								XCB_INPUT_XI_EVENT_MASK_KEY_PRESS |
								XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
								XCB_INPUT_XI_EVENT_MASK_ENTER));
		}

		++device;
	}
	assert(device - devices == num_devices);

	free(reply);

	/* update focus history */
	Box *box;
	for_each_box(box, root, root, {
		box->focus_hand = hand_map[box->focus_hand];

		if (!box_is_container(box)) {
			{
				Box *const new = realloc(box, sizeof(Box) + new_num_hands * sizeof(BoxPointer));
				box = move_box(new, box);
			}

			BoxPointer old_pointers[MAX_NUM_HANDS];
			/* save pointers of box becase we may shuffle them */
			memcpy(old_pointers, Box_pointers(box), num_hands * sizeof(BoxPointer));
			/* reset all */
			memset(Box_pointers(box), 0, new_num_hands * sizeof(BoxPointer));

			for (uint8_t i = 0; i < num_hands; ++i)
				if (NULL_HAND != hand_map[i])
					Box_pointers(box)[hand_map[i]] = old_pointers[i];
		}
	});

	/* forget deattached hands' focus by incrementing focus number of
	 * hands alive */
	increase_focus_seq();

	free(hands), hands = new_hands;
	num_hands = new_num_hands;

	focus_all_hands(root->focus_seq);
}

static void
handle_input_hierarchy_change(xcb_input_hierarchy_event_t const *const event)
{
	if (event->flags & (XCB_INPUT_HIERARCHY_MASK_MASTER_ADDED |
	                    XCB_INPUT_HIERARCHY_MASK_MASTER_REMOVED |
	                    XCB_INPUT_HIERARCHY_MASK_SLAVE_ADDED |
	                    XCB_INPUT_HIERARCHY_MASK_SLAVE_REMOVED |
	                    XCB_INPUT_HIERARCHY_MASK_SLAVE_ATTACHED |
	                    XCB_INPUT_HIERARCHY_MASK_SLAVE_DETACHED))
		update_hands();
}

static Hand *
find_hand_by_master_pointer(xcb_input_device_id_t const master_pointer)
{
	for_each_hand {
		if (master_pointer == hand->master_pointer)
			return hand;
	}

	return NULL;
}

/* TODO: accept numbers and minus prefix */
static bool
hand_input_find_label(Hand *hand, Label **out)
{
	uint8_t n = strnlen(hand->user_input, NAME_LEN);
	if (n < NAME_LEN)
		++n;

	for_each_body {
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			Label *const label = &body->labels[j];
			if (!memcmp(label->name, hand->user_input, n))
				continue;
			*out = label;
			return true;
		}
	}

	memset(hand->user_input, 0, sizeof hand->user_input);
	return false;
}

#if 0
static void
hand_set_timeout(Hand *const hand, int const timeout_ms)
{
	assert(0 != timeout_ms && "timeout already elapsed you dumbass");
	printf("setto=%d\n", timeout_ms);
	if (next_timeout_ms < timeout_ms) {
		next_timeout_ms = hand->timeout_ms = timeout_ms;
	} else if (next_timeout_ms <= hand->timeout_ms) {
		next_timeout_ms = hand->timeout_ms = timeout_ms;
		for_each_hand {
			if (next_timeout_ms < hand->timeout_ms)
				next_timeout_ms = hand->timeout_ms;
		}
	} else {
		hand->timeout_ms = timeout_ms;
	}

}
#endif

/** modifiers we are interested in */
#define KEY_MOD_MASK ( \
	XCB_MOD_MASK_CONTROL | \
	XCB_MOD_MASK_1       | \
	XCB_MOD_MASK_2       | \
	XCB_MOD_MASK_3       | \
	XCB_MOD_MASK_4       | \
	XCB_MOD_MASK_5       )

#if 0
static void
handle_input_key_release(xcb_input_key_press_event_t const *const event)
{
	xcb_keysym_t const sym = xcb_key_symbols_get_keysym(symbols, event->detail, event->mods.effective & XCB_MOD_MASK_SHIFT);

	printf("kep\n");
	Hand *const hand = get_hand_by_master_keyboard(event->deviceid);
#ifndef HEAWM_NDEBUG
	/* FIXME: just under Xephyr but root key press reports slave device */
	if (!hand)
		goto out;
#endif

	if (XKB_KEY_Super_L == sym || XKB_KEY_Super_R == sym) {
		hand_set_timeout(hand, -1);
		if (mode_boxes == hand->mode)
			hand->mode = mode_default;
	}

out:
	xcb_input_xi_allow_events(conn, XCB_CURRENT_TIME, event->deviceid,
			XCB_INPUT_EVENT_MODE_REPLAY_DEVICE,
			0, 0);

}
#endif

static void
hand_handle_input_key_normal(Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
	if (repeating)
		return;

	hand->want_focus |= XKB_KEY_Return == sym || XKB_KEY_KP_Enter == sym;

	if (hand->input_focus) {
		hand->input_focus->close_by_force = false;

		if (hand->focus != hand->input_focus) {
			box_propagate_change(hand->focus)->label_changed = true;
			hand->focus = hand->input_focus;
		}

		if (hand->latest_input[0] != hand->input_focus) {
			hand->latest_input[1] = hand->latest_input[0];
			hand->latest_input[0] = hand->input_focus;
		}
	}
}

static void
hand_input_reset(Hand *const hand)
{
	memset(hand->user_input, '\0', sizeof hand->user_input);
}

static void
hand_input_try_jump(Hand *const hand)
{
	Box *box;
	if (!find_box_by_name(&box, hand->user_input))
		return;

	assert(box != root);

	if (!box) {
		char name[NAME_LEN + 1];
		strncpy(name, hand->user_input, NAME_LEN);
		name[NAME_LEN] = '\0';

		/*MAN(HOOKS)
		 * .TP
		 * .B inputchange
		 * Run whenever user has would like to jump to a non-existing
		 * label. Can be useful to automagically start programs.
		 */
		if (SPAWN(heawm_file("autostart"), name))
			hand->want_focus = true;
		goto reset_input;
	}

	if (box == hand->input_focus)
		box = hand->latest_input[box == hand->latest_input[0]];

	if (!box)
		goto reset_input;

	hand_focus_box(hand, box);
reset_input:
	hand_input_reset(hand);
}

static bool
hand_handle_input(Hand *const hand, xcb_keysym_t const sym)
{
#define INPUT_BETWEEN(lower_char, lower, upper) \
	((lower) <= sym && sym <= (upper)) \
		hand->user_input[input_len] = (lower_char) + (sym - (lower))

	uint8_t const input_len = strnlen(hand->user_input, NAME_LEN);
	if INPUT_BETWEEN('a', XKB_KEY_a, XKB_KEY_z);
	else if INPUT_BETWEEN('A', XKB_KEY_A, XKB_KEY_Z);
	else if INPUT_BETWEEN('0', XKB_KEY_0, XKB_KEY_9);
	else if INPUT_BETWEEN('0', XKB_KEY_KP_0, XKB_KEY_KP_9);
	else return false;

	return true;

#undef INPUT_BETWEEN
}

static bool
hand_handle_input_key_super(Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
	/*MAN(Keybindings)
	 * .TP
	 * .B Mod+{a-zA-Z}...
	 * Focus window by name. If there is no such window,
	 * keybinding is treated like
	 * .B Ctrl
	 * was pressed.
	 */
	if (hand_handle_input(hand, sym)) {
		hand_input_try_jump(hand);
	} else switch (sym) {
	case XKB_KEY_parenright:
	{
		if (repeating)
			break;

		hand->want_focus = true;
		SPAWN(config.terminal, "-e", heawm_file("quickstart"));
	}
		break;

	case XKB_KEY_bracketright:
		if (repeating)
			break;

		hand->want_focus = true;
		SPAWN(config.terminal, "-e", "alsamixer");
		break;

	case XKB_KEY_asterisk:
		if (repeating)
			break;

		SPAWN("amixer", "-q", "set", "Master", "toggle");
		break;

	case XKB_KEY_minus:
		SPAWN("amixer", "-q", "set", "Master", "1-");
		break;

	case XKB_KEY_plus:
		SPAWN("amixer", "-q", "set", "Master", "1+", "unmute");
		break;

	case XKB_KEY_Return:
		if (repeating)
			break;

		/*MAN(Keybindings)
		 * .TP
		 * .B Mod+Return
		 * Open $\fBTERMINAL\fR.
		 */
		hand->want_focus = true;
		SPAWN(config.terminal);
		break;

	default:
		return true;
	}

	return false;
}

static void
box_set_num_columns(Box *const box, uint16_t const num_columns)
{
	assert(box_is_container(box));
	if (num_columns == box->num_columns)
		return;

	box->num_columns = num_columns;
	box_propagate_change(box)->layout_changed = true;
}

static void
hand_handle_input_key_mode(Hand *const hand, xcb_keysym_t const sym)
{
	Label *label;

	switch (hand->mode) {
	case mode_default:
		unreachable;
		return;

	case mode_move_a:
	case mode_move_b:
		/* assert(!"TODO"); */
		return;

	case mode_size_side:
	{
		if (hand_input_find_label(hand, &label)) {
			hand->mode = mode_size_to;
			hand->mode_box = label->base;
			return;
		}
		break;
	}
		return;

	case mode_size_to:
	{
		if (hand_input_find_label(hand, &label)) {
			return;
		}
	}
		break;

	case mode_name:
	{
		if (XKB_KEY_Return == sym ||
		    NAME_LEN == strnlen(hand->user_input, NAME_LEN))
		{
			bool const iscontainer = box_is_container(hand->mode_box);
			memcpy(hand->mode_box->name, hand->user_input, NAME_LEN);
			char *const c = &hand->mode_box->name[0];
			if (iscontainer)
				*c &= ~0x20;
			else
				*c |= 0x20;
			name_box(hand->mode_box, iscontainer);
			assert(box_is_container(hand->mode_box) == iscontainer);
			break;
		} else if (XKB_KEY_space && !*hand->user_input) {
			hand->mode_box->hide_label ^= 1;
			box_propagate_change(hand->mode_box)->label_changed = true;
			break;
		}
		box_propagate_change(hand->mode_box)->label_changed = true;
	}
		return;

	case mode_setcolumns:
	{
		Box *parent = hand->mode_box->parent;
		uint16_t num_columns;
		if (hand_input_find_label(hand, &label)) {
			if (!label)
				return;

			for (num_columns = 0;
			     parent->children[num_columns++] != label->base;);

			if (num_columns == parent->num_children)
				num_columns = UINT16_MAX;
		} else switch (sym) {
		case XKB_KEY_equal:
			num_columns = 0;
			break;

		case XKB_KEY_slash:
			num_columns = 1;
			break;

		case XKB_KEY_asterisk:
			num_columns = UINT16_MAX;
			break;

		case XKB_KEY_plus:
			num_columns = box_compute_num_columns(parent, box_get_num_tiles(parent)) + 1;
			break;

		case XKB_KEY_minus:
			num_columns = box_compute_num_columns(parent, box_get_num_tiles(parent));
			if (1 < num_columns)
				--num_columns;
			break;

		default:
			num_columns = parent->num_columns;
			break;
		}

		box_set_num_columns(parent, num_columns);
	}
		break;

	}

	hand_leave_mode(hand);
}

/* command for wm */
static bool
hand_handle_input_key_command(Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
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
		if (!hand->input_focus)
			break;

		hand->mode = mode_setcolumns;
		hand->mode_box = hand->input_focus;
		hand_input_reset(hand);
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
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B [focus] Mod+Ctrl+m {point-a} {point-b}
	 * Move focused window to the place of line from {point-a} to {point-b}.
	 */
	case XKB_KEY_m:
		hand->mode = mode_move_a;
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B [focus] Mod+Ctrl+l
	 * Lock focus position. On a focused window unlock.
	 */
	case XKB_KEY_l:
	{
		if (repeating)
			break;

		Box *box = hand->input_focus;
		if (!box)
			break;

		bool const set = !box->parent->focus_lock;
		if (box == hand->focus)
			/* focus lock makes sense only if we have more
			 * children, so move upwards till we set the
			 * focus lock on a container that has more than
			 * one children */
			do
				box_set_focus_lock(box, set);
			while (box->num_children <= 1 && (box = box->parent));
		else
			/* set focus lock from input_focus up to focus */
			do
				box_set_focus_lock((box = box->parent), set);
			while (hand->focus != box);

		/* we change properties only at a subtree so it is enough if we
		 * propagate changes upwards from the very bottom */
		box_propagate_change(box);
	}
		break;

	/* ~fullscreen~ fuck, i can't see anything: set concealed for
	 * all upper windows up to float/fixed sized or focus
	 *
	 * if nothing to do: unfullscreen
	 */
	/*MAN(Keybindings)
	 * .TP
	 * .B [focus] Mod+Ctrl+f
	 * Conceal all windows up to and including focus. Default to root window.
	 */
	case XKB_KEY_f:
	{
		if (repeating)
			break;

		if (!hand->focus)
			break;

		bool const conceal = !hand->focus->concealed;
		Box *box = hand->focus->parent;
		box_propagate_change(box)->layout_changed = true;
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box *child = box->children[i];
			child->concealed = conceal;
		}
	}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod+Ctrl+n {a-zA-Z}... Return
	 * Name window.
	 */
	case XKB_KEY_n:
		if (repeating)
			break;

		if (!hand->input_focus)
			break;

		hand->mode = mode_name;
		hand->mode_box = hand->focus;
		hand_input_reset(hand);
		break;

	/* wlose/xlose */
	/*MAN(Keybindings)
	 * .TP
	 * .B [focus] Mod+Ctrl+w
	 * Close window(s) in focus just like user would clicked
	 * \*(lqX\*(rq in the title bar. Second press kills by force.
	 */
	case XKB_KEY_w:
	{
		if (repeating)
			break;

		if (!hand->focus)
			break;

		Box *box;
		for_each_box(box, hand->focus, hand->focus, {
			if (!box_is_container(box))
				close_box(box);
		})
	}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B [focus] Mod+Ctrl+z
	 * Center pointer.
	 */
	case XKB_KEY_z:
	{
		Box *box = hand->focus;
		if (!box)
			break;

		DEBUG_CHECK(xcb_input_xi_warp_pointer, conn,
				XCB_WINDOW_NONE,
				bodies[box->body].screen->root,
				0, 0, 0, 0,
				(box->x + box->width  / 2) << 16,
				(box->y + box->height / 2) << 16,
				hand->master_pointer);
	}
		break;

	default:
		return true;
	}

	if (mode_default != hand->mode)
		hand_do_mode_changes(hand);

	return false;
}

static void
handle_input_key_press(xcb_input_key_press_event_t const *const event)
{
	bool propagate = true;

	Device *const device = get_device_by_id(event->sourceid);
	assert(device);
	Hand *hand = device->hand;

	assert(hand == hands);
	if (!device->keymap)
		device->keymap = xkb_x11_keymap_new_from_device(xkb_context, conn, device->id, XKB_KEYMAP_COMPILE_NO_FLAGS);
	if (!device->keymap)
		goto out;

	xkb_keysym_t const *syms;
	int n = xkb_keymap_key_get_syms_by_level(device->keymap, event->detail, 0, XCB_MOD_MASK_SHIFT & event->mods.effective ? 1 : 0, &syms);
	if (0 == n)
		goto out;

	xkb_keysym_t const sym = syms[0];
	/* printf("key=0x%x mods=%d,%d,%d,%d deviceid=%d sourceid=%d (root=%x)\n", sym,
	       event->mods.effective,
	       event->mods.base,
	       event->mods.latched,
	       event->mods.locked,
	       event->deviceid, event->sourceid, event->root); */

	LabelMode const old_mode = hand->mode;
	bool const is_grab = hand->master_keyboard == event->deviceid;

	switch (hand->mode) {
	case mode_default:
	{
		/* if (XKB_KEY_Super_L == sym || XKB_KEY_Super_R == sym) {
			propagate = false; */
			/* hand_set_timeout(hand, 70); */
		/* 	goto out;
		} */

#if 0
		/* hand_set_timeout(hand, -1); */
		if (mode_boxes == hand->mode && 0) {
			hand->mode = mode_default;
			hand_grab_keyboard(event->root, event->deviceid);
		}
#endif
		bool const repeating = XCB_INPUT_KEY_EVENT_FLAGS_KEY_REPEAT & event->flags;

		if (!is_grab) {
			if (!(KEY_MOD_MASK & event->mods.base))
				hand_handle_input_key_normal(hand, sym, repeating);
			return;
		} else switch (KEY_MOD_MASK & event->mods.base) {
		default:
			propagate = (hand_handle_input_key_normal(hand, sym, repeating), true);
			break;

		case XCB_MOD_MASK_4:
			if (hand->master_keyboard != event->deviceid)
				break;

			propagate = hand_handle_input_key_super(hand, sym, repeating);
			break;

		case XCB_MOD_MASK_4 | XCB_MOD_MASK_CONTROL:
		case XCB_MOD_MASK_4 | XCB_MOD_MASK_1:
			propagate = hand_handle_input_key_command(hand, sym, repeating);
			break;
		}

		if (!hand->input_focus)
			propagate = false;
	}
		break;

	default:
		propagate = false;

		if (!is_grab)
			break;

		/* RIP. dumb user get stuck and tries to escape */
		if (KEY_MOD_MASK & event->mods.base)
			/* but we just let this little shit shuffering a bit */
			break;

		if (XKB_KEY_Escape == sym) {
			hand_leave_mode(hand);
			break;
		}

		hand_handle_input(hand, sym);

		hand_handle_input_key_mode(hand, sym);
		break;
	}

	if (old_mode != hand->mode)
		hand_grab_keyboard(hand);

#ifndef HEAWM_NDEBUG
out:
#endif
	xcb_input_xi_allow_events(conn, XCB_CURRENT_TIME, event->deviceid,
			propagate
				? XCB_INPUT_EVENT_MODE_REPLAY_DEVICE
				: XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
	/* if event gets propagated, flush as soon as possible in order to
	 * minimize delay. */
	if (propagate)
		xcb_flush(conn);
}

static void
handle_input_button_press(xcb_input_button_press_event_t const *const event)
{
	Hand *const hand = find_hand_by_master_pointer(event->deviceid);
#ifndef HEAWM_NDEBUG
	if (!hand)
		return;
#endif

	printf("button\n");

	bool propagate = true;
	Body *const body = get_body_by_root(event->root);
	Box *box = find_box_in_body_by_window(body, event->child);
	if (box) {
		propagate = false;
		hand_focus_box(hand, box);
	}

	xcb_input_xi_allow_events(conn, XCB_CURRENT_TIME, event->deviceid,
			propagate
				? XCB_INPUT_EVENT_MODE_REPLAY_DEVICE
				: XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
}

static void
handle_input_enter(xcb_input_enter_event_t const *const event)
{
	if (XCB_INPUT_NOTIFY_MODE_NORMAL != event->mode)
		return;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod4+Mouse Enter...
	 * Focus window.
	 */
	if (XCB_MOD_MASK_4 == event->mods.base) {
		Hand *const hand = find_hand_by_master_pointer(event->deviceid);
		Box *const box = find_box_by_window(root, root, event->event);
		hand_focus_box(hand, box);
	} else {
		xcb_input_xi_set_client_pointer(conn, event->child, event->deviceid);
	}
}

static void
handle_input_event(xcb_ge_generic_event_t const *const event)
{
	switch (event->event_type) {
	case XCB_INPUT_HIERARCHY:
		handle_input_hierarchy_change((void const *)event);
		break;

	case XCB_INPUT_FOCUS_IN:
		handle_input_focus_in((void const *)event);
		break;

	case XCB_INPUT_BUTTON_PRESS:
		handle_input_button_press((void const *)event);
		break;

	case XCB_INPUT_KEY_PRESS:
		handle_input_key_press((void const *)event);
		break;

#if 0
	case XCB_INPUT_KEY_RELEASE:
		/* printf("aeouaoeu\n"); */
		/* handle_input_key_release((void const *)event); */
		break;
#endif

	case XCB_INPUT_UNGRAB_DEVICE_KEY:
		break;

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
	Body *const body = get_body_by_root(event->root);
	body_update_heads(body);
}

static bool
handle_randr_event(xcb_generic_event_t const *const event)
{
	if (0 == randr_base_event)
		return false;

	switch (XCB_EVENT_RESPONSE_TYPE(event) - randr_base_event) {
	case XCB_RANDR_SCREEN_CHANGE_NOTIFY:
		handle_randr_screen_change_notify((void *)event);
		break;

	default:
		return false;
	}

	return true;
}

static void
handle_keyboard_mapping(uint8_t const device_id)
{
	Device *device = devices;
	while (device->id != device_id)
		++device;

	xkb_keymap_unref(device->keymap), device->keymap = NULL;
}

static void
handle_xkb_new_keyboard_notify(xcb_xkb_new_keyboard_notify_event_t *const event)
{
	if (!(event->changed & (XCB_XKB_NKN_DETAIL_DEVICE_ID |
	                        XCB_XKB_NKN_DETAIL_KEYCODES)))
		return;

	/* FIXME: Eh... device id may change? Then we must get hierarchy change notify not? */
	uint8_t const deviceid = event->changed & XCB_XKB_NKN_DETAIL_DEVICE_ID
		? event->oldDeviceID
		: event->deviceID;

	 handle_keyboard_mapping(deviceid);
}

static void
handle_xkb_map_notify(xcb_xkb_map_notify_event_t *const event)
{
	handle_keyboard_mapping(event->deviceID);
}

static bool
handle_xkb_event(xcb_generic_event_t const *const event)
{
	if (0 == xkb_base_event)
		return false;

	switch (XCB_EVENT_RESPONSE_TYPE(event) - xkb_base_event) {
	case XCB_XKB_NEW_KEYBOARD_NOTIFY:
		handle_xkb_new_keyboard_notify((void *)event);
		break;

	case XCB_XKB_MAP_NOTIFY:
		handle_xkb_map_notify((void *)event);
		break;

	default:
		return false;
	}

	return true;
}

static bool
handle_extension_event(xcb_generic_event_t const *const event)
{
	return
		handle_randr_event(event) ||
		handle_xkb_event(event);
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
hand_timed_out(Hand *const hand)
{
	assert(mode_default == hand->mode);
	printf("hand_timed_out(()\n");
	hand->mode = mode_default;
}

static void
hands_time_elapsed(int elapsed_ms)
{
	if (next_timeout_ms < elapsed_ms)
		elapsed_ms = next_timeout_ms;

	int new_timeout_ms = -1;

	printf("Elapsed=%dms\n", elapsed_ms);
	for (uint8_t i = 0; i < num_hands; ++i) {
		Hand *const hand = &hands[i];
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

static int
run(void)
{
	struct pollfd pfd;
	pfd.fd = xcb_get_file_descriptor(conn);
	pfd.events = POLLIN;

	for (xcb_generic_event_t *event;; free(event)) {
		while (!(event = xcb_poll_for_event(conn)) &&
		       /* update screen if we have no more pending requests */
		       /* FIXME: investigate whether starvation could be a problem */
		       (do_update(), xcb_flush(conn),
		        /* make XCB poll()-ing again to see whether we
		         * have received something after flushing. poll()
		         * should be level-triggered so not sure why it
		         * is needed... */
		        !(event = xcb_poll_for_event(conn))))
		{
			struct timespec poll_start;

			if (0 < next_timeout_ms)
				clock_gettime(HAND_TIMEOUT_CLOCK, &poll_start);
			assert(0 != next_timeout_ms && "next_timeout_ms should never be 0");

			assert(next_timeout_ms == -1);
			if (0 == poll(&pfd, 1, next_timeout_ms)) {
				assert(0);
				hands_time_elapsed(next_timeout_ms);
				continue;
			} else if (0 < next_timeout_ms) {
				assert(0);
				struct timespec now;
				clock_gettime(HAND_TIMEOUT_CLOCK, &now);
				hands_time_elapsed(((now.tv_sec - poll_start.tv_sec) << 10) + ((now.tv_nsec - poll_start.tv_nsec) >> 20));
				assert(0 != next_timeout_ms);
			}

			if (pfd.revents & ~POLLIN)
				return EXIT_FAILURE;
		}

		/* printf("event = (%d)%s\n", event->response_type, xcb_event_get_label(event->response_type)); */

		switch (XCB_EVENT_RESPONSE_TYPE(event)) {
		case 0:
			handle_error((void *)event);
			continue;

		case XCB_MAP_REQUEST:
			handle_map_request((void *)event);
			break;

		case XCB_CONFIGURE_REQUEST:
			handle_configure_request((void *)event);
			break;

		case XCB_UNMAP_NOTIFY:
			handle_unmap_notify((void *)event);
			break;

		case XCB_DESTROY_NOTIFY:
			handle_destroy_notify((void *)event);
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
			handle_extension_event((void *)event);
			break;
		}
	}
}

static void
init_config(void)
{
	(config.shell = getenv("SHELL")) ||
	(config.shell = "sh");

	(config.terminal = getenv("TERMINAL")) ||
	(config.terminal = "xterm");

	char *env;
	/*MAN(ENVIRONMENT)
	 * .TP
	 * .B HEAWM_HOME
	 * Specifies directory where main configuration resides.
	 */
	if ((env = getenv("HEAWM_HOME"))) {
		snprintf(config.heawm_home, sizeof config.heawm_home,
				"%s", env);
	} else if ((env = getenv("XDG_CONFIG_HOME"))) {
		/* FIXME: split on ':' */
		snprintf(config.heawm_home, sizeof config.heawm_home,
				"%s/heawm", env);
	} else if ((env = getenv("HOME"))) {
		struct stat st;

		snprintf(config.heawm_home, sizeof config.heawm_home,
				"%s/.heawm", env);

		if (stat(config.heawm_home, &st) || !S_ISDIR(st.st_mode))
			snprintf(config.heawm_home, sizeof config.heawm_home,
					"%s/.config/heawm", env);
	} else {
		strcpy(config.heawm_home, ".");
	}
	/* make sure environment variable set so can be used by scripts */
	setenv("HEAWM_HOME", config.heawm_home, false);
}

int
main(int _argc, char *_argv[])
{
	argc = _argc, argv = _argv;
	atexit(quit);

	setup_signals();

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
			fprintf(stderr, "Option -%c is invalid or requires an argument.\n",
					optopt);
			exit(EXIT_FAILURE);

		default:
			abort();
		}
	}

	init_config();

	connect_display();
	setup_xrm();
	setup_display();

	/*MAN(HOOKS)
	 * .TP
	 * .B startup
	 * Run on program (re)start.
	 */
	SPAWN(heawm_file("startup"));

	return run();
}
