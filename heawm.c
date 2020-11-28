#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <memory.h>
#include <poll.h>
#include <signal.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
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
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_image.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xcb_xrm.h>
#include <xcb/xfixes.h>
#include <xcb/bigreq.h>
#include <xcb/xinput.h>
#include <xcb/xkb.h>
#include <xcb/xproto.h>
#include <xcb/xtest.h>
#include <xkbcommon/xkbcommon.h>
#include <xkbcommon/xkbcommon-x11.h>

/* TEST: https://superuser.com/questions/801611/how-to-make-all-applications-respect-my-modified-xkb-layout/844673#844673 */

/* motif: https://gitlab.gnome.org/GNOME/gtk/-/blob/master/gdk/x11/MwmUtil.h */
/* https://people.gnome.org/~tthurman/docs/metacity/xprops_8h-source.html */

#ifdef HEAWM_NDEBUG
/** suppress debug output */
#endif

#ifndef M_PHI
# define M_PHI 1.6180339887 /* Golden Ratio */
#endif

#define EXTREMAL_NAME_CHAR '\xff'

#define RGB8_TO_FLOATS(color) \
	(uint8_t)((color) >> 16) / 256., \
	(uint8_t)((color) >> 8 ) / 256., \
	(uint8_t)((color)      ) / 256.

/* sizeof array... but in elements */
#define ARRAY_SIZE(...) (sizeof((__VA_ARGS__)) / sizeof(*(__VA_ARGS__)))
#define offsetof(type, member) (size_t)(&((type *)0)->member)
#define membersizeof(type, member) (sizeof(((type *)0)->member))

#if defined(__GNUC__)
# define maybe_unused __attribute__((unused))
#else
# define maybe_unused
#endif

#ifndef HEAWM_NDEBUG
# if defined(__GNUC__) || defined(clang)
#  define unreachable __builtin_unreachable()
# else
#  define unreachable
# endif
#else
# define unreachable abort()
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
		char zero_pad[MAX(membersizeof(xcb_send_event_request_t, ev##ent) - sizeof(event), 1)]; \
	}){ \
		.data = { __VA_ARGS__ } \
	}

#define XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(grab_window, deviceid, grab_type, detail, ...) \
	xcb_input_xi_passive_ungrab_device(conn, (grab_window), \
			(detail), (deviceid), \
			ARRAY_SIZE((uint32_t const[])__VA_ARGS__), \
			(grab_type), \
			(uint32_t const[])__VA_ARGS__)

#define XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(grab_window, deviceid, grab_type, detail, grab_mode, owner_events, mask, ...) \
	xcb_input_xi_passive_grab_device_unchecked(conn, XCB_CURRENT_TIME, (grab_window), \
			XCB_CURSOR_NONE, (detail), (deviceid), \
			ARRAY_SIZE((uint32_t const[])__VA_ARGS__), 1, \
			(grab_type), \
			(grab_mode), XCB_INPUT_GRAB_MODE_22_ASYNC, \
			(owner_events), \
			(uint32_t const[]){ (mask) }, (uint32_t const[])__VA_ARGS__)

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

#define FRAME_WINDOW_EVENT_MASK \
(	XCB_EVENT_MASK_STRUCTURE_NOTIFY \
|	XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT \
)

#define CLIENT_WINDOW_EVENT_MASK \
(	XCB_EVENT_MASK_PROPERTY_CHANGE \
|	XCB_EVENT_MASK_STRUCTURE_NOTIFY \
|	XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY \
|	XCB_EVENT_MASK_FOCUS_CHANGE \
|	XCB_EVENT_MASK_ENTER_WINDOW \
)

#define NULL_BODY 128U
#define MAX_NUM_HANDS 63U
#define NULL_HAND (MAX_NUM_HANDS + 1U)

#define XCB_STRING_MAX (64 * sizeof(uint32_t))

#define LABEL_CLASS "heawm"
#define LABEL_INSTANCE LABEL_CLASS "-label"

#define MONITOR_CLASS "heawm-monitor"

# define NORMAL_GRAB_MASKS { \
	0, \
	XCB_MOD_MASK_SHIFT, \
	XCB_MOD_MASK_CONTROL, \
	XCB_MOD_MASK_1, \
\
	XCB_MOD_MASK_LOCK | 0, \
	XCB_MOD_MASK_LOCK | XCB_MOD_MASK_SHIFT, \
	XCB_MOD_MASK_LOCK | XCB_MOD_MASK_CONTROL, \
	XCB_MOD_MASK_LOCK | XCB_MOD_MASK_1, \
}

#define for_each_template_(elem_type, list, index_type, name, extra) \
	for (index_type name##_index = 0; (extra) || name##_index < num_##list; ++name##_index) \
		for (elem_type *const name = &list[name##_index], *loop_ = (void *)true; loop_; loop_ = (void *)false)

#define for_each_body for_each_template_(Body, bodies, uint8_t, body, 0 == body_index)
#define for_each_hand for_each_template_(Hand, hands, uint8_t, hand, 0)

#define for_each_box(x, root, start) \
	for (bool loop_ = ((x) = (start), true); loop_ && ((x)->iter = 0, true); ({ \
		while ((x)->num_children <= (x)->iter) { \
			if ((x) == (root)) { \
				(x) = NULL; \
				loop_ = false; \
				break; \
			} \
			assert((x)->parent); \
			(x) = (x)->parent; \
		} \
		if (loop_) \
			(x) = (x)->children[x->iter++]; \
	}))

static struct {
	char const *terminal;
	char const *shell;
	char heawm_home[PATH_MAX];
} config;

typedef struct {
	xcb_window_t window; /** relative to */
	xcb_input_fp1616_t x, y;
} BoxPointer;

#define Box_pointers(box) ((BoxPointer *)&box->children)

typedef struct box Box;
struct box {
	int16_t x, y; /** box location relative to root box */
	uint16_t width, height;

	/** user requested rect; mainly for floating windows */
	int16_t user_x, user_y;
	uint16_t user_width, user_height;

	xcb_window_t window;
	/* parent of window
	 *
	 * why?
	 * (1) because retard GIMP unmaps its dialog boxes when its main
	 *     window. who the fuck asked for it? really... so much shit... its the
	 *     task of wm bitches.
	 * (2) programs may keep around unused windows unmapped and if such
	 *     windows had been concealed they will not be deleted. */
	xcb_window_t frame;
	xcb_window_t leader;

	uint16_t num_children,
	/** internal variable used to track progress of recursive searching
	 *
	 * - it does not occupy too much space and we do not have to
	 *   dynamically allocate more memory for backtracking or track maximum
	 *   possible depth
	 * - it also makes very convenient to continue the walk where we left
	 *   off */
	         iter,

	/**
	 * specially:
	 * - 0: auto,
	 * - 1: column,
	 * - -1: row,
	 * - other: fixed, user set */
	         num_columns;

	char name[2]; /** address of box for user */

	uint8_t mod_x, mod_y; /** size granulaty */

	/** relative size to siblings
	 *
	 * used only when box has one variable dimension, i.e. when parent
	 * layout consits of a single row or column. */
	uint8_t weight;
	uint8_t body;

	/** which hand focused the box (last time)
	 *
	 * because simultenously multiple hands can focus a box, it is only
	 * valid if box->focus_seq != root->focus_seq (not currently focused).
	 * in these cases focus_hand is garbage and to find out which hand(s)
	 * hold(s) the focus: hands[..]->input_focus ?= box.
	 */
	uint8_t focus_hand;

#if 0
	char *title; /* stuff that desribes window (X11 window title or monitor name) */
#endif
	char *class;

	/* we always update the whole scene. these variables help to track changes */
	bool position_changed: 1, /** box repositioned */
	     layout_changed: 1, /** any of the children's boundary box changed */
	     focus_changed: 1, /** focused child changed */
	     content_changed: 1, /** set by children to indicate parent should
	                            descend with update (means that this flag
	                            must be propagated upwards until root);
	                            for client windows indicates that name or title changed */
#if 0
	     title_changed: 1, /** for containers, set by children */
#endif
	     label_changed: 1, /** label should be redrawn */
	     close_by_force: 1,

	     hide_label: 1,
	     /** show only when has focus; hide otherwise */
	     concealed: 1,
	     user_concealed: 1,
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

	Box *parent;
	Box *children[];
};

/** mother of all boxes */
static Box *root;

static bool ewmh_client_list_changed;

typedef enum {
	LABEL_NORMAL, /** text only */
	LABEL_BOX, /** draw glory if |base| focused */
	LABEL_HLINE, /** text with a horizontal line */
	LABEL_VLINE, /** text with a vertical line */
} LabelType;

/** little angels flying around the screen */
typedef struct {
	Box *base;
	uint64_t hands;
	xcb_window_t window;
	xcb_pixmap_t shape;
	int16_t x, y;
	LabelType type: 2;
	bool position_changed: 1,
	     content_changed: 1;
	char name[membersizeof(Box, name)];
} Label;

typedef struct {
	xcb_screen_t *screen;
	xcb_visualtype_t *visual_type;
	int screen_index;

	uint32_t num_labels_used,
	/* <= */ num_labels_mapped,
	/* <= */ num_labels;
	Label *labels;

	xcb_window_t net_window;
} Body;

static uint8_t num_bodies;
static Body *bodies;
static int default_screen;

/* not a too exact time */
static int next_timeout_ms = -1;

typedef enum {
	HAND_MODE_DEFAULT,
	HAND_MODE_SETCOLUMNS,
	HAND_MODE_MOVE,
	HAND_MODE_SIZE_SIDE,
	HAND_MODE_SIZE_TO,
	HAND_MODE_NAME,
} HandMode;

typedef struct {
	/* keyboard and pointer are always in pair */
	xcb_input_device_id_t master_pointer; /** master pointer device */
	xcb_input_device_id_t master_keyboard; /** master key device */

	/** focus freshly mapped related windows */
	bool want_focus: 1,
	     check_input: 1,
	     barricade: 1;

	/*
	 *   0
	 *1 BOX 3
	 *   2
	 */
	xcb_xfixes_barrier_t barriers[4]; /** pointer barrier (four sides) */

	char user_input[membersizeof(Box, name)];

	uint32_t num_labels; /** number of labels owned by this hand */

	/** time until |hand_timed_out()| is called; -1 if disarmed */
	int timeout_ms;

	HandMode mode;

	/* some |mode| related state */
	Box *mode_box;
	unsigned mode_dir;

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

	unsigned int color; /** 0xrrggbb */
} Hand;

static uint8_t num_hands;
static Hand *hands;

typedef struct {
	xcb_input_device_id_t id;
	struct xkb_keymap *keymap; /* NULL if not keyboard */
	Hand *hand;
} Device;

static uint8_t num_devices;
static Device *devices;

typedef struct {
	int16_t x, y;
} Point;

#define label_stroke_rgb 0, 0, 0
#define label_color_rgb 1, 1, 0

static char const *label_font = "monospace";
static Point label_rect = { .x = 30, .y = 60 }; /** in pts */
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
	"_HEAWM_NAME",
	"_NET_ACTIVE_WINDOW",
	"_NET_CLIENT_LIST",
	"_NET_CLOSE_WINDOW",
	"_NET_SUPPORTING_WM_CHECK",
	"_NET_WM_NAME",
	"_NET_WM_STATE",
	"_NET_WM_STATE_DEMANDS_ATTENTION",
	"_NET_WM_STATE_HIDDEN",
	"_NET_WM_STATE_FOCUSED",
	"_NET_WM_TRANSIENT_FOR",
	"UTF8_STRING",
	"WM_CLIENT_LEADER",
	"WM_DELETE_WINDOW",
	"WM_NORMAL_HINTS",
	"WM_PROTOCOLS",
	"WM_SIZE_HINTS",
	"WM_STATE",
#if 0
	/* https://specifications.freedesktop.org/wm-spec/1.3/ar01s05.html */
	/* https://specifications.freedesktop.org/wm-spec/1.3/ar01s07.html */
	/* https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html#idm46035372536800 */
	"_NET_WM_WINDOW_STATE",
	"_NET_WM_WINDOW_TYPE",
	"_NET_WM_TYPE_DOCK",
#endif
};
static xcb_atom_t atoms[ARRAY_SIZE(ATOM_NAMES)]; /** resolved ATOM_NAMES */

static xcb_connection_t *conn;
static xcb_xrm_database_t *xrm;
static int preferred_screen;
/* https://www.x.org/releases/X11R7.7/doc/inputproto/XI2proto.txt */
static uint8_t xi_opcode; /** major opcode of XInput extension */
/* https://www.x.org/releases/X11R7.7/doc/xextproto/shape.html */
static uint8_t shape_base_event; /** major opcode of Shape extension */
/* https://www.x.org/releases/X11R7.5/doc/randrproto/randrproto.txt */
static uint8_t randr_base_event; /** beginning of XRandR event range */
static uint8_t xkb_base_event; /** beginning of XKB event range */
static struct xkb_context *xkb_context;
static xcb_key_symbols_t *symbols;

static int argc;
static char **argv;

static uint16_t const WINDOW_GAP = 1;
static uint16_t const CONTAINER_GAP = 4;

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
#define BODY_SPAWN(body, ...) spawn((char const *[]){ __VA_ARGS__, NULL }, (void(*)(void *))body_set_display, (body))

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
		for (int sig = 1; sig < SIGRTMAX; ++sig)
			sigaction(sig, &(struct sigaction const){
					 .sa_handler = SIG_DFL
				}, NULL);

		sigemptyset(&sigmask);
		pthread_sigmask(SIG_SETMASK, &sigmask, NULL);

		/* disconnet standard io */
		dup2(open("/dev/null", O_RDONLY | O_CLOEXEC), STDIN_FILENO);
		dup2(open("/dev/null", O_WRONLY | O_CLOEXEC), STDOUT_FILENO);
		/* leave stderr untouched */

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
find_box_by_window(Box *const root, Box *start, size_t const offset, xcb_window_t const window)
{
	if (XCB_WINDOW_NONE == window)
		return NULL;

	Box *box;
	for_each_box(box, root, start)
		if (window == *(xcb_window_t const *)((uintptr_t)box + offset))
			break;
	return box;
}

static bool
box_is_monitor(Box const *const box)
{
	return !box->parent->parent;
}

static Box *
box_get_head(Box const *box)
{
	while (!box_is_monitor(box))
		box = box->parent;
	return (Box *)box;
}

static Point
monitor_convert_pt2px(Box const *const monitor, Point const pt)
{
	assert(box_is_monitor(monitor));

#define CONVERT(x, width) .x = ((int)monitor->width * (int)pt.x * 254 / 720 / (int)monitor->user_##width)
	return (Point){ CONVERT(x, width), CONVERT(y, height) };
#undef CONVERT
}

static bool
box_is_container(Box const *const box)
{
	return XCB_WINDOW_NONE == box->frame;
	return !(box->name[0] & 0x20);
}

static bool
box_is_focused(Box const *const box)
{
	return box->focus_seq && root->focus_seq == box->focus_seq;
}

static void
label_repaint(Label const *const label, bool const shape)
{
	char name[sizeof label->name + 1];
	memcpy(name, label->name, sizeof label->name);
	name[sizeof name - 1] = '\0';

	Box const *const monitor = box_get_head(label->base);
	Point const size = monitor_convert_pt2px(monitor, label_rect);
	int const font_size = monitor_convert_pt2px(monitor,
			(Point){ 0, label_size }).y;
	int const stroke_width = monitor_convert_pt2px(monitor,
			(Point){ label_stroke, 0 }).x;

	Body const *const body = &bodies[label->base->body];
	cairo_surface_t *const surface = shape
		? cairo_xcb_surface_create_for_bitmap(conn, body->screen, label->shape, size.x, size.y)
		: cairo_xcb_surface_create(conn, label->window, body->visual_type, size.x, size.y);
	cairo_t *const cr = cairo_create(surface);

	if (/* no compositor */true)
		cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);

	cairo_select_font_face(cr, label_font,
			CAIRO_FONT_SLANT_NORMAL,
			CAIRO_FONT_WEIGHT_BOLD);
	cairo_set_font_size(cr, font_size);

	/* https://www.designworkplan.com/read/signage-and-color-contrast */
	/* http://www.writer2001.com/colwebcontrast.htm */
	/* https://dequeuniversity.com/tips/color-contrast */
	/* cairo_rectangle(cr, 0, 0, 40, 40);
	cairo_set_source_rgb(cr, 1.0, 1.0, 0.0); */
	/* cairo_fill(cr); */

	if (shape) {
		cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
		cairo_rectangle(cr, 0, 0, size.x, size.y);
		cairo_fill(cr);
	}

	cairo_translate(cr, size.x / 2, size.y / 2);

	cairo_text_extents_t te;
	cairo_text_extents(cr, name, &te);

	int const radius = font_size / 2/*sqrt(te.y * te.y + te.x * te.x)*/;

	switch (label->type) {
	case LABEL_NORMAL:
	normal:
		/* no extra stuff */
		break;

	case LABEL_BOX:
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

	case LABEL_HLINE:
	case LABEL_VLINE:
		cairo_set_line_width(cr, 1);
		if (shape)
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

		if (LABEL_HLINE == label->type) {
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

	double te_left = -(te.width + te.x_bearing) / 2;
	double te_top = -te.y_bearing - te.height / 2;

	if (!shape)
		cairo_set_source_rgb(cr, label_stroke_rgb);
	else
		cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
	cairo_move_to(cr, te_left, te_top);
	cairo_text_path(cr, name);
	cairo_set_line_width(cr, stroke_width + .5);
	cairo_stroke(cr);

	if (!shape)
		cairo_set_source_rgb(cr, label_color_rgb);
	cairo_move_to(cr, te_left, te_top);
	cairo_show_text(cr, name);

	char const *symbol = NULL;

	if (LABEL_BOX == label->type) {
		if (label->base->user_concealed)
			symbol = label->base->concealed ? "-" : "+";
		else if (label->base->focus_lock && box_is_container(label->base))
			symbol = "\xe2\x80\xa2" /* U+2022 BULLET */;
	}

	if (symbol) {
		cairo_set_font_size(cr, font_size);

		cairo_text_extents(cr, symbol, &te);

		te_left -= te.width + te.x_bearing + stroke_width / 2;
		te_top = -te.y_bearing - te.height / 2;

		if (!shape)
			cairo_set_source_rgb(cr, label_stroke_rgb);
		else
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
		cairo_move_to(cr, te_left, te_top);
		cairo_text_path(cr, symbol);
		cairo_set_line_width(cr, stroke_width + .5);
		cairo_stroke(cr);

		if (!shape)
			cairo_set_source_rgb(cr, label_color_rgb);
		cairo_move_to(cr, te_left, te_top);
		cairo_show_text(cr, symbol);
	}

	if (!shape)
		cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	cairo_surface_flush(surface);
	cairo_surface_destroy(surface);
	cairo_destroy(cr);
}

static Label *
label_new_for(Box *const box)
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
	label->type = LABEL_NORMAL;
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
			" (seq=%d, opcode=%d:%d, value=%d=0x%x)\n",
			__FILE__, message,
#ifndef HEAWM_NDEBUG
			xcb_event_get_error_label(error->error_code),
#else
			error->error_code,
#endif
			error->sequence,
			error->major_code, error->minor_code,
			error->resource_id, error->resource_id);
}

static int
check_cookie(xcb_void_cookie_t const cookie, char const *const request)
{
	int ret = 0;
	xcb_generic_error_t *error;

	if ((error = xcb_request_check(conn, cookie))) {
		print_error(error, request);
		ret = error->error_code;
		if (0 == ret)
			unreachable;
		free(error);
	}

	return ret;
}

static void
label_set_name(Label *label, char name[static membersizeof(Label, name)])
{
	label->content_changed |= !!memcmp(label->name, name, sizeof label->name);
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

typedef enum {
	LEFT = -1,
	TOP = -1,
	CENTER = 0,
	RIGHT = 1,
	BOTTOM = 1,
} Orientation;

static Point
box_compute_position(Box const *const box, Orientation const ox, Orientation const oy, bool const outside)
{
	Box const *const monitor = box_get_head(box);
	Point const size = monitor_convert_pt2px(monitor, label_rect);

	Point ret = {
		.x = box->x + (ox + 1) * (box->width  -size.x) / 2 + (outside ? ox : 0) * (size.x / 2),
		.y = box->y + (oy + 1) * (box->height -size.y) / 2 + (outside ? oy : 0) * (size.y / 2)
	};

#define CLAMP(x, width) \
	if (ret.x < monitor->x) \
		ret.x = monitor->x; \
	else if (monitor->x + monitor->width < ret.x + size.x) \
		ret.x = monitor->x + monitor->width - size.x;

	CLAMP(x, width)
	CLAMP(y, height)

#undef CLAMP

	return ret;
}

static void
label_set_position_to_box(Label *const label, Orientation const ox, Orientation const oy, bool const outside)
{
	Point const pt = box_compute_position(label->base, ox, oy, outside);
	label_set_position(label, pt.x, pt.y);
}

static void
label_create_window(Label *const label)
{
	assert(label->base);
	Body const *const body = &bodies[label->base->body];
	xcb_screen_t const *const screen = body->screen;

	label->window = xcb_generate_id(conn);

	Box const *const monitor = box_get_head(label->base);
	Point const size = monitor_convert_pt2px(monitor, label_rect);

	DEBUG_CHECK(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			label->window,
			screen->root,
			label->x, label->y,
			size.x, size.y,
			0,
			XCB_WINDOW_CLASS_INPUT_OUTPUT,
			XCB_COPY_FROM_PARENT,
			XCB_CW_SAVE_UNDER |
			XCB_CW_OVERRIDE_REDIRECT |
			XCB_CW_EVENT_MASK,
			(uint32_t const[]){
				true,
				true,
				XCB_EVENT_MASK_EXPOSURE
			});

	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			label->window, XCB_ATOM_WM_CLASS,
			XCB_ATOM_STRING, 8,
			sizeof LABEL_INSTANCE "\0" LABEL_CLASS,
			LABEL_INSTANCE "\0" LABEL_CLASS);

	/* TODO: search for labels with the same name and type and copy shape from there */
	/* setup its shape */
	label->shape = xcb_generate_id(conn);

	DEBUG_CHECK(xcb_create_pixmap, conn,
		/* mask is on or off */ 1,
		label->shape,
		label->window,
		size.x, size.y);

	/* we need a valid pixmap so we use the bounding mask but we
	 * use offsets to move it outside of the area making effective
	 * input region empty */
	DEBUG_CHECK(xcb_shape_mask, conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_INPUT,
			label->window,
			size.x, size.y,
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
label_update(Label *const label)
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
				(uint32_t const[]){
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

	for (size_t i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		char const *const name = ATOM_NAMES[i];
		cookies[i] = xcb_intern_atom_unchecked(conn, false, strlen(name), name);
	}

	for (size_t i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
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
find_box_by_name(Box **optimum, char name[static membersizeof(Box, name)])
{
	*optimum = NULL;

	uint8_t const n = strnlen(name, membersizeof(Box, name));
	if (0 == n)
		return false;

	bool complete = true;
	Box *box;
	for_each_box(box, root, root)
		if (!memcmp(name, box->name, n)) {
			complete &= membersizeof(Box, name) <= n || '\0' == box->name[n];
			if (!*optimum || (*optimum)->focus_seq < box->focus_seq)
				*optimum = box;
		}

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

static char const *
box_get_class_instance(Box const *const box)
{
	return box->class ? box->class + strlen(box->class) + 1 : NULL;
}

static bool
box_match_class(Box const *const box, char const *const class, char const *const instance)
{
	return (!class || (box->class && !strcmp(class, box->class))) &&
	       (!instance || (box_get_class_instance(box) && !strcmp(instance, box_get_class_instance(box))));
}

static void
box_name(Box *const box)
{
	if (EXTREMAL_NAME_CHAR == *box->name)
		return;

	bool const is_container = box_is_container(box);
	/* collect the min distance, max focus_seq */
	struct {
		uint32_t focus_seq;
	} letters[127];
	/* then search for max distance, min focus_seq */

	memset(letters, 0, sizeof letters);

	uint8_t const n = strnlen(box->name, sizeof box->name);

	/* TODO: if box has name, leave it and check if there are conflicts in its parent */
	if (0 < n)
		return;

	Box *to = box->parent;

	/* prohibit same name among children so we can always move horizontally */
	if (to)
		for (uint16_t i = 0; i < to->num_children; ++i) {
			Box const *const child = to->children[i];
			if (!memcmp(child->name, box->name, n))
				letters[(unsigned char)child->name[n]].focus_seq = -1;
		}

	/* prohibit same name in subtree so we can always move vertically */
	for (to = box; (to = to->parent);)
		if (!memcmp(to->name, box->name, n))
			letters[(unsigned char)to->name[n]].focus_seq = -1;

	unsigned char optimum = '\0';

	if (!n && !is_container) {
		if (box_match_class(box, NULL, "Alacritty"))
			optimum = 'a';
		else if (box_match_class(box, "gl", "mpv"))
			optimum = 'v';
		else if (box_match_class(box, "Navigator", "firefox"))
			optimum = 'b';
		else if (box_match_class(box, "telegram-desktop", "TelegramDesktop"))
			optimum = 't';
	}

	if (!optimum)
		letters[optimum].focus_seq = -1;

	for_each_box(to, root, root)
		if (!memcmp(to->name, box->name, n)) {
			uint32_t *const p = &letters[(unsigned char)to->name[n]].focus_seq;
			if (*p < to->focus_seq + 1)
				*p = to->focus_seq + 1;
		}

	for (unsigned char start = is_container ? 'A' : 'a', end = start + ('Z' - 'A');
	     start <= end;
	     ++start)
	{
		if (letters[start].focus_seq < letters[optimum].focus_seq)
			optimum = start;
	}

	if (!optimum) {
		abort();
		return;
	}

	box->name[n] = optimum;

	/* TODO: make sure upper part of name is zeroed */

	if (!box_is_container(box))
		DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				box->window, ATOM(_HEAWM_NAME),
				XCB_ATOM_STRING, 8,
				n + 1, box->name);

	/* name changed, labels need to be updated */
	box_propagate_change(box)->content_changed = true;
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
		 * in more squary tiles. the process repeats until we can place
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
box_set_position(Box *const box, int16_t const x, int16_t const y)
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

static void
box_save_pointer(Box *const box, Hand const *const hand)
{
	GET_REPLY(reply, xcb_input_xi_query_pointer, conn,
			bodies[box->body].screen->root, hand->master_pointer);
	if (!reply)
		return;

	Box const *const base = find_box_by_window(root, root, offsetof(Box, frame), reply->child);
	Box_pointers(box)[hand - hands] = base
		? (BoxPointer){
			.window = reply->child,
			.x = reply->root_x - (base->x << 16),
			.y = reply->root_y - (base->y << 16),
		}
		: (BoxPointer){
			.window = reply->root,
			.x = reply->root_x,
			.y = reply->root_y,
		};

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
hand_unbarricade(Hand *const hand)
{
	if (!hand->barriers[0])
		return;

	for (uint8_t i = 0; i < ARRAY_SIZE(hand->barriers); ++i)
		DEBUG_CHECK(xcb_xfixes_delete_pointer_barrier, conn, hand->barriers[i]);
	hand->barriers[0] = XCB_NONE;
}

static void
hand_barricade(Hand *const hand, Box const *const box)
{
	hand_unbarricade(hand);
	for (uint8_t i = 0; i < ARRAY_SIZE(hand->barriers); ++i)
		DEBUG_CHECK(xcb_xfixes_create_pointer_barrier, conn,
				(hand->barriers[i] = xcb_generate_id(conn)),
				box->window,
				box->x + (i < 2 ? 0 : box->width),
				box->y + (i < 2 ? 0 : box->height),
				box->x + (i % 2 ? 0 : box->width),
				box->y + (i % 2 ? box->height : 0),
				i < 2 ? XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_X | XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_Y
				      : XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_X | XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_Y,
				1, (uint16_t const[]){ hand->master_pointer });
}

static void
hand_refocus(Hand *const hand)
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

	if (hand->barricade && focus)
		hand_barricade(hand, focus);
}

static void
hand_set_barrier(Hand *const hand, bool const barricade)
{
	if (hand->barricade == barricade)
		return;

	if ((hand->barricade = barricade))
		hand_refocus(hand);
	else
		hand_unbarricade(hand);
}

static void
hand_focus(Hand *const hand)
{
	hand_refocus(hand);

	if (hand->input_focus)
		box_restore_pointer(hand->input_focus, hand);
}

static bool
box_is_visible(Box const *const box)
{
	bool const ret = !box->concealed || box->parent->focus_seq == box->focus_seq;
	if (!ret && box->parent->focus_seq == root->focus_seq)
		for_each_hand
			if (box->parent == hand->focus)
				return true;
	return ret;
}

static bool
box_is_tiled(Box const *const box)
{
	return box_is_visible(box) && !box_is_floating(box);
}

static uint16_t
box_get_num_tiles(Box const *const box)
{
	uint16_t count = 0;

	for (uint16_t i = 0; i < box->num_children; ++i)
		count += box_is_tiled(box->children[i]);

	return count;
}

static void
label_assign_hand(Label *const label, Hand const *const hand)
{
	label->hands |= 1 << (hand - hands);
}

static void
generate_name(char name[static membersizeof(Box, name)], uint32_t number)
{
	enum { N_SYMBOLS = 'z' - 'a' + 1 };

	memset(name, 0, membersizeof(Box, name));

	char *p = name;
	do {
		*p++ = 'a' + (number % N_SYMBOLS);
	} while (0 < (number /= N_SYMBOLS));

	if (p < name + membersizeof(Box, name))
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
		char name[membersizeof(Box, name)];
		Label *label;

		switch (hand->mode) {
		case HAND_MODE_DEFAULT:
			if (box->hide_label)
				break;
			/* fall through */
		case HAND_MODE_NAME:
		case HAND_MODE_MOVE:
			label = label_new_for(box);
			if (HAND_MODE_NAME == hand->mode && hand->mode_box == box) {
				char display_name[sizeof name + 1];
				memcpy(display_name, hand->user_input, sizeof display_name);
				display_name[strnlen(display_name, sizeof display_name)] = '?';
				label_set_name(label, display_name);
			} else {
				label_set_name(label, box->name);
			}
			Point pt = box_compute_position(label->base, box_is_container(box) ? CENTER : RIGHT, TOP, false);
			if (box_is_container(box)) {
				Box const *const monitor = box_get_head(box);
				int const font_size = monitor_convert_pt2px(monitor,
						(Point){ 0, label_size }).y;

				pt.y -= font_size;
				for (Box const *b = box; !box_is_floating(b); b = b->parent)
					pt.y += font_size;
			}
			label_set_position(label, pt.x, pt.y);
			label->type = LABEL_BOX;
			label_assign_hand(label, hand);
			label_update(label);
			break;

#if 0
		{
			/* not a real child */
			if (box_is_monitor(box))
				break;

			bool const is_container = box_is_container(box);
			bool const leftmost_col = box->parent->x == box->x;
			bool const top_row = box->parent->y != box->y;
			bool const bottom_row = box->parent->y + box->height == box->y + box->height;
			bool const rightmost_col = box->parent->x + box->width == box->x + box->width;
			uint8_t i = 0;
			int to[8];

			if (is_container || !(top_row && leftmost_col))
				to[i++] = left, to[i++] = top;

			if (is_container || (bottom_row && !leftmost_col))
				to[i++] = left, to[i++] = bottom;

			if (is_container || (!top_row && rightmost_col))
				to[i++] = right, to[i++] = bottom;

			if (is_container || !(bottom_row && rightmost_col))
				to[i++] = right, to[i++] = top;

			while (0 < i) {
				i -= 2;

				generate_name(name, hand->num_labels++);
				label = label_new_for(box);
				label_assign_hand(label, hand);
				label_set_name(label, name);
				label_set_position_to_box(label, to[i], to[i + 1], true);
				label_update(label);
			}
		}
			break;
#endif

		case HAND_MODE_SETCOLUMNS:
		{
			if (box->parent != hand->mode_box->parent)
				break;

			label = label_new_for(box);
			generate_name(name, hand->num_labels++);
			label_set_name(label, name);
			label_set_position_to_box(label, CENTER, CENTER, false);
			label_update(label);
		}
			break;

		case HAND_MODE_SIZE_SIDE:
		{
			if (box_is_monitor(box))
				break;

			if (box_is_floating(box))
				goto all_four;

			if (!box_is_split(box->parent))
				break;

		all_four:
			label = label_new_for(box);
			strncpy(name, "h", sizeof name);
			label_set_name(label, name);
			label_set_position_to_box(label, LEFT, CENTER, true);
			label_update(label);

			label = label_new_for(box);
			strncpy(name, "j", sizeof name);
			label_set_name(label, name);
			label_set_position_to_box(label, CENTER, BOTTOM, true);
			label_update(label);

			label = label_new_for(box);
			strncpy(name, "k", sizeof name);
			label_set_name(label, name);
			label_set_position_to_box(label, CENTER, TOP, true);
			label_update(label);

			label = label_new_for(box);
			strncpy(name, "l", sizeof name);
			label_set_name(label, name);
			label_set_position_to_box(label, RIGHT, CENTER, true);
			label_update(label);
		}
			break;

		case HAND_MODE_SIZE_TO:
		{
			if (box != hand->mode_box)
				break;

			Box const *const monitor = box_get_head(box);
			Point const size = monitor_convert_pt2px(monitor, label_rect);

			for (char ch = 'a'; ch <= 'z'; ++ch) {
				label = label_new_for(box);
				name[0] = ch;
				name[1] = '\0';
				label_set_name(label, name);
				label_set_position(label, box->x + (ch - 'a') * size.x, box->y + box->height / 2);
				label_update(label);
			}
		}
			break;
		}
	}
}

static void
box_delete(Box *box);

static void
xcb_icccm_set_wm_state(xcb_window_t const window, xcb_icccm_wm_state_t const state)
{
	typedef struct {
		uint32_t state;
		xcb_window_t icon;
	} xcb_icccm_wm_state_data_t;

	if (XCB_ICCCM_WM_STATE_WITHDRAWN == state)
		DEBUG_CHECK(xcb_delete_property, conn, window, ATOM(WM_STATE));
	else
		DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				window, ATOM(WM_STATE),
				ATOM(WM_STATE), 32, sizeof(xcb_icccm_wm_state_data_t) / sizeof(uint32_t),
				&(xcb_icccm_wm_state_data_t){
					.state = state,
					.icon = XCB_WINDOW_NONE,
				});
}

static void
box_update(Box *const box)
{
	static int depth = 0;

#define HEAWM_DEBUG_UPDATE
#ifndef HEAWM_DEBUG_UPDATE
#define printf(...) (0)
#endif

	if (box != root)
		printf(
				"%*.sbox 0x%p (%.*s) win=0x%x %ux%u+%d+%d u%ux%u+%d+%d leader=%x"
#if 0
				" title=\"%s\""
#endif
				" class=\"%s\", \"%s\" focus=%d/%d hand=%d %c%c%c%c\n",
				depth, "",
				(void *)box,
				(int)sizeof box->name, box->name,
				box->window,
				box->width, box->height, box->x, box->y,
				box->user_width, box->user_height, box->user_x, box->user_y,
				box->leader,
#if 0
				box->title,
#endif
				box->class,
				box_get_class_instance(box),
				box->focus_seq, root->focus_seq,
				box->focus_hand,
				box_is_floating(box) ? 'F' : '-',
				box->focus_lock ? 'L' : '-',
				box->user_concealed ? 'U' : '-',
				box->concealed ? 'C' : '-');

	depth += 2;

	uint32_t mask = 0;
	uint32_t list[7];
	uint8_t i = 0;

	bool const content_changed = box->content_changed;
	bool const position_changed = box->position_changed;
	/* because of relative position values, layout change must be
	 * propagated so children will be repositioned correctly */
	bool const focus_changed = box->focus_changed;
	bool const layout_changed = box->layout_changed;
	bool const label_changed = box->label_changed || position_changed;
	bool should_map = false;

	box->layout_changed = false;
	box->content_changed = false;
	box->position_changed = false;
	box->focus_changed = false;
#if 0
	box->title_changed = false;
#endif
	box->label_changed = false;

	if (position_changed) {
		mask |=
			XCB_CONFIG_WINDOW_X |
			XCB_CONFIG_WINDOW_Y;
		list[i++] = box->x;
		list[i++] = box->y;
	}

	if ((layout_changed || position_changed || focus_changed) && box->parent) {
		if (0 < box->width) {
			mask |=
				XCB_CONFIG_WINDOW_WIDTH |
				XCB_CONFIG_WINDOW_HEIGHT;
			list[i++] = box->width;
			list[i++] = box->height;

			if (0 == box->num_children && box->parent && box->parent->parent)
				goto not_a_container;

			uint32_t total_weight = 0;
			uint16_t tiles = 0;

			for (uint16_t i = 0; i < box->num_children; ++i) {
				Box const *const child = box->children[i];
				if (!box_is_tiled(child))
					continue;

				total_weight += child->weight;
				++tiles;
			}

			uint16_t num_columns = box_compute_num_columns(box, tiles);
			if (num_columns < 1)
				num_columns = 1;

			uint16_t const num_rows = (tiles + num_columns - 1) / num_columns;
			uint16_t row = 0, column = 0;
			int16_t y = 0, x = 0;
			uint16_t height = box->height / (!num_rows ? 1 : num_rows);

			printf("%*.srearrange: children=%d tiles=%d cols=%d\n",
					depth, "", box->num_children, tiles, num_columns);
			uint16_t width = box->width / num_columns;
			for (uint16_t i = 0; i < box->num_children; ++i) {
				Box *const child = box->children[i];

				if (box_is_tiled(child)) {
					struct {
						uint16_t top;
						uint16_t right;
						uint16_t bottom;
						uint16_t left;
					} gap;

					gap.left = box->focus_lock
						? 0
						: box_is_container(child)
						? CONTAINER_GAP : WINDOW_GAP;
					gap.right = gap.left / 2;
					gap.left -= gap.right;

					gap.top = gap.left;
					gap.bottom = gap.right;

					if (0 == column)
						gap.left = 0;
					if (num_columns == column + 1)
						gap.right = 0;

					if (0 == row)
						gap.top = 0;
					if (num_rows == row + 1)
						gap.bottom = 0;

					/* per window width/height:
					 * - last children in column or row eats up all space,
					 * - size is rounded to the nearest mod_{x,y}
					 */
					uint16_t const tile_width =
						column + 1 == num_columns
						? box->width - x
						: width + (1 == num_rows && 1 < tiles && 0 < child->mod_x ? child->mod_x / 2 - (width + child->mod_x / 2) % child->mod_x : 0);
					uint16_t const tile_height =
						1 == tiles
						? box->height - y
						: height + (1 == num_columns && 1 < tiles && 0 < child->mod_y ? child->mod_y / 2 - (height + child->mod_y / 2) % child->mod_y : 0);
					box_set_position(child, box->x + x + gap.left, box->y + y + gap.top);
					box_set_size(child, tile_width - (gap.left + gap.right), tile_height - (gap.top + gap.bottom));

					--tiles;
					if (++column < num_columns) {
						x += tile_width;
					} else {
						++row, column = 0;
						y += tile_height, x = 0;

						if (0 < tiles && tiles < num_columns) {
							num_columns = tiles;
							width = box->width / num_columns;
						}
					}
				} else if (!box_is_visible(child)) {
					box_set_size(child, 0, 0);
				}

				box_update(child);
			}
		not_a_container:;
			should_map = layout_changed;
		} else {
			box_delete_labels(box);
			if (box->window != XCB_WINDOW_NONE) {
				printf("%*.s unmap\n", depth, "");
				xcb_icccm_set_wm_state(box->window, XCB_ICCCM_WM_STATE_ICONIC);
				DEBUG_CHECK(xcb_unmap_window, conn, box->frame);
			}

			for (uint16_t i = 0; i < box->num_children; ++i) {
				Box *const child = box->children[i];
				box_set_size(child, 0, 0);
				box_update(child);
			}

			goto out;
		}
	} else if (content_changed) {
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box *const child = box->children[i];
			box_update(child);
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

#endif

	if (!box->width)
		goto out;

	if (box->window != XCB_WINDOW_NONE && !box_is_container(box)) {
		if (0 < i) {
			mask |= XCB_CONFIG_WINDOW_STACK_MODE;
			list[i++] = XCB_STACK_MODE_BELOW;
			printf("%*.s configure n=%d\n", depth, "", i);
			DEBUG_CHECK(xcb_configure_window, conn, box->frame, mask, list);

			if ((XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT) & mask) {
				mask = XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT;
				list[0] = box->width;
				list[1] = box->height;
				DEBUG_CHECK(xcb_configure_window, conn, box->window, mask, list);
			}
		}

		/* map only after configure */
		if (should_map) {
			printf("%*.s map\n", depth, "");
			xcb_icccm_set_wm_state(box->window, XCB_ICCCM_WM_STATE_NORMAL);
			CHECK(xcb_map_window, conn, box->frame);
		}

		/* xcb_configure_window(conn, box->window, XCB_CONFIG_WINDOW_BORDER_WIDTH, &(uint32_t const){ 4 }); */

		if (box_is_focused(box) && !box_is_container(box) && focus_changed) {
			/* hand focus can only be updated after window is mapped */
			for_each_hand {
				Box const *const focus = hand->input_focus;
				if (box == focus) {
					if (focus_changed)
						hand_focus(hand);
					else
						hand_refocus(hand);
				}
			}
		}
	}

	if ((box->window != XCB_WINDOW_NONE ? should_map : layout_changed || position_changed || focus_changed) || label_changed)
		box_update_label(box);

out:
	depth -= 2;

#ifndef HEAWM_DEBUG_UPDATE
#undef printf
#endif
}

static void
ewmh_client_list_append(Box const *const box)
{
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_APPEND,
			bodies[box->body].screen->root, ATOM(_NET_CLIENT_LIST),
			XCB_ATOM_WINDOW, 32,
			1, &box->window);
}

static void
ewmh_client_list_clear(Body const *const body)
{
	DEBUG_CHECK(xcb_delete_property, conn, body->screen->root, ATOM(_NET_CLIENT_LIST));
}

static void
ewmh_client_list_update(Body const *const body)
{
	static size_t max_num_windows = 0;

	if (!ewmh_client_list_changed)
		return;

	xcb_window_t windows[1 << 16];
	uint32_t num_windows = 0;
	xcb_prop_mode_t mode = XCB_PROP_MODE_REPLACE;

	if (!max_num_windows) {
		max_num_windows = xcb_get_maximum_request_length(conn) - sizeof(xcb_change_property_request_t) / sizeof(uint32_t);
		if (ARRAY_SIZE(windows) < max_num_windows)
			max_num_windows = ARRAY_SIZE(windows);
	}

	Box *box;
	for_each_box(box, root, root)
		if ((body - bodies) == box->body && !box_is_container(box)) {
			windows[num_windows++] = box->window;
			if (max_num_windows == num_windows) {
				DEBUG_CHECK(xcb_change_property, conn, mode,
						body->screen->root, ATOM(_NET_CLIENT_LIST),
						XCB_ATOM_WINDOW, 32,
						num_windows, windows);
				num_windows = 0;
				mode = XCB_PROP_MODE_APPEND;
			}
		}

	if (0 < num_windows)
		DEBUG_CHECK(xcb_change_property, conn, mode,
				body->screen->root, ATOM(_NET_CLIENT_LIST),
				XCB_ATOM_WINDOW, 32,
				num_windows, windows);

	ewmh_client_list_changed = false;
}

static void
ewmh_update(Body const *const body)
{
	ewmh_client_list_update(body);
}

static void
do_update(void)
{
	do
		box_update(root);
	while (root->content_changed);

	for_each_body {
		assert(body->num_labels_used <= body->num_labels_mapped);
		while (body->num_labels_used < body->num_labels_mapped) {
			Label *label = &body->labels[--body->num_labels_mapped];
			hide_label(label);
		}

		ewmh_update(body);
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

static uint16_t
box_unparent(Box *const box)
{
	Box *parent = box->parent;
	if (!parent)
		return 0;

	Box **child = &parent->children[0];
	while (box != *child)
		++child;

	/* pos of box in its parent */
	uint16_t const pos = child - parent->children;

	memmove(
		child,
		child + 1,
		(--parent->num_children - pos) * sizeof *child
	);

	box_update_focus_seq(parent);
	box_propagate_change(parent)->layout_changed = true;
	box->parent = NULL;

	return pos;
}

static void box_vacuum(Box *box);

static void
hand_focus_box(Hand *hand, Box *box);

/* fill *boxes with the n latest focused box associated with hand */
static void
hand_find_recents(Hand *hand, Box *root, uint32_t const focus_seq, Box **const boxes, uint32_t const n)
{
	static Box const EMPTY_BOX;

	/* use a dummy box so we do not have to check for NULL */
	for (uint32_t i = 0; i < n; ++i)
		/* cast is safe because we do not write boxes and they
		 * will be NULLed out at the end */
		boxes[i] = (Box *)&EMPTY_BOX;

	Box *box;
	for_each_box(box, root, root)
		if (/* is it a client window? */
		    !box_is_container(box) &&
		    /* if not find a box that... */
		    (NULL_HAND == box->focus_hand || (
		     /* is not focused by any hand; so we do not interfere */
		     focus_seq != box->focus_seq &&
		     /* but was focused by the current hand; it's in the history */
		     hand - hands == box->focus_hand) ||
		     box == hand->input_focus))
		{
			uint32_t i = n;
			while (i > 0 && boxes[i - 1]->focus_seq <= box->focus_seq)
				--i;
			if (i < n) {
				memmove(&boxes[i], &boxes[i + 1], (n - i - 1) * sizeof *boxes);
				boxes[i] = box;
			}
		}

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
		hand_find_recents(hand, root, focus_seq, &optimum, 1);

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
#if 1
	Box *box;
	for_each_box(box, root, root) {
		box->label_changed = true;
		box->content_changed = true;
	}
#else
	switch (hand->mode, mode_move_a) {
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
		for_each_box(box, root, root) {
			box->label_changed = true;
			box->content_changed = true;
		}
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
#endif

	hand->num_labels = 0;

	uint64_t const hand_mask = ~(1 << (hand - hands));
	for_each_body {
		for (uint32_t j = body->num_labels_used; 0 < j;) {
			Label *const label = &body->labels[--j];
			if ((label->hands &= hand_mask))
				continue;

			Label const tmp = *label;
			*label = body->labels[--body->num_labels_used];
			body->labels[body->num_labels_used] = tmp;
		}
	}
}

static void
hand_input_reset(Hand *const hand)
{
	memset(hand->user_input, '\0', sizeof hand->user_input);
}

static void
hand_leave_mode(Hand *const hand)
{
	hand_do_mode_changes(hand);
	hand_input_reset(hand);

	switch (hand->mode) {
	case HAND_MODE_MOVE:
		hand_focus_box(hand, hand->mode_box);
		break;

	default:
		/* noop */
		break;
	}

	hand->mode = HAND_MODE_DEFAULT;
	hand->mode_box = NULL;
}

static void
box_free(Box *const box)
{
#if 0
	free(box->title);
#endif
	free(box->class);
#ifndef HEAWM_NDEBUG
	memset(box, 0xcc, sizeof *box);
#endif
	free(box);
}

static void
box_delete(Box *box)
{
	/* if we removed the currently selected box, and there is only one
	 * hand, root->focus_seq will dropped that could make a previously
	 * selected seems like it's selected (possibly by someone other) */
	uint32_t const real_focus_seq = root->focus_seq;
	bool focus_changed = false;

	assert(!box->num_children);

	for_each_hand {
		if (box == hand->mode_box)
			hand_leave_mode(hand);

		/* forget current focus */
		if (box == hand->focus) {
			focus_changed = true;
			hand->focus = NULL;
		}

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
	}

	box_delete_labels(box);

	if (focus_changed)
		increase_focus_seq();

	Box *const box_parent = box->parent;
	box_unparent(box);

	if (focus_changed)
		focus_all_hands(real_focus_seq);

	if (!box_is_container(box)) {
		DEBUG_CHECK(xcb_shape_select_input, conn, box->window, false);
		DEBUG_CHECK(xcb_change_window_attributes, conn, box->window,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					XCB_NONE
				});
		xcb_icccm_set_wm_state(box->window, XCB_ICCCM_WM_STATE_WITHDRAWN);
		DEBUG_CHECK(xcb_reparent_window, conn, box->window, bodies[box->body].screen->root, 0, 0);
		DEBUG_CHECK(xcb_destroy_window, conn, box->frame);
		DEBUG_CHECK(xcb_change_save_set, conn, XCB_SET_MODE_DELETE, box->window);
		ewmh_client_list_changed = true;
	}

	box_vacuum(box_parent);
	box_free(box);
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
box_reparent(Box *into, uint16_t const pos, Box *box);

static void
box_vacuum(Box *const box)
{
	assert(box_is_container(box) && "for containers only");

	if (box == root ||
	    box_is_monitor(box))
		goto out;

	/* substitute box with its only children */
	if (1 == box->num_children && box_is_container(box->children[0])) {
		/* keep name only */
		memcpy(box->children[0]->name, box->name, sizeof box->name);
		box->children[0]->user_concealed = box->user_concealed;
		box->children[0]->concealed = box->concealed;
		box->children[0]->hide_label |= box->hide_label;
		box_reparent(box->parent, box_get_pos(box), box->children[0]);
		return;
	}

	if (0 < box->num_children)
		goto out;

	box_delete(box);

	return;

out:
	/* called after box lost a child so it changed */
	box_propagate_change(box)->layout_changed = true;
}

static void
box_realloc(Box **const box, size_t const new_size)
{
	Box *const old = *box;
	Box *const new = realloc(old, new_size);

	if (old == new)
		return;

	*box = new;

#define UPDATE_REF(var) do if (old == (var)) var = new; while (0)

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
		assert(old == root && "unattached box has been resized");
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
}

static void
box_reparent(Box *into, uint16_t pos, Box *box)
{
	assert(box_is_container(into));
	assert(root != box && "root cannot be reparented");

	Box *const old_parent = box->parent;

	uint16_t const box_pos = box_unparent(box);
	if (old_parent == into)
		pos -= box_pos < pos;

	box_realloc(&into, offsetof(Box, children[into->num_children + 1]));
	box->parent = into;

	/* inherit some properties from parent */
	if (into->parent) {
		box->focus_lock = into->parent->focus_lock;
		box->body = into->parent->body;
	}

	assert(pos <= into->num_children);

	memmove(
		into->children + pos + 1,
		into->children + pos,
		(into->num_children++ - pos) * sizeof *into->children
	);
	into->children[pos] = box;

	if (old_parent)
		box_vacuum(old_parent);

	box_update_focus_seq(into);
	box_propagate_change(into)->layout_changed = true;

	box_name(box);

	/* auto properties: conceal neck */
	if (into->parent && box_is_monitor(into))
		box->concealed = true;
}

static Box *
box_new(void)
{
	Box *box = calloc(1, sizeof *box + num_hands * sizeof(BoxPointer));
	box->focus_hand = NULL_HAND;
	/* an initially zeroed out box->{x,y} does not mean box->window is
	 * really at 0,0 and without it no reconfiguration would happen */
	box->position_changed = true;
	return box;
}

static bool
box_is_descendant(Box const *const base, Box const *box)
{
	do
		if (base == box)
			return true;
	while ((box = box->parent));

	return false;
}

static void
box_reparent_checked(Box *const into, uint32_t const pos, Box *box)
{
	if (box_is_descendant(box, into) ||
	    root == into)
		return;

	/* insert an intermediate box under monitor */
	if (box_is_monitor(into) && !box_is_container(box)) {
		Box *const container = box_new();
		box_reparent(into, pos, container);
		box_reparent(container, 0, box);
		return;
	}

	box_reparent(into, pos, box);
}

static void
box_set_focus_lock(Box *const box, bool const enable)
{
	if (enable != box->focus_lock) {
		box->focus_lock = enable;
		box->layout_changed = true;
	}
}

static void
increase_focus_seq(void)
{
	uint32_t const old_focus_seq = root->focus_seq;
	++root->focus_seq;

	for_each_hand {
		Box *box;

		if (!(box = hand->input_focus) &&
		    !(box = hand->focus))
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
box_swap(Box *const x, Box *const y)
{
	if (!x || !y || x == y)
		return;

	Box **px, **py;

	for (px = x->parent->children; *px != x; ++px);
	for (py = y->parent->children; *py != y; ++py);

	Box *const y__parent = y->parent;
	(*px = y)->parent = x->parent;
	(*py = x)->parent = y__parent;

	box_propagate_change(x->parent)->layout_changed = true;
	box_propagate_change(y->parent)->layout_changed = true;

	box_update_focus_seq(x->parent);
	box_update_focus_seq(y->parent);
}

static void
box_swap_checked(Box *const x, Box *const y)
{
	if (box_is_descendant(x, y) ||
	    box_is_descendant(y, x))
		return;

	if (box_is_monitor(x) ||
	    box_is_monitor(y))
		return;

	box_swap(x, y);
}

static void
hand_grab_keyboard(Hand const *const hand)
{
	for_each_body {
		xcb_window_t const root_window = body->screen->root;

		if (HAND_MODE_DEFAULT == hand->mode) {
			XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(root_window,
					hand->master_keyboard,
					XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
					{ XCB_INPUT_MODIFIER_MASK_ANY });

			XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
					hand->master_keyboard,
					XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
					XCB_INPUT_GRAB_MODE_22_SYNC,
					XCB_INPUT_GRAB_OWNER_NO_OWNER,
					XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
					{
						XCB_MOD_MASK_4                     | 0,
						XCB_MOD_MASK_4                     | XCB_MOD_MASK_SHIFT,
						XCB_MOD_MASK_4                     | XCB_MOD_MASK_CONTROL,
						XCB_MOD_MASK_4                     | XCB_MOD_MASK_1,

						/* uses effective modifiers so we have to specify them
						 * with their CapsLock variants too */
						XCB_MOD_MASK_4 | XCB_MOD_MASK_LOCK | 0,
						XCB_MOD_MASK_4 | XCB_MOD_MASK_LOCK | XCB_MOD_MASK_SHIFT,
						XCB_MOD_MASK_4 | XCB_MOD_MASK_LOCK | XCB_MOD_MASK_CONTROL,
						XCB_MOD_MASK_4 | XCB_MOD_MASK_LOCK | XCB_MOD_MASK_1,
					});

#if 0
			if (hand->check_input || !hand->want_focus) {
				xcb_keycode_t const *const keycodes = hand->check_input
					? (xcb_keycode_t const[]){ XCB_GRAB_ANY, XCB_NO_SYMBOL }
					: xcb_key_symbols_get_keycode(symbols, XKB_KEY_Return);

				if (keycodes) {
					xcb_keycode_t const *keycode = keycodes;

					do {
						XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
								XCB_INPUT_DEVICE_ALL_MASTER,
								XCB_INPUT_GRAB_TYPE_KEYCODE, *keycode,
								XCB_INPUT_GRAB_MODE_22_ASYNC,
								/* process event only if client window would receive it */
								XCB_INPUT_GRAB_OWNER_OWNER,
								XCB_INPUT_XI_EVENT_MASK_KEY_PRESS |
								XCB_INPUT_XI_EVENT_MASK_KEY_RELEASE,
								NORMAL_GRAB_MASKS);
					} while (XCB_NO_SYMBOL != *++keycode);

					if (!hand->check_input)
						free((void *)keycodes);
				}
			}
#endif
		} else {
			XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
					hand->master_keyboard,
					XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
					XCB_INPUT_GRAB_MODE_22_SYNC,
					XCB_INPUT_GRAB_OWNER_NO_OWNER,
					XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
					{ XCB_INPUT_MODIFIER_MASK_ANY });
		}
	}
}

static void
hand_assign_latest_input(Hand *const hand)
{
	if (hand->input_focus && hand->focus) {
		hand->input_focus->close_by_force = false;

		if (hand->focus != hand->input_focus) {
			printf("focus := input_focus\n");
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
hand_focus_box_internal(Hand *const hand, Box *const box)
{
	if (hand->input_focus)
		box_save_pointer(hand->input_focus, hand);

	if (hand->input_focus)
		box_propagate_change(hand->input_focus)->label_changed = true;
	else if (hand->focus)
		box_propagate_change(hand->focus)->label_changed = true;
	box_propagate_change(box)->label_changed = true;

	if (hand->focus)
		hand->focus->focus_changed = true;
	if (hand->input_focus)
		hand->input_focus->focus_changed = true;

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
	hand_find_recents(hand, locked, root->focus_seq, recents, ARRAY_SIZE(recents));
	if (locked != box && recents[0] != box)
		box_swap(recents[1], recents[0]);

	hand->focus = box;

	Box *const new_input_focus = !box_is_container(box)
		? box
		: recents[0];

	/* avoid moving pointer if it would be unintuitive */
	if (hand->input_focus &&
	    new_input_focus &&
	    (/* window under pointer would be focused */
	     new_input_focus->window == Box_pointers(hand->input_focus)[hand - hands].window ||
	     /* mouse would be warped in the same window */
	     Box_pointers(new_input_focus)[hand - hands].window == Box_pointers(hand->input_focus)[hand - hands].window))
		memcpy(&Box_pointers(new_input_focus)[hand - hands],
		       &Box_pointers(hand->input_focus)[hand - hands],
		       sizeof(BoxPointer));

	hand->input_focus = new_input_focus;

	uint32_t const old_focus_seq = root->focus_seq;

	increase_focus_seq();

	if (locked != box) {
		box_swap(box, recents[1]);
		/* if there was no such box, that's not a problem: |box| will
		 * be that beginning from now */
	}

	hand->check_input = true;
	hand_grab_keyboard(hand);

	printf("focus=%.*s; input=%.*s\n",
			(int)sizeof box->name, hand->focus ? hand->focus->name : NULL,
			(int)sizeof box->name, hand->input_focus ? hand->input_focus->name : NULL);
}

static void
hand_focus_box(Hand *const hand, Box *const box)
{
	hand_assign_latest_input(hand);

	Box *neck = box;
	if (!box_is_monitor(neck) && 1 < root->num_children) {
		while (!box_is_monitor(neck->parent))
			neck = neck->parent;

		Box *const box_head = box_get_head(box);

		for (uint16_t i = 0; i < root->num_children; ++i) {
			Box *const head = root->children[i];

			if (box_head == head)
				continue;

			for (uint16_t j = 0; j < head->num_children; ++j) {
				Box *const head_neck = head->children[j];
				if (!memcmp(neck->name, head_neck->name, sizeof neck->name)) {
					if (!(head_neck->focus_seq && head_neck->focus_seq == head->focus_seq))
						hand_focus_box_internal(hand, head_neck);
					break;
				}
			}
		}
	}

	hand_focus_box_internal(hand, box);
}

static Body *
body_get_by_root(xcb_window_t const root_window)
{
	Body *body = bodies;
	while (root_window != body->screen->root)
		++body;
	return body;
}

static void
box_set_size_hints(Box *const box, xcb_get_property_reply_t const *const reply)
{
	xcb_size_hints_t const *const hints = xcb_get_property_value(reply);

	if (sizeof *hints != xcb_get_property_value_length(reply))
		return;

	if (XCB_ICCCM_SIZE_HINT_P_RESIZE_INC & hints->flags) {
		box->mod_y = hints->height_inc;
		box->mod_x = hints->width_inc;
	}
}

static void
box_set_class(Box *const box, xcb_get_property_reply_t const *const reply)
{
	int const len = xcb_get_property_value_length(reply);
	char const *const class = xcb_get_property_value(reply);
	char const *delim_null;

	if (len < (int)XCB_STRING_MAX &&
	    (delim_null = memchr(class, '\0', len)) &&
	    class + len - 1 == memchr(delim_null + 1, '\0', len - ((delim_null + 1) - class)))
		if ((box->class = malloc(len)))
			memcpy(box->class, class, len);
}

/* manage window */
static Box *
box_window(xcb_window_t const root_window, xcb_window_t const window)
{
	Box *box = box_new();

	box->window = window;
	box->frame = xcb_generate_id(conn);

	/* DEBUG_CHECK(xcb_configure_window, conn, window, XCB_CONFIG_WINDOW_BORDER_WIDTH, &(uint32_t const){ 0 }); */

	xcb_get_property_cookie_t const cookies[] = {
		xcb_get_property(conn, 0, window, ATOM(_NET_WM_STATE),       XCB_ATOM_ATOM,       0, 1),
		xcb_get_property(conn, 0, window, XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW,     0, 1),
		xcb_get_property(conn, 0, window, ATOM(WM_CLIENT_LEADER),    XCB_ATOM_WINDOW,     0, 1),
		xcb_get_property(conn, 0, window, ATOM(WM_NORMAL_HINTS),     ATOM(WM_SIZE_HINTS), 0, 1),
		xcb_get_property(conn, 0, window, ATOM(_HEAWM_NAME),         XCB_ATOM_STRING,     0, membersizeof(Box, name)),
		xcb_get_property(conn, 0, window, XCB_ATOM_WM_CLASS,         XCB_ATOM_STRING,     0, XCB_STRING_MAX / sizeof(uint32_t)),
	};

	xcb_atom_t state = XCB_ATOM_NONE;
	xcb_window_t transient_for = XCB_WINDOW_NONE;

	for (size_t i = 0; i < ARRAY_SIZE(cookies); ++i) {
		xcb_get_property_reply_t *const reply = xcb_get_property_reply(conn, cookies[i], NULL);
		if (!reply)
			continue;

		int const len = xcb_get_property_value_length(reply);
		void const *const value = xcb_get_property_value(reply);

		switch (i) {
		case 0:
			if (sizeof state == len)
				state = *(xcb_atom_t const *)value;
			break;

		case 1:
			if (sizeof transient_for == len)
				transient_for = *(xcb_window_t const *)value;
			break;

		case 2:
			if (sizeof box->leader == len)
				box->leader = *(xcb_window_t const *)value;
			break;

		case 3:
			box_set_size_hints(box, value);
			break;

		case 4:
			memcpy(box->name, value, len);
			memset(box->name + len, 0, sizeof box->name - len);
			break;

		case 5:
			box_set_class(box, reply);
			break;

		case ARRAY_SIZE(cookies):
		default:
			abort();
		}

		free(reply);
	}

	Hand *box_hand = NULL;
	Box *parent = NULL;
	uint32_t pos;
	bool focus = false;

	if (0 < num_hands) {
		Box *origin = NULL;
		(origin = find_box_by_window(root, root, offsetof(Box, window), transient_for)) ||
		(origin = find_box_by_window(root, root, offsetof(Box, leader), box->leader));

		/* find the hand that could possibly create this window */
		if (origin) {
			/* maybe a close dialog, maybe something other */
			origin->close_by_force = false;

			if (box_is_focused(origin)) {
				for_each_hand {
					if (origin != hand->input_focus)
						continue;

					if (hand->want_focus) {
						box_hand = hand;
						break;
					}

					if (!box_hand)
						box_hand = hand;
				}

				assert(box_hand && "box focused but no hands have it");
				focus = true;
			} else if (NULL_HAND != origin->focus_hand) {
				box_hand = &hands[origin->focus_hand];
				focus = false;
			} else {
				origin = NULL;
				goto unknown_hand;
			}
			pos = box_get_pos(origin) + 1;
			parent = origin->parent;
		} else {
		unknown_hand:;
			/* we use first hand even if its auto_focus is off */
			box_hand = &hands[0];
			for (uint8_t i = 0; i < num_hands; ++i) {
				if (hands[i].want_focus) {
					box_hand = &hands[i];
					break;
				}
			}
			focus = box_hand->want_focus;
			parent = box_hand->focus;

			if (parent) {
				if (box_is_monitor(parent)) {
					pos = parent->num_children;
				} else {
					pos = box_get_pos(parent) + 1;
					parent = parent->parent;

					if (parent && !parent->parent)
						parent = NULL;
				}
			}
		}
	}

	if (!parent) {
		assert(root->num_children && "no monitors");

		/* attach to the first (probably primary) monitor of body */
		Body *body = body_get_by_root(root_window);
		Box **head = root->children;
		while ((body - bodies) != (*head)->body)
			++head;

		parent = *head;
		while (parent->num_children && box_is_container(parent->children[0]))
			parent = parent->children[0];

		/* place at the end */
		pos = parent->num_children;
	}

	box_reparent_checked(parent, pos, box);
	if (box_hand) {
		box_hand->want_focus = false;
		if (focus && HAND_MODE_DEFAULT == box_hand->mode)
			hand_focus_box(box_hand, box);
	}

	Body *const body = &bodies[parent->body];
	DEBUG_CHECK(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			box->frame,
			body->screen->root,
			-1, -1, 1, 1, /* rect */
			0, /* border */
			XCB_WINDOW_CLASS_INPUT_OUTPUT,
			XCB_COPY_FROM_PARENT,
			XCB_CW_OVERRIDE_REDIRECT |
			XCB_CW_EVENT_MASK,
			(uint32_t const[]){
				true,
				FRAME_WINDOW_EVENT_MASK
			});

	DEBUG_CHECK(xcb_change_save_set, conn, XCB_SET_MODE_INSERT, window);

	if (CHECK(xcb_reparent_window, conn, window, box->frame, 0, 0))
		goto fail;

	if (CHECK(xcb_map_window, conn, window))
		goto fail;

	if (CHECK(xcb_change_window_attributes, conn, window,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					CLIENT_WINDOW_EVENT_MASK
				}))
		goto fail;

	DEBUG_CHECK(xcb_input_xi_select_events, conn, window, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
					XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
					XCB_INPUT_XI_EVENT_MASK_ENTER));

	DEBUG_CHECK(xcb_shape_select_input, conn, window, true);

	ewmh_client_list_append(box);

	return box;

fail:
	box_delete(box);
	return NULL;
}

static void
generate_coredump(void)
{
	if (!fork())
		abort();
}

static void
handle_signal_segfault(int signum)
{
	(void)signum;

	generate_coredump();
	restart();
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

#if 0
	sa.sa_handler = handle_signal_segfault;
	sigaction(SIGSEGV, &sa, NULL);
#endif

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
	xcb_prefetch_extension_data(conn, &xcb_xfixes_id);
	xcb_prefetch_extension_data(conn, &xcb_big_requests_id);

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
	else
		shape_base_event = ext->first_event;

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

	ext = xcb_get_extension_data(conn, &xcb_xfixes_id);
	if (!ext->present) {
		fprintf(stderr, "XFixes extension missing."
				" Pointer barricading will not work.\n");
	} else {
		GET_REPLY(reply, xcb_xfixes_query_version, conn,
				XCB_XFIXES_MAJOR_VERSION, XCB_XFIXES_MINOR_VERSION);
		if (!reply || !(XCB_XFIXES_MAJOR_VERSION == reply->major_version &&
		                XCB_XFIXES_MINOR_VERSION == reply->minor_version))
		{
			fprintf(stderr, "Requested XFixes version %" PRIu32 ".%" PRIu32 " not supported by server; got %" PRIu32 ".%" PRIu32 "."
					" Pointer barricading may not work.\n",
					XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION,
					reply ? reply->major_version : 0, reply ? reply->minor_version : 0);
		}

		free(reply);
	}

	xcb_prefetch_maximum_request_length(conn);
}

static char const *
xcb_connection_strerror(int const error)
{
	switch (error) {
	case XCB_CONN_ERROR:                   return "Stream error";
	case XCB_CONN_CLOSED_EXT_NOTSUPPORTED: return "Extension not supported";
	case XCB_CONN_CLOSED_MEM_INSUFFICIENT: return "Cannot allocate memory";
	case XCB_CONN_CLOSED_REQ_LEN_EXCEED:   return "Maximum request length exceeded";
	case XCB_CONN_CLOSED_PARSE_ERR:        return "Malformed display string";
	default:                               return "Unknown error";
	}
}

static void
connect_display(void)
{
	static char const DEFAULT_DISPLAY[] = ":0";

	for (int error; (error = xcb_connection_has_error((conn = xcb_connect(NULL, &default_screen))));) {
		char const *const display = getenv("DISPLAY");

		if (display) {
			fprintf(stderr, "Could not open display %s: %s\n",
					display, xcb_connection_strerror(error));
		} else {
			fprintf(stderr, "DISPLAY is not set; default to %s.\n",
					DEFAULT_DISPLAY);

			if (!setenv("DISPLAY", DEFAULT_DISPLAY, false))
				continue;
		}

		exit(EXIT_FAILURE);
	}

	init_atoms();

	root = box_new();

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
body_setup_windows(Body *const body)
{
	xcb_screen_t const *const screen = body->screen;
	xcb_query_tree_reply_t *const reply =
		xcb_query_tree_reply(conn,
				xcb_query_tree_unchecked(conn, screen->root),
				NULL);
	if (!reply)
		return;

	assert(!xcb_connection_has_error(conn));

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

		assert(!xcb_connection_has_error(conn));

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
body_setup_net_name(Body const *const body, char const *const name)
{
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->net_window, ATOM(_NET_WM_NAME),
			ATOM(UTF8_STRING), 8,
			strlen(name), name);
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->net_window, ATOM(_NET_SUPPORTING_WM_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &body->net_window);
	DEBUG_CHECK(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->screen->root, ATOM(_NET_SUPPORTING_WM_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &body->net_window);
}

static void
body_setup_ewmh(Body const *const body)
{
	ewmh_client_list_clear(body);
}

static void
body_setup_net(Body *const body)
{
	body->net_window = xcb_generate_id(conn);
	DEBUG_CHECK(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			body->net_window,
			body->screen->root,
			-1, -1, 1, 1, /* rect */
			0, /* border */
			XCB_WINDOW_CLASS_INPUT_ONLY,
			XCB_COPY_FROM_PARENT,
			XCB_CW_OVERRIDE_REDIRECT,
			(uint32_t const[]){
				true
			});

	body_setup_net_name(body, "heawm");
}

static void
body_update_heads(Body *const body);

static void
body_setup_heads(Body *const body)
{
	DEBUG_CHECK(xcb_randr_select_input, conn, body->screen->root,
			XCB_RANDR_NOTIFY_MASK_SCREEN_CHANGE);

	body_update_heads(body);
}

static void
body_setup_hands(Body *const body);

static void
body_setup(Body *const body)
{
	for (int error; (error =
		CHECK(xcb_change_window_attributes, conn, body->screen->root,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					CLIENT_WINDOW_EVENT_MASK |
					XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
				}));)
	{
		fprintf(stderr, "Unable to manage windows on screen %d: %s\n",
				body->screen_index,
				XCB_ACCESS == error
					? "Window manager is already running"
					: "Unknown error");
		/* it is a fatal error because we have input devices that we do
		 * not know if that WM handles or not that will surely cause
		 * conflict */
		/* also: it can crashes X */
		exit(EXIT_FAILURE);
	}

	body_setup_cursor(body, "default");
	body_setup_heads(body);
	body_setup_hands(body);
	body_setup_net(body);
	body_setup_ewmh(body);
	body_setup_windows(body);
	assert(!xcb_connection_has_error(conn));
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
body_sort_heads(Body const *const body)
{
	uint8_t const body_index = body - bodies;

	for (uint16_t i = 1; i < root->num_children; ++i) {
		Box *const this_head = root->children[i];

		uint16_t j = i;
		for (; 0 < j; --j) {
			Box *const head = root->children[j - 1];
			if (this_head->y < head->y &&
			    this_head->x < head->x)
				break;
		}
		memmove(&root->children[j + 1], &root->children[j], (i - j) * sizeof *root->children);
		root->children[j] = this_head;
	}
}

static Box *
body_find_head_by_name(Body *const body, char const *const name)
{
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (body - bodies != head->body)
			continue;

		if (name == head->class ||
		    (name && head->class && !strcmp(head->class + sizeof MONITOR_CLASS, name + sizeof MONITOR_CLASS)))
			return head;
	}

	return NULL;
}

static void
body_set_display(Body const *const body)
{
	char *host;
	int display;
	int screen;

	if (!xcb_parse_display(NULL, &host, &display, &screen))
		return;

	char display_screen[1 + 20 + 1 + 20 + 1];
	size_t const host_size = strlen(host);

	char *p;
	if ((p = realloc(host, host_size + sizeof display_screen))) {
		host = p;
		sprintf(p + host_size, ":%d.%d", display, body->screen_index) + 1;

		setenv("DISPLAY", host, true);
	}
	free(host);
}

static void
box_reset_name(Box *const box)
{
	memset(box->name, 0, sizeof box->name);
}

static int
body_head_cmp(void const *const p, void const *const q)
{
	Box const *const x = *(Box **)p;
	Box const *const y = *(Box **)q;

	if (x->body != y->body)
		return (int)x->body - (int)y->body;

	/* x is less then y if x is placed in the left-top quadrant relative to y */
	return x->x <= y->x && x->y <= y->y ? -1 : 1;
}

static void
body_update_heads(Body *const body)
{
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if ((body - bodies) == head->body)
			*head->name = EXTREMAL_NAME_CHAR;
	}

	Box *parent = NULL;

	GET_REPLY(monitors, xcb_randr_get_monitors, conn, body->screen->root, true);
	if (!monitors) {
		fprintf(stderr, "Failed to query RandR monitors.\n");

	add_screen_head:;
		Box *const head = parent = box_new();
		head->body = body - bodies;
		head->class = NULL;
		box_reparent(root, 0, head);

		head->user_width = body->screen->width_in_millimeters;
		head->user_height = body->screen->height_in_millimeters;
		box_set_size(head, body->screen->width_in_pixels, body->screen->height_in_pixels);
		box_set_position(head, 0, 0);
	} else {
		int const num_monitors = xcb_randr_get_monitors_monitors_length(monitors);
		if (!num_monitors)
			goto add_screen_head;

		xcb_get_atom_name_cookie_t *const cookies = malloc(num_monitors * sizeof *cookies);

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

			char *name = NULL;
			xcb_get_atom_name_reply_t *const name_reply =
				xcb_get_atom_name_reply(conn, cookies[iter.rem - 1], NULL);
			if (name_reply) {
				int const len = xcb_get_atom_name_name_length(name_reply);
				name = malloc(sizeof MONITOR_CLASS + len + 1);
				memcpy(name, MONITOR_CLASS, sizeof MONITOR_CLASS);
				memcpy(name + sizeof MONITOR_CLASS, xcb_get_atom_name_name(name_reply), len);
				name[sizeof MONITOR_CLASS + len] = '\0';

				free(name_reply);
			}

			Box *head = body_find_head_by_name(body, name);
			if (!head) {
				head = box_new();
				head->body = body - bodies;
				head->class = name, name = NULL;
				box_reparent(root, monitor->primary ? 0 : root->num_children, head);
			}

			/* FIXME: swap values if rotated */
			head->user_width = monitor->width_in_millimeters;
			head->user_height = monitor->height_in_millimeters;
			box_set_size(head, monitor->width, monitor->height);
			box_set_position(head, monitor->x, monitor->y);

			/* primary monitors named before others to get
			 * stable names */
			if (monitor->primary) {
				*head->name = '\0';
				box_name(head);
			}

			if (!parent)
				parent = head;

			free(name);
		}

		free(cookies);
	}
	free(monitors);

	qsort(root->children, root->num_children, sizeof *root->children, body_head_cmp);

	for (uint16_t i = root->num_children; 0 < i;) {
		Box *const head = root->children[--i];

		if (EXTREMAL_NAME_CHAR != *head->name)
			continue;

		free(head->class), head->class = NULL;
		head->user_width = 0;
		head->user_height = 0;

		*head->name = '\0';
		box_reparent(parent, 0, head), parent = head->parent;
		box_vacuum(head);
	}

	/*MAN(HOOKS)
	 * .TP
	 * .B displaychange
	 * Run whenever display configuration changes, e.g. monitor
	 * connected/disconnected, resolution changed. See xrandr(1).
	 */
	BODY_SPAWN(body, heawm_file("displaychange"));
}

static void
body_setup_hands(Body *const body)
{
	DEBUG_CHECK(xcb_input_xi_select_events, conn, body->screen->root, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
					XCB_INPUT_XI_EVENT_MASK_HIERARCHY |
					XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
					XCB_INPUT_XI_EVENT_MASK_ENTER));
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
		Body *const body = &bodies[default_screen == i ? 0 : i + 1];

		body->screen_index = i;
		body->screen = screen;
		body->visual_type = lookup_visual_type(screen);

		body_setup(body);
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
debug_print_atom_name(char const *const name, xcb_atom_t const atom)
{
#ifndef HEAWM_NDEBUG
	xcb_get_atom_name_reply_t *const name_reply =
		xcb_get_atom_name_reply(conn, xcb_get_atom_name_unchecked(conn, atom), NULL);
	printf("%s %.*s\n", name, xcb_get_atom_name_name_length(name_reply), xcb_get_atom_name_name(name_reply));
#endif
}

#if 0
static void
box_set_title(Box *const box, xcb_get_property_reply_t const *const reply)
{
	int const size = xcb_get_property_value_length(reply);
	char *p;
	if (!(p = realloc(box->title, size + 1)))
		return;
	memcpy((box->title = p), xcb_get_property_value(reply), size);
	box->title[size] = '\0';
}
#endif

static void
box_flatten(Box *into, uint16_t pos, Box const *const box)
{
	assert(box_is_container(into));
	assert(box_is_container(box));

	uint16_t i = box->num_children;
	if (!i)
		return;

	Box *const last_child = box->children[i - 1];

	do {
		Box *const child = 1 < i ? box->children[0] : last_child;
		if (!child->user_concealed)
			child->concealed = false;

		/* can occur if box == into */
		if (into->num_children < pos)
			pos = into->num_children;
		box_reparent_checked(into, pos++, child), into = child->parent;

		/* last children will be vacuumed upwards automatically */
	} while (0 < --i);
}

/** create a new box that contains all related boxes at the place of |box| */
static bool
box_group(Box const *const box)
{
	uint16_t num_items = 0;
	uint16_t pos;
	bool ignore_leader = false;

	assert(!box_is_container(box));

retry:
	for (uint16_t i = 0; i < box->parent->num_children; ++i) {
		Box *const child = box->parent->children[i];
		child->iter = !child->user_concealed &&
			!ignore_leader && box->leader
				? box->leader == child->leader
				: box->class && child->class &&
				  !strcmp(box_get_class_instance(box), box_get_class_instance(child));
		num_items += child->iter;

		if (box == child)
			pos = i;
	}

	if (!(1 < num_items && num_items < box->parent->num_children)) {
		if (!ignore_leader && box->leader) {
			ignore_leader = true;
			goto retry;
		}

		return false;
	}

	{
		Box *const container = box_new();
		*container->name = EXTREMAL_NAME_CHAR;
		box_reparent(box->parent, pos, container);
	}

	Box *const parent = box->parent;

	for (uint16_t i = 0; i < parent->num_children;) {
		Box *const child = parent->children[i];

		if (!child->iter) {
			++i;
			continue;
		}

		Box *const new = parent->children[pos];
		box_reparent(new, new->num_children, child);
		if (i < pos)
			--pos;

		child->concealed = true;
	}

	{
		Box *const container = parent->children[pos];
		*container->name = '\0';
		box_name(container);
	}

	return true;
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
	Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->window); \
	if (!box) \
		goto out;

#if 0
	if (ATOM(_NET_WM_NAME) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, ATOM(UTF8_STRING), XCB_STRING_MAX / sizeof(uint32_t));

		box_set_title(box, reply);
	} else if (XCB_ATOM_WM_NAME == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_STRING, XCB_STRING_MAX / sizeof(uint32_t));

		box_set_title(box, reply);
	} else
#endif
	if (ATOM(WM_NORMAL_HINTS) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, ATOM(WM_SIZE_HINTS), sizeof(xcb_size_hints_t) / sizeof(uint32_t));

		box_set_size_hints(box, reply);
	} else if (ATOM(WM_CLIENT_LEADER) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_WINDOW, sizeof(xcb_window_t) / sizeof(uint32_t));

		box->leader = *(xcb_window_t *)xcb_get_property_value(reply);
	} else if (XCB_ATOM_WM_CLASS == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_STRING, XCB_STRING_MAX / sizeof(uint32_t));

		box_set_class(box, reply);
	} else if (ATOM(_HEAWM_NAME) == event->atom) {
		FIND_BOX;
		REQUEST_PROPERTY(box, XCB_ATOM_STRING, membersizeof(Box, name));

		int const len = xcb_get_property_value_length(reply);
		if ((int)membersizeof(Box, name) < len) {
			fprintf(stderr, "0x%x._HEAWM_NAME = \"%.*s\" is too long."
					" Maximum allowed size is %zu.\n",
					box->window,
					len, (char *)xcb_get_property_value(reply),
					membersizeof(Box, name));
			goto out;
		}

		memcpy(box->name, xcb_get_property_value(reply), len);
		if (len < (int)membersizeof(Box, name))
			box->name[len] = '\0';
	} else {
		/* debug_print_atom_name("property", event->atom); */
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

		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
				hand->master_pointer,
				XCB_INPUT_GRAB_TYPE_BUTTON, XCB_GRAB_ANY,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				XCB_INPUT_GRAB_OWNER_NO_OWNER,
				XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS,
				{ XCB_MOD_MASK_4 });
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

		Box *box = find_box_by_window(head, head, offsetof(Box, window), window);
		if (box)
			return box;
	}

	return NULL;
}

static void
handle_unmap_notify(xcb_unmap_notify_event_t const *const event)
{
	/* not because reparenting */
	if (event->event != event->window)
		return;

	/* printf("unmap notify %x event=%x, from_configure=%d\n", event->window, event->event, event->from_configure); */
	Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->window);
	if (box)
		box_delete(box);
}

static void
handle_map_request(xcb_map_request_event_t const *const event)
{
	/* printf("map request %x\n", event->window); */
	Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->window);
	if (!box)
		box_window(event->parent, event->window);
}

static void
handle_configure_notify(xcb_configure_notify_event_t const *const event)
{
	/* we are interested in root window size changes when XRandR is not
	 * available */
	if (randr_base_event)
		return;

	for_each_body {
		if (event->window != body->screen->root)
			continue;

		body_update_heads(body);

		return;
	}
}

static void
handle_configure_request(xcb_configure_request_event_t const *const event)
{
	Box *box;
	if ((box = find_box_by_window(root, root, offsetof(Box, window), event->window))) {
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
wm_close_window(xcb_window_t const window)
{
	DEBUG_CHECK(xcb_send_event, conn, false, window,
			XCB_EVENT_MASK_NO_EVENT/* client messages cannot be masked */,
			(char const *)&(xcb_client_message_event_t){
				.response_type = XCB_CLIENT_MESSAGE,
				.format = 32,
				.window = window,
				.type = ATOM(WM_PROTOCOLS),
				.data = {
					.data32 = {
						ATOM(WM_DELETE_WINDOW),
					}
				}
			});
}

static void
box_close(Box *const root)
{
	Box *box;
	for_each_box(box, root, root) {
		if (box_is_container(box))
			continue;

		if (!box->close_by_force) {
			box->close_by_force = true;
			wm_close_window(box->window);
		} else {
			DEBUG_CHECK(xcb_kill_client, conn, box->window);
		}

	}
}

static void
handle_client_message(xcb_client_message_event_t const *const event)
{
	if (ATOM(_NET_CLOSE_WINDOW) == event->type) {
		Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->window);
		if (box)
			box_close(box);
	} else if (ATOM(_NET_ACTIVE_WINDOW) == event->type) {
		if (2 != event->data.data32[0])
			return;

		Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->window);
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
	} else {
		debug_print_atom_name("client message", event->type);
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

#if 0
		for_each_body {
			DEBUG_CHECK(xcb_input_xi_select_events, conn, body->screen->root, 1,
					XI_EVENT_MASK(device->id,
							XCB_INPUT_XI_EVENT_MASK_KEY_PRESS |
							XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
							XCB_INPUT_XI_EVENT_MASK_ENTER));
		}
#endif

		++device;
	}
	assert(device - devices == num_devices);

	free(reply);

	/* update focus history */
	Box *box;
	for_each_box(box, root, root) {
		box->focus_hand = hand_map[box->focus_hand];

		if (!box_is_container(box)) {
			box_realloc(&box, sizeof(Box) + new_num_hands * sizeof(BoxPointer));

			BoxPointer old_pointers[MAX_NUM_HANDS];
			/* save pointers of box becase we may shuffle them */
			memcpy(old_pointers, Box_pointers(box), num_hands * sizeof(BoxPointer));
			/* reset all */
			memset(Box_pointers(box), 0, new_num_hands * sizeof(BoxPointer));

			for (uint8_t i = 0; i < num_hands; ++i)
				if (NULL_HAND != hand_map[i])
					Box_pointers(box)[hand_map[i]] = old_pointers[i];
		}
	}

	/* forget deattached hands' focus by incrementing focus number of
	 * hands alive */
	increase_focus_seq();

	free(hands), hands = new_hands;
	num_hands = new_num_hands;

	focus_all_hands(root->focus_seq);

	/*MAN(HOOKS)
	 * .TP
	 * .B inputchange
	 * Run whenever input devices change, e.g. keyboard plugged/unplugged,
	 * master device added. See xinput(1).
	 */
	SPAWN(heawm_file("inputchange"));
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
	uint8_t n = strnlen(hand->user_input, sizeof hand->user_input);
	if (0 == n)
		return false;
	else if (n < sizeof hand->user_input)
		++n;

	for_each_body {
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			Label *const label = &body->labels[j];
			if (memcmp(label->name, hand->user_input, n))
				continue;
			*out = label;
			return true;
		}
	}

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
#define KEY_MOD_MASK \
(	XCB_MOD_MASK_CONTROL \
|	XCB_MOD_MASK_1 \
|	XCB_MOD_MASK_2 \
|	XCB_MOD_MASK_3 \
|	XCB_MOD_MASK_4 \
|	XCB_MOD_MASK_5 \
)

static void
handle_input_key_release(xcb_input_key_press_event_t const *const event)
{
	Device *const device = get_device_by_id(event->sourceid);
	assert(device);
	Hand *hand = device->hand;

	printf("up\n");
#if 1
	xcb_test_fake_input(conn, XCB_INPUT_KEY_RELEASE, event->detail, 0,
			event->root, event->root_x, event->root_y,
			event->sourceid);
	hand_grab_keyboard(hand);
#endif
#if 0
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
#endif
}

static void
hand_handle_input_key_normal(xcb_input_key_press_event_t const *const event, Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
#if 0
	uint32_t const grab_detail = XKB_KEY_Return == sym ? event->detail : XCB_GRAB_ANY;
	/* one shot. but it is *required* before fake input, otherwise we will
	 * get repeadetly fired */

# if 1
	DEBUG_CHECK(xcb_input_xi_allow_events, conn, XCB_CURRENT_TIME, event->deviceid,
			XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
	DEBUG_CHECK(xcb_input_xi_allow_events, conn, XCB_CURRENT_TIME, event->deviceid,
			XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
	DEBUG_CHECK(xcb_input_xi_allow_events, conn, XCB_CURRENT_TIME, event->deviceid,
			XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
# endif
# if 1
	XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(event->root,
			0,
			XCB_INPUT_GRAB_TYPE_KEYCODE, grab_detail,
			{ XCB_INPUT_MODIFIER_MASK_ANY });
	XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(event->root,
			hand->master_keyboard,
			XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
			{ XCB_INPUT_MODIFIER_MASK_ANY });
#endif
#if 1
	CHECK(xcb_test_fake_input, conn, XCB_INPUT_KEY_RELEASE, event->detail, 0,
			event->root, event->root_x, event->root_y,
			event->sourceid);
	CHECK(xcb_test_fake_input, conn, XCB_INPUT_KEY_PRESS, event->detail, 0,
			event->root, event->root_x, event->root_y,
			event->sourceid);
	xcb_flush(conn);
#endif

	printf("down\n");

#endif

	if (repeating)
		return;

#if 0
	if (XKB_KEY_Return == sym) {
		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(bodies[0].screen->root,
				XCB_INPUT_DEVICE_ALL,
				XCB_INPUT_GRAB_TYPE_KEYCODE, event->detail,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				/* process event only if client window would receive it */
				XCB_INPUT_GRAB_OWNER_OWNER,
				XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
				NORMAL_GRAB_MASKS);
	}
#endif

	hand->want_focus |= XKB_KEY_Return == sym;
	hand->check_input = false;
	hand_assign_latest_input(hand);
}

static Box *
hand_get_latest_input(Hand const *const hand)
{
	return hand->latest_input[hand->focus == hand->latest_input[0]];
}

static void
hand_input_try_jump(Hand *const hand)
{
	Box *box;
	/* search is incomplete, waiting for further input */
	if (!find_box_by_name(&box, hand->user_input))
		return;

	assert(box != root);

	/* search finished with no results */
	if (!box) {
		char name[sizeof hand->user_input + 1];
		memcpy(name, hand->user_input, sizeof hand->user_input);
		name[sizeof name - 1] = '\0';

		/*MAN(HOOKS)
		 * .TP
		 * .B autostart
		 * Run whenever user has would like to jump to a non-existing
		 * label. Can be useful to automagically start programs.
		 */
		if (SPAWN(heawm_file("autostart"), name))
			hand->want_focus = true;
		goto reset_input;
	}

	/* if focus would not change, try focus an interesting window */
	if (box == hand->focus)
		box = hand_get_latest_input(hand);

	if (!box)
		goto reset_input;

	printf("jump %.*s\n", (int)sizeof box->name, box->name);
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

	uint8_t const input_len = strnlen(hand->user_input, sizeof hand->user_input);
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
	 * .BR Mod- { a-zA-Z }...
	 * Focus box. Run hook
	 * .BR autostart " \fIname\fR"
	 * if there is no such box.
	 */
	if (!repeating && hand_handle_input(hand, sym)) {
		hand_input_try_jump(hand);
	} else switch (sym) {
	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-Tab
	 * Focus box that previously received input.
	 */
	case XKB_KEY_Tab:
	{
		Box *const box = hand_get_latest_input(hand);
		if (box)
			hand_focus_box(hand, box);
	}
		break;
	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-)
	 * Open hook
	 * .B quickstart
	 * inside $\fBTERMINAL\fR.
	 */
	case XKB_KEY_parenright:
	{
		if (repeating)
			break;

		hand->want_focus = true;
		SPAWN(config.terminal, "-e", heawm_file("quickstart"));
	}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-]
	 * Open
	 * .BR alsamixer(1) .
	 */
	case XKB_KEY_bracketright:
		if (repeating)
			break;

		hand->want_focus = true;
		SPAWN(config.terminal, "-e", "alsamixer");
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-*
	 * Toggle system mute.
	 */
	case XKB_KEY_asterisk:
		if (repeating)
			break;

		SPAWN("amixer", "-q", "set", "Master", "toggle");
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod--
	 * Decrease system volume.
	 */
	case XKB_KEY_minus:
		SPAWN("amixer", "-q", "set", "Master", "3-");
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-+
	 * Increase system volume.
	 */
	case XKB_KEY_plus:
		SPAWN("amixer", "-q", "set", "Master", "1+", "unmute");
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-Return
	 * Open $\fBTERMINAL\fR.
	 */
	case XKB_KEY_Return:
		if (repeating)
			break;

		hand->want_focus = true;
		SPAWN(config.terminal);
		break;

	default:
		return true;
	}

	return false;
}

static void
box_change_columns(Box *const box, uint16_t const num_columns)
{
	assert(box_is_container(box));
	if (num_columns == box->num_columns)
		return;

	box->num_columns = num_columns;
	box_propagate_change(box)->layout_changed = true;
}

static void
box_explode(Box *const box, bool const vertical)
{
	for (uint8_t i = 0; i < 3; ++i) {
		Box *const split = box_new();
		*split->name = EXTREMAL_NAME_CHAR;
		box_reparent(box->parent, i, split);
	}

	Box *const parent = box->parent;

	size_t const pos_offset = vertical ? offsetof(Box, x)     : offsetof(Box, y);
	size_t const dim_offset = vertical ? offsetof(Box, width) : offsetof(Box, height);
#define L(box) (*(int16_t *)((uintptr_t)box + pos_offset))
#define U(box) (L(box) + *(uint16_t *)((uintptr_t)box + dim_offset))

	int16_t const lower = L(box);
	int16_t const upper = U(box);

	while (3 < parent->num_children) {
		Box *const child = parent->children[3];
		uint8_t const k = (lower <= L(child)) + (upper <= L(child));
		Box *const split = parent->children[k];
		box_reparent(split, split->num_children, child);
	}

#undef L
#undef U

	parent->children[0]->num_columns = parent->num_columns;
	parent->children[1]->num_columns = vertical ? 1 : UINT16_MAX;
	parent->children[2]->num_columns = parent->num_columns;
	parent->num_columns = vertical ? UINT16_MAX : 1;

	for (uint8_t i = 3; 0 < i;) {
		Box *const split = parent->children[--i];
		*split->name = '\0';
		box_name(split);
		box_vacuum(split);
	}
}

static Box *
box_clone(Box const *const box)
{
	bool const is_container = box_is_container(box);
	size_t const new_size = sizeof *box + (is_container
			? box->num_children * sizeof *box->children
			: num_hands * sizeof(BoxPointer));
	Box *const new = malloc(new_size);
	if (!new)
		return NULL;

	memcpy(new, box, is_container ? sizeof *box : new_size);
	new->parent = NULL;

	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box **const child = &new->children[i];
		if (!(*child = box_clone(box->children[i]))) {
			while (0 < i)
				box_free(new->children[--i]);
			box_free(new);
			return NULL;
		}
		(*child)->parent = (Box *)box;
	}

	return new;
}

static void
hand_handle_input_key_mode(xcb_input_key_press_event_t const *const event, Hand *const hand, xcb_keysym_t const sym)
{
	Label *label;

	switch (hand->mode) {
	case HAND_MODE_DEFAULT:
		unreachable;
		return;

	case HAND_MODE_MOVE:
		switch (KEY_MOD_MASK & event->mods.base) {
		case XCB_MOD_MASK_4:
			if (hand_handle_input(hand, sym))
				hand_input_try_jump(hand);
			return;

		case 0:
		case XCB_MOD_MASK_SHIFT:
			switch (sym) {
			case XKB_KEY_F:
				if (!box_is_container(hand->mode_box))
					hand->mode_box = hand->mode_box->parent;
				box_flatten(hand->focus->parent, box_get_pos(hand->focus), hand->mode_box);
				break;

			case XKB_KEY_f:
				if (!box_is_container(hand->mode_box))
					hand->mode_box = hand->mode_box->parent;
				box_flatten(hand->focus->parent, box_get_pos(hand->focus) + 1, hand->mode_box);
				break;

			case XKB_KEY_s:
			case XKB_KEY_S:
				box_swap_checked(hand->focus, hand->mode_box);
				break;

			case XKB_KEY_b:
			case XKB_KEY_P:
			insert:
				if (root == hand->focus->parent)
					goto into;
				box_reparent_checked(hand->focus->parent, box_get_pos(hand->focus), hand->mode_box);
				break;

			case XKB_KEY_a:
			case XKB_KEY_p:
			append:
				if (root == hand->focus->parent)
					goto into;
				box_reparent_checked(hand->focus->parent, box_get_pos(hand->focus) + 1, hand->mode_box);
				break;

			case XKB_KEY_i:
			into:
				if (!box_is_container(hand->focus))
					hand->focus = hand->focus->parent;

				box_reparent_checked(hand->focus, 0, hand->mode_box);
				break;

			case XKB_KEY_h:
				box_explode(hand->focus, false);
				goto insert;

			case XKB_KEY_j:
				box_explode(hand->focus, true);
				goto append;

			case XKB_KEY_k:
				box_explode(hand->focus, true);
				goto insert;

			case XKB_KEY_l:
				box_explode(hand->focus, false);
				goto append;

#if 0
			case XKB_KEY_d:
				if (!(hand->mode_box = box_clone(hand->mode_box)))
					break;
				goto append;
#endif

			default:
				return;
			}
			break;
		}
		break;

	case HAND_MODE_SIZE_SIDE:
		if (hand_input_find_label(hand, &label)) {
			hand->mode = HAND_MODE_SIZE_TO;
			hand->mode_box = label->base;
			break;
		}
		return;

	case HAND_MODE_SIZE_TO:
		if (hand_input_find_label(hand, &label)) {
			break;
		}
		return;

	case HAND_MODE_NAME:
		if (!(KEY_MOD_MASK & event->mods.base))
			hand_handle_input(hand, sym);

		if (XKB_KEY_Return == sym ||
		    sizeof hand->user_input == strnlen(hand->user_input, sizeof hand->user_input))
		{
			memcpy(hand->mode_box->name, hand->user_input, sizeof hand->user_input);
			char *const c = &hand->mode_box->name[0];
			if (box_is_container(hand->mode_box))
				*c &= ~0x20;
			else
				*c |= 0x20;
			box_name(hand->mode_box);
			break;
		} else if (XKB_KEY_space == sym && !*hand->user_input) {
			hand->mode_box->hide_label ^= 1;
			box_propagate_change(hand->mode_box)->label_changed = true;
			break;
		}
		box_propagate_change(hand->mode_box)->label_changed = true;
		return;

	case HAND_MODE_SETCOLUMNS:
	{
		if (!(KEY_MOD_MASK & event->mods.base))
			hand_handle_input(hand, sym);

		Box *const parent = hand->mode_box->parent;
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

		box_change_columns(parent, num_columns);
	}
		break;

	}

	hand_leave_mode(hand);
}

static bool
hand_handle_input_key_command(Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
	switch (sym) {
	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-c "olumns \fIhow\fR"
	 * Set number of columns.
	 * .I how
	 * can be:
	 * .RS
	 * .TP
	 * .RB { a-z }
	 * Set number of columns by selecting box that should appear at the end of the first row.
	 * .TP
	 * .B =
	 * Auto layout.
	 * .TP
	 * .B *
	 * Card (vertical stack) layout.
	 * .TP
	 * .B /
	 * (Horizontal) Stack layout.
	 * .TP
	 * .B +
	 * One more column.
	 * .TP
	 * .B -
	 * One less column.
	 * .RE
	 */
	case XKB_KEY_c:
		if (!hand->focus)
			break;

		hand->mode = HAND_MODE_SETCOLUMNS;
		hand->mode_box = hand->focus;
		break;

	/*MAN( Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-r \fIwhat\fR \fIto-where\fR
	 * Resize a split or a side of a floating window.
	 * .IP
	 * .B =
	 * sets equalizes sizes for all splits in the grid.
	 */
	case XKB_KEY_r:
		/* TODO: take account window gravity */
		hand->mode = HAND_MODE_SIZE_SIDE;
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-g roup
	 * Group related boxes.
	 * .IP
	 * Related boxes are determined by
	 * .BR WM_CLIENT_LEADER " and " WM_CLASS
	 * window properties in this order.
	 */
	case XKB_KEY_g:
		if (!hand->input_focus)
			break;

		box_group(hand->input_focus);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-b arricade
	 * Toggle pointer barrier around focused box.
	 */
	case XKB_KEY_b:
		hand_set_barrier(hand, !hand->barricade);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-t "ake [ " Mod- { a-zA-Z "}... ]... \fIhow\fR"
	 * Take box to somewhere else.
	 * .I how
	 * can be:
	 * .RS
	 * .TP
	 * .BR a "fter, " p aste
	 * Move box after focused one.
	 * .TP
	 * .BR b "efore, " P aste
	 * Move box before focused one.
	 * .TP
	 * .BR i nto
	 * Move box into container.
	 * .TP
	 * .BR h ", " j ", " k ", " l
	 * Place box visually at the given direction.
	 * .TP
	 * .BR s wap
	 * Swap box with focused one.
	 * .TP
	 * .BR f latten
	 * Take out every children of box and place them after focused box.
	 * .TP
	 * .BR F latten
	 * Just like
	 * .B f
	 * but place before.
	 * .RE
	 */
	case XKB_KEY_t:
	{
		if (!hand->focus)
			break;

		hand->mode = HAND_MODE_MOVE;
		hand->mode_box = hand->focus;

		Box *const box = hand_get_latest_input(hand);
		if (box && !box_is_descendant(hand->mode_box, box))
			hand_focus_box(hand, box);
	}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-l ock
	 * Toggle focus lock inside box.
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

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-s how
	 * Explicitly unconceal box.
	 */
	case XKB_KEY_s:
	case XKB_KEY_plus:
	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-h ide
	 * Explicitly conceal box.
	 */
	case XKB_KEY_h:
	case XKB_KEY_minus:
	{
		if (repeating)
			break;

		if (!hand->focus)
			break;

		hand->focus->user_concealed = !hand->focus->user_concealed;
		hand->focus->concealed = sym == XKB_KEY_s || sym == XKB_KEY_plus
			? false
			: hand->focus->user_concealed;
		hand->focus->parent->layout_changed = true;
		box_propagate_change(hand->focus)->label_changed = true;
	}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-m aximize
	 * Implicitly conceal box and its siblings. Unconceal when there are nothing
	 * to conceal.
	 */
	case XKB_KEY_m:
	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-f ull
	 * Do
	 * .B Mod-Ctrl-m
	 * for each level inside a floating box.
	 */
	case XKB_KEY_f:
	{
		if (repeating)
			break;

		if (!hand->focus)
			break;

		bool conceal = !hand->focus->concealed;
		bool const recursive = sym == XKB_KEY_f;

		/* first try conceal any unconcealed box so it is possible to get rid of new boxes */

		for (bool do_conceal = false;; do_conceal = true) {
			Box *parent = hand->focus->parent;

			if (do_conceal)
				box_propagate_change(parent);
			else if (conceal)
				continue;

			do {
				parent->layout_changed = true;
				for (uint16_t i = 0; i < parent->num_children; ++i) {
					Box *const child = parent->children[i];
					if (!child->user_concealed) {
						if (!do_conceal) {
							if ((conceal = !child->concealed))
								goto do_conceal;
						} else {
							child->concealed = conceal;
						}
					}
				}
			} while ((recursive && !box_is_floating(parent)) && (parent = parent->parent, true));

			if (do_conceal)
				break;
		do_conceal:;
		}
	}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-k
	 * Focus parent.
	 */
	case XKB_KEY_k:
		if (hand->focus && hand->focus->parent &&
		    /* addressable? */
		    *hand->focus->parent->name)
			hand_focus_box(hand, hand->focus->parent);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-j
	 * Focus child.
	 */
	case XKB_KEY_j:
		if (hand->focus && hand->input_focus && hand->focus != hand->input_focus) {
			Box *box = hand->input_focus;
			while (box->parent != hand->focus)
				box = box->parent;
			hand_focus_box(hand, box);
		}
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-n "ame {" a-zA-Z "}... Return"
	 * Name focused box.
	 */
	case XKB_KEY_n:
		if (repeating)
			break;

		if (!hand->focus)
			break;

		hand->mode = HAND_MODE_NAME;
		hand->mode_box = hand->focus;
		break;

	/* wlose/xlose */
	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-w indow
	 * Close box just like user would click
	 * \*(lqX\*(rq in the title bar. Kill by force second time.
	 */
	case XKB_KEY_w:
		if (repeating)
			break;

		if (!hand->focus)
			break;

		box_close(hand->focus);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-z enter
	 * Center pointer inside box.
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

	if (HAND_MODE_DEFAULT != hand->mode) {
		hand_input_reset(hand);
		hand_do_mode_changes(hand);
	}

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
	int const n = xkb_keymap_key_get_syms_by_level(device->keymap, event->detail, 0,
			/* onyl care about Shift when user really presses */
			(XCB_MOD_MASK_SHIFT & event->mods.base ? 1 : 0), &syms);
	if (!n)
		goto out;

	xkb_keysym_t const sym = syms[0];
	printf("key=0x%x mods=%d,%d,%d,%d deviceid=%d sourceid=%d (root=%x)\n", sym,
	       event->mods.effective,
	       event->mods.base,
	       event->mods.latched,
	       event->mods.locked,
	       event->deviceid, event->sourceid, event->root);

	HandMode const old_mode = hand->mode;

	switch (hand->mode) {
	case HAND_MODE_DEFAULT:
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

		switch (KEY_MOD_MASK & event->mods.base) {
		default:
			propagate = (hand_handle_input_key_normal(event, hand, sym, repeating), true);
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

		if (XKB_KEY_Escape == sym) {
			hand_leave_mode(hand);
			break;
		}

		hand_handle_input_key_mode(event, hand, sym);
		break;
	}

	if (old_mode != hand->mode)
		hand_grab_keyboard(hand);

out:

	/* if (propagate)
		xcb_test_fake_input(conn, XCB_INPUT_KEY_PRESS, event->detail, XCB_CURRENT_TIME, event->root, event->root_x, event->root_y, event->sourceid); */
	DEBUG_CHECK(xcb_input_xi_allow_events, conn, XCB_CURRENT_TIME, event->deviceid,
			propagate
				? XCB_INPUT_EVENT_MODE_REPLAY_DEVICE
				: XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
}

static void
handle_input_button_press(xcb_input_button_press_event_t const *const event)
{
	Device *const device = get_device_by_id(event->sourceid);
	assert(device);
	Hand *hand = device->hand;

	printf("press\n");
	Body *const body = body_get_by_root(event->root);
	Box *const box = find_box_in_body_by_window(body, event->child);
	if (box)
		hand_focus_box(hand, box);

	/* DEBUG_CHECK(xcb_input_xi_allow_events, conn, XCB_CURRENT_TIME, event->deviceid,
			XCB_INPUT_EVENT_MODE_REPLAY_DEVICE, 0, 0); */
}

static void
handle_input_enter(xcb_input_enter_event_t const *const event)
{
	if (XCB_INPUT_NOTIFY_MODE_NORMAL != event->mode)
		return;

	/*MAN( Keybindings)
	 * .TP
	 * .B Mod4-Mouse Enter...
	 * Focus window.
	 */
	if (0 && XCB_MOD_MASK_4 == event->mods.base) {
		Hand *const hand = find_hand_by_master_pointer(event->deviceid);
		Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->event);
		printf("mod enter\n");
		hand_focus_box(hand, box);
	} else {
		xcb_input_xi_set_client_pointer(conn, event->child, event->deviceid);
	}
}

static void
handle_shape_notify(xcb_shape_notify_event_t const *const event)
{
	Box *const box = find_box_by_window(root, root, offsetof(Box, window), event->affected_window);
	if (!box)
		return;

	if (event->shaped)
		DEBUG_CHECK(xcb_shape_combine, conn, XCB_SHAPE_SO_SET,
				event->shape_kind, event->shape_kind,
				box->frame,
				0, 0, /* offset */
				box->window);
	else
		DEBUG_CHECK(xcb_shape_mask, conn,
				event->shape_kind, event->shape_kind,
				box->frame,
				0, 0, /* offset */
				XCB_PIXMAP_NONE);
}

static bool
handle_shape_event(xcb_generic_event_t const *const event)
{
	if (!shape_base_event)
		return false;

	switch (XCB_EVENT_RESPONSE_TYPE(event) - shape_base_event) {
	case XCB_SHAPE_NOTIFY:
		handle_shape_notify((void const *)event);
		break;

	default:
		return false;
	}

	return true;
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

	case XCB_INPUT_KEY_RELEASE:
		handle_input_key_release((void const *)event);
		break;

	case XCB_INPUT_ENTER:
		handle_input_enter((void const *)event);
		break;
	}
}

static void
handle_randr_screen_change_notify(xcb_randr_screen_change_notify_event_t const *const event)
{
	Body *const body = body_get_by_root(event->root);
	body_update_heads(body);
}

static bool
handle_randr_event(xcb_generic_event_t const *const event)
{
	if (!randr_base_event)
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
	if (!xkb_base_event)
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
		handle_xkb_event(event) ||
		handle_shape_event(event);
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
	assert(HAND_MODE_DEFAULT == hand->mode);
	printf("hand_timed_out(()\n");
	hand->mode = HAND_MODE_DEFAULT;
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

	assert(!xcb_connection_has_error(conn));

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
#define EVENT(type, handler) case type: handler((void *)event); break;
		EVENT(0,                     handle_error);
		EVENT(XCB_MAP_REQUEST,       handle_map_request);
		EVENT(XCB_CONFIGURE_NOTIFY,  handle_configure_notify);
		EVENT(XCB_CONFIGURE_REQUEST, handle_configure_request);
		EVENT(XCB_UNMAP_NOTIFY,      handle_unmap_notify);
		EVENT(XCB_EXPOSE,            handle_expose);
		EVENT(XCB_PROPERTY_NOTIFY,   handle_property_notify);
		EVENT(XCB_CLIENT_MESSAGE,    handle_client_message);
		EVENT(XCB_GE_GENERIC,        handle_generic_event);
#undef EVENT
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

	if ((env = getenv("HOME")))
		chdir(env);
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
