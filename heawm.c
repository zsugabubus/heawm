#include <assert.h>
#include <ctype.h>
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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#include <cairo/cairo-xcb.h>
#include <cairo/cairo.h>
#include <xcb/bigreq.h>
#include <xcb/composite.h>
#include <xcb/randr.h>
#include <xcb/shape.h>
#include <xcb/xcb.h>
#include <xcb/xcb_atom.h>
#include <xcb/xcb_aux.h>
#include <xcb/xcb_cursor.h>
#include <xcb/xcb_event.h>
#include <xcb/xcb_ewmh.h>
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_image.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xcb_xrm.h>
#include <xcb/xfixes.h>
#include <xcb/xinput.h>
#include <xcb/xkb.h>
#include <xcb/xproto.h>
#include <xcb/xtest.h>
#include <xkbcommon/xkbcommon-x11.h>
#include <xkbcommon/xkbcommon.h>

/* TEST: https://superuser.com/questions/801611/how-to-make-all-applications-respect-my-modified-xkb-layout/844673#844673 */
/* https://specifications.freedesktop.org/wm-spec/1.3/ar01s05.html */
/* https://specifications.freedesktop.org/wm-spec/1.3/ar01s07.html */
/* https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html#idm46035372536800 */
/* https://people.gnome.org/~tthurman/docs/metacity/xprops_8h-source.html */

#if defined(__GNUC__)
# define maybe_unused __attribute__((unused))
#else
# define maybe_unused
#endif

#if defined(__GNUC__) || defined(clang)
# define unreachable __builtin_unreachable()
#else
# define unreachable (*(int *)0 = 0)
#endif

#ifndef M_PHI
# define M_PHI 1.6180339887 /** Golden Ratio */
#endif

/* sizeof array... but in elements */
#define ARRAY_SIZE(...) (sizeof (__VA_ARGS__) / sizeof *(__VA_ARGS__))
#define memberof(type, base, offset) ((type *)((uintptr_t)(base) + (offset)))
#define membersizeof(type, member) (sizeof(((type *)0)->member))

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

#define XCHECK(xcb_request, ...) \
	check_cookie(xcb_request##_checked(__VA_ARGS__), STRINGIFY(__LINE__) ": " #xcb_request)

#if 1 <= HEAWM_VERBOSE
# define XDO(xcb_request, ...) (void)XCHECK(xcb_request, __VA_ARGS__)
#else
# define XDO(xcb_request, ...) (void)xcb_request(__VA_ARGS__)
#endif

#define XCB_GET_REPLY(x, xcb_request, ...) \
	xcb_request##_reply_t *const x = \
			xcb_request##_reply(conn, xcb_request##_unchecked(conn, __VA_ARGS__), NULL)

/* Because xcb_send_event() does not take an |event| length argument, no matter
 * what, it will copy a fixed number of bytes. To avoid reading (then sending)
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

#define XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(grab_window, cursor, deviceid, grab_type, detail, grab_mode, owner_events, mask, ...) \
	xcb_input_xi_passive_grab_device_unchecked(conn, XCB_CURRENT_TIME, grab_window, \
			cursor, detail, deviceid, \
			ARRAY_SIZE((uint32_t const[])__VA_ARGS__), 1, \
			grab_type, \
			grab_mode, XCB_INPUT_GRAB_MODE_22_ASYNC, \
			owner_events, \
			(uint32_t const[]){ mask }, (uint32_t const[])__VA_ARGS__)

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

#define XCB_ERROR_NOTIFY 0

enum {
	XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_XY =
		XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_X |
		XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_Y,
	XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_XY =
		XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_X |
		XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_Y,
	XCB_XFIXES_BARRIER_DIRECTIONS_ALL =
		XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_X |
		XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_Y |
		XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_X |
		XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_Y,
};

enum { XCB_MOD_MASK_NUM_LOCK = XCB_MOD_MASK_2, };

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

#define EFFECTIVE_MASK(mask) \
	                                            (mask), \
	XCB_MOD_MASK_LOCK |                         (mask), \
	                    XCB_MOD_MASK_NUM_LOCK | (mask), \
	XCB_MOD_MASK_LOCK | XCB_MOD_MASK_NUM_LOCK | (mask)

#define FRAME_WINDOW_EVENT_MASK \
	( XCB_EVENT_MASK_STRUCTURE_NOTIFY \
	| XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT \
	)

#define CLIENT_WINDOW_EVENT_MASK \
	( XCB_EVENT_MASK_PROPERTY_CHANGE \
	| XCB_EVENT_MASK_STRUCTURE_NOTIFY \
	| XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY \
	| XCB_EVENT_MASK_FOCUS_CHANGE \
	| XCB_EVENT_MASK_ENTER_WINDOW \
	)

#define XCB_STRING_MAX (64 * sizeof(uint32_t))

#define WM_NAME "heawm"

#define REPEAT_XY(macro) macro(x, width) macro(y, height)

#define for_each_template_(elem_type, list, index_type, name, extra) \
	for (index_type name##_index = 0; (extra) || name##_index < num_##list; ++name##_index) \
		for (elem_type *const name = &list[name##_index], *loop_ = (void *)true; loop_; loop_ = (void *)false)

#define MONITOR_CLASS WM_NAME"-monitor"
#define EXTREMAL_NAME_CHAR '\x7f'

enum Orientation {
	LEFT = -1,
	TOP = -1,
	CENTER = 0,
	RIGHT = 1,
	BOTTOM = 1,
};

enum FloatResize {
	FLOAT_UNCHANGED,
	FLOAT_TILE0,
	FLOAT_TILE1 = FLOAT_TILE0 + 1,
	FLOAT_TILE9 = FLOAT_TILE0 + 9,
	FLOAT_RECT,
};

#define for_each_box(x, root) \
	for (bool loop_ = ((x) = (root), true); loop_ && ((x)->iter = 0, true); ({ \
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

typedef struct {
	xcb_window_t window; /**< Relative to. */
	xcb_input_fp1616_t x, y;
} BoxPointer;

#define Box_pointers(box) ((BoxPointer *)&box->children)

typedef struct Box Box;
struct Box {
	xcb_rectangle_t rect;

	/** Last time (sequence number) when box has been focused
	 *
	 * (1) {never focused box}->focus_seq := 0
	 * (2) {old focus}->focus_seq < {new focus}->focus_seq
	 * (3) parent->focus_seq := MAX(children[..]->focus_seq)
	 *  => {focused box(es)}->focus_seq := root->focus_seq
	 *  => {focused child(ren)}->focus_seq := parent->focus_seq
	 */
	uint32_t focus_seq;

	xcb_window_t window;
	/**
	 * Parent of window
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
	/**
	 * Show only last N focused children. (Not counting user_concealed.)
	 *
	 * Default: 0. n=1: maximize.
	 */
	         num_visible,
	/**
	 * Internal variable used to track progress of recursive searching
	 *
	 * - it does not occupy too much space and we do not have to
	 *   dynamically allocate more memory for backtracking or track maximum
	 *   possible depth
	 * - it also makes very convenient to continue the walk where we left
	 *   off */
	         iter;

	/*
	 * For monitors: urect.{width,height} is in mm monitor dimensions.
	 * For floating: user set size.
	 */
	xcb_rectangle_t urect;

	char name[2];

	uint8_t mod_x, mod_y; /**< Size granulaty */

	uint8_t body;

	/**
	 * Which hand focused the box (last time)
	 *
	 * because simultenously multiple hands can focus a box, it is only
	 * valid if box->focus_seq != root->focus_seq (not currently focused).
	 * in these cases focus_hand is garbage and to find out which hand(s)
	 * hold(s) the focus: hands[..]->input_focus ?= box. */
	uint8_t focus_hand;

	uint16_t conceal_seq; /**< When children of this container was last time
	                        maximized */

	/* We always update the whole scene. these variables help to track changes. */
	bool position_changed: 1, /**< rect->{x,y} changed */
	     layout_changed: 1, /**< Anything changed that may affect layout of its children */
	     should_map: 1, /**< Together with position_changed means that
	                      window has been freshly mapped (size was
	                      previously zero) */
	     should_focus: 1, /**< Window freshly focused */
	     content_changed: 1, /**< Set by children to indicate parent should
	                           descend with update (means that this flag
	                           must be propagated upwards until root);
	                           for client windows indicates that name or title changed */
	     label_changed: 1, /**< Label should be repainted */
	     close_by_force: 1,
	     flagged: 1, /**< Temporary flag for various purposes. */
	     hide_label: 1,
	     concealed: 1, /**< Show only when has focus; hide otherwise. */
	     user_concealed: 1,
	     saved_concealed: 1,
	     focus_lock: 1, /**< Fix focus position on screen by always swapping
	                      newly focused window with previously focused
	                      window */
	     shaped: 1, /**< Client has shaped bounding box. */
	     floating: 1, /**< Tiled or floating? */
	     vertical: 1,
	     net_fullscreen: 1;

	xcb_xfixes_barrier_t barrier;

	char *title;
	char *class;

	Box *parent;
	Box *children[];
};

static Box *root; /**< Mother of all boxes. */

static uint16_t const MONITOR_GAP = 0;
static uint16_t const CONTAINER_GAP = 4;
static uint16_t const WINDOW_GAP = 1;
static uint16_t const BORDER_RADIUS = 0;
static uint16_t const SNAP_DISTANCE = 25;
/**
 * Do not apply border radius for shaped windows.
 */
static bool const BORDER_RADIUS_FOR_SHAPED = true;

#define LABEL_INSTANCE WM_NAME"-label"

#define RGB8_TO_FLOATS(color) \
		(uint8_t)((color) >> 16) / 256., \
		(uint8_t)((color) >> 8 ) / 256., \
		(uint8_t)((color)      ) / 256.

#define label_stroke_rgb 0, 0, 0
#define label_color_rgb 1, 1, 0

typedef struct {
	int16_t x, y;
} Point;

enum LabelType {
	LABEL_NORMAL, /**< Text only */
	LABEL_BOX, /**< Draw glory if |base| focused */
	LABEL_HLINE, /**< Text with a horizontal line */
	LABEL_VLINE, /**< Text with a vertical line */
};

typedef struct {
	Box *base;
	uint64_t hands;
	xcb_window_t window;
	xcb_pixmap_t shape;
	int16_t x, y;
	enum LabelType type: 2;
	bool position_changed: 1;
	char name[membersizeof(Box, name)];
} Label;

static char const LABEL_FONT_DEFAULT[] = "monospace";
static char *label_font;
static Point label_rect = { .x = 30, .y = 60 }; /* In pts */
static int label_stroke = 2;
static int label_font_size = 17;

enum HandMode {
	HAND_MODE_NULL,
	HAND_MODE_MOVE,
	HAND_MODE_NAME,
	HAND_MODE_POINTER_MOVE,
};

#define MAX_NUM_HANDS 63U
#define NULL_HAND (MAX_NUM_HANDS + 1U)

#define for_each_hand for_each_template_(Hand, hands, uint8_t, hand, 0)

typedef struct {
	/* Keyboard and pointer are always in pair */
	xcb_input_device_id_t master_pointer; /**< Master pointer device */
	xcb_input_device_id_t master_keyboard; /**< Master key device */

	bool
	     want_focus: 1, /**< Allow auto focusing newly mapped window */
	     check_input: 1,
	     barricade: 1, /**< Make barricade around currently focused window */
	     /**
	      * Next newly mapped window should treated as a popup
	      *
	      * Ideally we should get this window through X Startup Notification
	      * however many applications does not support it we cannot rely on
	      * it. */
	     want_popup: 1,
	     focus_changed: 1;

	xcb_xfixes_barrier_t barrier; /**< Pointer barrier (around) */

	char user_input[membersizeof(Box, name)];

	/**
	 * Current mode and related state.
	 */
	enum HandMode mode;
	Box *mode_box;
	xcb_rectangle_t mode_rect;

	/**
	 * Box that last time received keyboard input (only non-container);
	 * we also store the box that was focused before last time since if
	 * we start typing into the currently focused window that is not
	 * really interesting to remember for.
	 */
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

	unsigned int color; /* 0xrrggbb */
} Hand;

static bool hands_changed;
static uint8_t num_hands;
static Hand *hands;

typedef struct {
	xcb_input_device_id_t id;
	struct xkb_keymap *keymap; /* NULL if not keyboard */
	Hand *hand;
} Device;

static uint8_t num_devices;
static Device *devices;

#define for_each_body for_each_template_(Body, bodies, uint8_t, body, 0 == body_index)

typedef struct {
	xcb_screen_t *screen;
	xcb_visualtype_t *visual_type;
	int screen_index;

	uint32_t num_labels_used,
	/* <= */ num_labels_mapped,
	/* <= */ num_labels_created,
	/* <= */ num_labels;
	Label *labels;

	/*
	 *  LABELS
	 *      A
	 *      |
	 * label_layer
	 *      |
	 *      V
	 * FLOATING FOCUSED
	 *
	 * float_layer
	 *      |
	 *      V
	 *   FLOATING
	 *
	 *      _
	 *      |
	 *      V
	 *    TILES
	 */
	union {
		xcb_window_t label_layer;
		xcb_window_t net_window;
	};
	xcb_window_t float_layer;

	bool net_client_list_changed: 1,
	     composited: 1;

	xcb_cursor_t move_cursor;
} Body;

static uint8_t num_bodies;
static Body *bodies;

typedef struct {
	char name[membersizeof(Box, name)];
	char const *class_instance;
} Rule;

static Rule const RULES[] = {
	/* FIXME: Ehhm... Probably class<->instance. */
	{ "b", "Navigator\0firefox", },
	{ "t", "telegram-desktop\0TelegramDesktop", },
	{ "v", "gl\0mpv", },
	{ "z", "org.pwmt.zathura\0Zathura", },
};

#define ATOM(name) ((xcb_atom_t const)atoms[HEAWM_ATOM_##name])

#define NET_ATOMS \
	xmacro(_NET_ACTIVE_WINDOW) \
	xmacro(_NET_CLIENT_LIST) \
	xmacro(_NET_CLOSE_WINDOW) \
	xmacro(_NET_SUPPORTED) \
	xmacro(_NET_SUPPORTING_WM_CHECK) \
	xmacro(_NET_WM_NAME) \
	xmacro(_NET_WM_PID) \
	xmacro(_NET_WM_STATE) \
	xmacro(_NET_WM_STATE_FOCUSED) \
	xmacro(_NET_WM_STATE_FULLSCREEN) \
	xmacro(_NET_WM_STATE_HIDDEN) \
	xmacro(_NET_WM_TRANSIENT_FOR) \

#define ATOMS \
	NET_ATOMS /* Must start with this. */ \
	xmacro(_HEAWM_NAME) \
	xmacro(WM_CLIENT_LEADER) \
	xmacro(WM_DELETE_WINDOW) \
	xmacro(WM_NORMAL_HINTS) \
	xmacro(WM_PROTOCOLS) \
	xmacro(WM_SIZE_HINTS) \
	xmacro(WM_STATE) \
	xmacro(UTF8_STRING) \

static char const *const ATOM_NAMES[] = {
#define xmacro(name) #name,
	ATOMS
#undef xmacro
};

enum {
#define xmacro(name) HEAWM_ATOM_##name,
	ATOMS
#undef xmacro
};

static xcb_atom_t atoms[ARRAY_SIZE(ATOM_NAMES)];

static xcb_connection_t *conn;
static xcb_xrm_database_t *xrm;
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

static struct {
	char const *terminal;
	char const *shell;
	char heawm_home[PATH_MAX];
	size_t heawm_home_size;
} config;

static int const GC_INTERVAL = 5 * 60 * 1000;

#define print_strerror(what) \
	fprintf(stderr, "%s: %s: %s\n", __func__, what, strerror(errno));

#define SPAWN(...) spawn((char const *[]){ __VA_ARGS__, NULL }, NULL, NULL)
#define HOOK_SPAWN(hook, arg, ...) spawn((char const *[]){ __VA_ARGS__, NULL }, (void(*)(void *))hook, arg)
#define BODY_SPAWN(body, ...) spawn((char const *[]){ __VA_ARGS__, NULL }, (void(*)(void *))body_set_display, (body))

#define SCRIPT_SIZE_MAX 32

static char const *
get_script_path(char const *name)
{
	assert(strlen(name) <= SCRIPT_SIZE_MAX);

	strcpy(config.heawm_home + config.heawm_home_size, name);

	return config.heawm_home;
}

static pid_t
spawn(char const *argv[], void(*fork_cb)(void *), void *arg)
{
	pid_t ret;
	sigset_t sigmask, origmask;

	/* Block all signals in order to avoid executing registered signal
	 * handlers in child. */
	sigfillset(&sigmask);
	pthread_sigmask(SIG_SETMASK, &sigmask, &origmask);

	fprintf(stderr, "spawning");
	for (char const **arg = argv; *arg; ++arg)
		fprintf(stderr, " \"%s\"", *arg);
	fputc('\n', stderr);

	if (0 == (ret = fork())) {
		/* Move process into its own session. */
		setsid();

		/* Signal handlers have to be reset to their default action
		 * before we unblock them for the new process to avoid executing
		 * them before exec(). */
		for (int sig = 1; sig < SIGRTMAX; ++sig)
			sigaction(sig, &(struct sigaction const){
					.sa_handler = SIG_DFL
				}, NULL);

		sigemptyset(&sigmask);
		pthread_sigmask(SIG_SETMASK, &sigmask, NULL);

		/* Disconnect standard IO. */
		int dev_null = open("/dev/null", O_RDWR | O_CLOEXEC);
		if (dup2(dev_null, STDIN_FILENO) < 0)
			close(STDIN_FILENO);
		if (dup2(dev_null, STDOUT_FILENO) < 0)
			close(STDOUT_FILENO);
		/* Leave stderr untouched. */

		if (fork_cb)
			fork_cb(arg);

		execvp(argv[0], (char **)argv);
		print_strerror("execlp");
		_exit(127);
	}

	pthread_sigmask(SIG_SETMASK, &origmask, NULL);

	return ret;
}

static Hand *
hand_find_by_barrier(xcb_xfixes_barrier_t const barrier)
{
	for_each_hand
		if (hand->barrier <= barrier && barrier < hand->barrier + 4)
			return hand;
	return NULL;
}

static Box *
box_find_by_barrier(Box *const root, xcb_xfixes_barrier_t const barrier)
{
	Box *box;
	for_each_box(box, root)
		if (box->barrier <= barrier && barrier < box->barrier + 4)
			break;
	return box;
}

static Box *
box_find_by_window(Box *const root, unsigned const offset, xcb_window_t const window)
{
	if (XCB_WINDOW_NONE == window)
		return NULL;

	Box *box;
	for_each_box(box, root)
		if (window == *memberof(xcb_window_t const, box, offset))
			break;
	return box;
}

static Box *
find_box_in_body_by_window(Body *const body, unsigned const offset, xcb_window_t const window)
{
	uint8_t const body_pos = body - bodies;

	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (body_pos != head->body)
			continue;

		Box *const box = box_find_by_window(head, offset, window);
		if (box)
			return box;
	}

	return NULL;
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

#define CONVERT(x, width) .x = ((int)monitor->rect.width * (int)pt.x * 254 / 720 / (int)monitor->urect.width),
	return (Point){ REPEAT_XY(CONVERT) };
#undef CONVERT
}

static bool
box_is_container(Box const *const box)
{
	return XCB_WINDOW_NONE == box->frame;
}

static bool
box_is_super_float(Box const *const box)
{
	assert(box_is_container(box));
	for (uint16_t i = 0; i < box->num_children; ++i)
		if (!box->children[i]->floating)
			return false;
	return true;
}

static bool
box_is_super_container(Box const *const box)
{
	assert(box_is_container(box));
	for (uint16_t i = 0; i < box->num_children; ++i)
		if (!box_is_container(box->children[i]))
			return false;
	return true;
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
			(Point){ 0, label_font_size }).y;
	int const stroke_width = monitor_convert_pt2px(monitor,
			(Point){ label_stroke, 0 }).x;

	Body const *const body = &bodies[label->base->body];
	cairo_surface_t *const surface = shape
		? cairo_xcb_surface_create_for_bitmap(conn, body->screen, label->shape, size.x, size.y)
		: cairo_xcb_surface_create(conn, label->window, body->visual_type, size.x, size.y);
	cairo_t *const cr = cairo_create(surface);

	if (!bodies[label->base->body].composited)
		cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);

	cairo_select_font_face(cr, label_font ? label_font : LABEL_FONT_DEFAULT,
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

		/* Maybe an inbetween container that contains focused box. */
		if (0 == i)
			goto normal;

		if (shape) {
			/* The shape is a circle, we do not have to care about colors. */
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
		else if (label->base->concealed && label->base->parent->focus_seq != label->base->focus_seq)
			symbol = "~";
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

static void *
check_alloc(void *p) {
	if (p)
		return p;
	fputs("Failed to allocate memory\n", stderr);
	exit(EXIT_FAILURE);
}

static Label *
label_new_for(Box *const box)
{
	Label *label;
	Body *const body = &bodies[box->body];

	if (body->num_labels_used == body->num_labels) {
		size_t const new_size = (body->num_labels + 1) * 8 / 5 /* Golden ratio. */;
		void *const p = realloc(body->labels, new_size * sizeof *body->labels);
		if (!p)
			return NULL;

		body->labels = p;
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
#if 1 <= HEAWM_VERBOSE
			"%s"
#else
			"%d"
#endif
			" (seq=%d, opcode=%d:%d, value=%d=0x%x)\n",
			__FILE__, message,
#if 1 <= HEAWM_VERBOSE
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

static Point
box_compute_position(Box const *const box, enum Orientation const ox, enum Orientation const oy, bool const outside)
{
	Box const *const monitor = box_get_head(box);
	Point const size = monitor_convert_pt2px(monitor, label_rect);

	Point ret = {
		.x = box->rect.x + (ox + 1) * (box->rect.width  -size.x) / 2 + (outside ? ox : 0) * (size.x / 2),
		.y = box->rect.y + (oy + 1) * (box->rect.height -size.y) / 2 + (outside ? oy : 0) * (size.y / 2)
	};

#define CLAMP(x, width) \
	if (ret.x < monitor->rect.x) \
		ret.x = monitor->rect.x; \
	else if (monitor->rect.x + monitor->rect.width < ret.x + size.x) \
		ret.x = monitor->rect.x + monitor->rect.width - size.x;
	REPEAT_XY(CLAMP);
#undef CLAMP

	return ret;
}

static void
label_destroy(Label *const label)
{
	XDO(xcb_destroy_window, conn, label->window);
	XDO(xcb_free_pixmap, conn, label->shape);
}

static void
label_create(Label *const label)
{
	assert(label->base);
	Body const *const body = &bodies[label->base->body];
	xcb_screen_t const *const screen = body->screen;

	if (XCB_WINDOW_NONE == label->window)
		label->window = xcb_generate_id(conn);

	Box const *const monitor = box_get_head(label->base);
	Point const size = monitor_convert_pt2px(monitor, label_rect);

	XDO(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
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

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			label->window, XCB_ATOM_WM_CLASS,
			XCB_ATOM_STRING, 8,
			sizeof LABEL_INSTANCE "\0" WM_NAME,
			LABEL_INSTANCE "\0" WM_NAME);

	/* TODO: search for labels with the same name and type and copy shape from there */
	/* setup its shape */
	if (XCB_PIXMAP_NONE == label->shape)
		label->shape = xcb_generate_id(conn);

	XDO(xcb_create_pixmap, conn,
			1, /* Mask is on or off thus 1 bit. */
			label->shape,
			label->window,
			size.x, size.y);

	/* We need a valid pixmap so we use the bounding mask but we
	 * use offsets to move it outside of the area making effective
	 * input region empty. */
	XDO(xcb_shape_mask, conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_INPUT,
			label->window,
			size.x, size.y,
			label->shape);
}

static void
label_update(Label *const label)
{
	Body *const body = &bodies[label->base->body];

	bool const should_create = body->num_labels_created <= (label - body->labels);
	if (should_create) {
		assert(body->num_labels_created + 1 == body->num_labels_used);
		body->num_labels_created = body->num_labels_used;
		label_create(label);
	}

	if (label->position_changed) {
		label->position_changed = false;
		/* move label to its place and make sure its above base window */
		XDO(xcb_configure_window, conn, label->window,
				XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y |
				XCB_CONFIG_WINDOW_SIBLING |
				XCB_CONFIG_WINDOW_STACK_MODE,
				(uint32_t const[]){
					label->x, label->y,
					body->label_layer,
					XCB_STACK_MODE_ABOVE
				});
	}

	label_repaint(label, true);

	XDO(xcb_shape_mask, conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_BOUNDING,
			label->window,
			0, 0,
			label->shape);

	bool const should_map = body->num_labels_mapped <= (label - body->labels);
	if (should_map) {
		assert(body->num_labels_mapped + 1 == body->num_labels_used);
		body->num_labels_mapped = body->num_labels_used;
		XDO(xcb_map_window, conn, label->window);
		/* Will be repainted at exposure. */
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
	SPAWN(get_script_path("exit"));

	exit(EXIT_SUCCESS);
}

static void
init_atoms(void)
{
	xcb_intern_atom_cookie_t cookies[ARRAY_SIZE(ATOM_NAMES)];

	for (uint32_t i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		char const *const name = ATOM_NAMES[i];
		cookies[i] = xcb_intern_atom_unchecked(conn, false, strlen(name), name);
	}

	for (uint32_t i = 0; i < ARRAY_SIZE(ATOM_NAMES); ++i) {
		xcb_intern_atom_reply_t *const reply =
			xcb_intern_atom_reply(conn, cookies[i], NULL);

		if (!reply)
			continue;

		atoms[i] = reply->atom;
		free(reply);
	}
}

/**
 * @return whether search finished
 */
static bool
box_find_by_name(Box **const optimum, char name[static membersizeof(Box, name)])
{
	uint8_t const n = strnlen(name, membersizeof(Box, name));
	if (!n)
		return false;

	for (Box *top = *optimum;; top = root) {
		if (!top)
			continue;

		bool complete = true;

		*optimum = NULL;

		Box *box;
		for_each_box(box, top) {
			if (!memcmp(name, box->name, n)) {
				complete &= membersizeof(Box, name) <= n || !box->name[n];
				if (!*optimum || (*optimum)->focus_seq < box->focus_seq)
					*optimum = box;
			}
		}

		if (*optimum || top == root)
			return complete;
	}
}

static Box *
box_propagate_change(Box *box)
{
	Box *const ret = box;
	while ((box = box->parent) && !box->content_changed)
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
box_clear_name(Box *const box)
{
	*box->name = '\0';
}

static void
box_set_placeholder_name(Box *const box)
{
	*box->name = EXTREMAL_NAME_CHAR;
}

static bool
box_has_placeholder_name(Box const *const box)
{
	return EXTREMAL_NAME_CHAR == *box->name;
}

static void
box_name(Box *const box)
{
	if (box_has_placeholder_name(box))
		return;

	assert(box != root && box->parent);

	bool const is_container = box_is_container(box);
	struct {
		uint32_t focus_seq;
	} letters[(unsigned)EXTREMAL_NAME_CHAR + 1] = { { UINT32_MAX }, /* 0, 0, 0, ... */ };

	uint8_t n = strnlen(box->name, sizeof box->name);
	n -= !!n;

	char optimum = box->name[n];

	for (size_t i = 0; i < ARRAY_SIZE(RULES); ++i) {
		Rule const *const rule = &RULES[i];
		if (!(rule->name[n] && !rule->name[n + 1]))
			continue;

		/* Try assigning reserved letters last. */
		letters[(unsigned char)rule->name[n]].focus_seq = root->focus_seq + 1;

		if (!optimum && box_match_class(box, rule->class_instance, rule->class_instance + strlen(rule->class_instance) + 1))
			optimum = rule->name[n];
	}

	if (!optimum) {
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box const *const child = box->children[i];
			if (!box_has_placeholder_name(child)) {
				assert(child->name[0] && "box has parent but no name");
				for (uint8_t j = n; !(optimum = child->name[j--]););
				break;
			}
		}
	}

	optimum = (box_is_container(box) ? toupper : tolower)(optimum);

#define NAME_MATCHES(test_box) \
	(/* Prefix matches? */ \
	 !memcmp((test_box)->name, box->name, n) && \
	 /* End of name? */ \
	 ((uint8_t)membersizeof(Box, name) <= n + 1 || !(test_box)->name[n + 1]))

	/* Exclude names of children so we can always move at least one downwards. */
	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box const *const child = box->children[i];
		if (NAME_MATCHES(child))
			letters[(unsigned char)child->name[n]].focus_seq = UINT32_MAX;
	}

	if (!box_is_monitor(box) && box_is_monitor(box->parent))
		/* Exclude names of neck (treat all necks from all heads as children). */
		for (uint16_t i = 0; i < root->num_children; ++i) {
			Box const *const head = root->children[i];
			for (uint16_t j = 0; j < head->num_children; ++j) {
				Box const *const neck = head->children[j];
				if (neck != box && NAME_MATCHES(neck))
					letters[(unsigned char)neck->name[n]].focus_seq = UINT32_MAX;
			}
		}
	else
		/* Exclude names of siblings so we can always move horizontally. */
		for (uint16_t i = 0; i < box->parent->num_children; ++i) {
			Box const *const child = box->parent->children[i];
			if (child != box && NAME_MATCHES(child))
				letters[(unsigned char)child->name[n]].focus_seq = UINT32_MAX;
		}

	/* Exclude parent names so we can always move upwards. */
	for (Box const *parent = box; (parent = parent->parent);)
		if (NAME_MATCHES(parent))
			letters[(unsigned char)parent->name[n]].focus_seq = UINT32_MAX;

	Box *test;
	for_each_box(test, root)
		if (test != box && NAME_MATCHES(test)) {
			uint32_t *const p = &letters[(unsigned char)test->name[n]].focus_seq;
			/* Use a non-zero focus_seq to avoid treating never
			 * focused boxes as free letters. */
			if (*p < test->focus_seq + 2)
				*p = test->focus_seq + 2;
		}

#undef NAME_MATCHES

	if (UINT32_MAX == letters[(unsigned char)optimum].focus_seq)
		for (unsigned char start = is_container ? 'A' : 'a', end = start + ('Z' - 'A');
		     start <= end;
		     ++start)
			if (letters[start].focus_seq < letters[(unsigned char)optimum].focus_seq)
				optimum = start;

	if (UINT32_MAX == letters[(unsigned char)optimum].focus_seq) {
		abort();
		return;
	}

	box->name[n++] = optimum;
	memset(box->name + n, 0, sizeof box->name - n);

	if (!box_is_container(box))
		XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				box->window, ATOM(_HEAWM_NAME),
				XCB_ATOM_STRING, 8,
				n, box->name);

	box_propagate_change(box)->label_changed = true;
}

static uint16_t
compute_num_columns(xcb_rectangle_t const *const rect, uint16_t const num_tiles)
{
	uint16_t ret = 1; /* Always return non-zero. */
	uint16_t minimal_row = 0;
	uint32_t minimal = UINT32_MAX;
	for (uint16_t cols = 1; cols <= num_tiles; ++cols) {
		uint16_t rows = (num_tiles + cols - 1) / cols;
		uint16_t last_cols = num_tiles - (rows - 1) * cols;
		uint32_t perimeter =
			((rect->width / cols) + (rect->height / rows)) * (rows - 1) * cols +
			((rect->width / last_cols) + (rect->height / rows)) * last_cols * 1;
		if (perimeter < minimal && minimal_row != rows) {
			ret = cols;
			minimal_row = rows;
			minimal = perimeter;
		}
	}
	return ret;
}

static void
box_set_position(Box *const box, int16_t const x, int16_t const y)
{
	box->position_changed |=
		x != box->rect.x ||
		y != box->rect.y;
	box->rect.x = x;
	box->rect.y = y;
}

static void
box_set_size(Box *const box, uint16_t const width, uint16_t const height)
{
	box->layout_changed |=
		width != box->rect.width ||
		height != box->rect.height;
	box->should_map |= !box->rect.width;
	box->rect.width = width;
	box->rect.height = height;
}

static void
box_set_uposition(Box *const box, int16_t const x, int16_t const y)
{
	box->urect.x = x;
	box->urect.y = y;
	box_set_position(box, x, y);
}

static void
box_set_usize(Box *const box, uint16_t const width, uint16_t const height)
{
	box->urect.width = width;
	box->urect.height = height;
	box_set_size(box, width, height);
}

static void
box_set_urect(Box *const box, xcb_rectangle_t const rect)
{
	box_set_uposition(box, rect.x, rect.y);
	box_set_usize(box, rect.width, rect.height);
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
	XCB_GET_REPLY(reply, xcb_input_xi_query_pointer,
			bodies[box->body].screen->root, hand->master_pointer);
	if (!reply)
		return;

	Box const *const base = box_find_by_window(root, offsetof(Box, frame), reply->child);
	Box_pointers(box)[hand - hands] = base
		? (BoxPointer){
			.window = reply->child,
			.x = reply->root_x - (base->rect.x << 16),
			.y = reply->root_y - (base->rect.y << 16),
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
	bool check = true;

	if (XCB_WINDOW_NONE == pointer.window) {
	set_default_location:
		check = false;
		pointer.window = bodies[box->body].screen->root;
		pointer.x = (xcb_input_fp1616_t)(box->rect.x + box->rect.width / 2) << 16;
		pointer.y = (xcb_input_fp1616_t)(box->rect.y + box->rect.height / M_PHI) << 16;
	}

	if (XCHECK(xcb_input_xi_warp_pointer, conn,
			XCB_WINDOW_NONE,
			pointer.window,
			0, 0, 0, 0,
			pointer.x, pointer.y,
			hand->master_pointer) && check)
		goto set_default_location;
}

static xcb_xfixes_barrier_t
generate_barrier_ids(void)
{
	xcb_xfixes_barrier_t id = xcb_generate_id(conn);

	for (uint8_t i = 1; i < 4;) {
		xcb_xfixes_barrier_t next_id = xcb_generate_id(conn);
		if (id + i++ == next_id)
			continue;

		id = next_id;
		i = 1;
	}

	return id;
}

static void
delete_barrier(xcb_xfixes_barrier_t const barrier)
{
	if (XCB_NONE != barrier)
		for (uint8_t i = 0; i < 4; ++i)
			XDO(xcb_xfixes_delete_pointer_barrier, conn, barrier + i);
}

static void
hand_unbarricade(Hand *const hand)
{
	delete_barrier(hand->barrier);
}

static void
box_update_barrier(Box *const box, Hand const *const for_hand)
{
	delete_barrier(box->barrier);

	if (!for_hand && !box->floating)
		return;

	if (XCB_NONE == box->barrier)
		box->barrier = generate_barrier_ids();

	xcb_xfixes_barrier_t const barrier = for_hand ? for_hand->barrier : box->barrier;
	for (uint8_t i = 0; i < 4; ++i)
		/*
		 *  0<
		 * 1 2
		 * %3
		 */
		XDO(xcb_xfixes_create_pointer_barrier, conn,
				barrier + i,
				bodies[box->body].screen->root,
				box->rect.x + (i < 2 ? 0 : box->rect.width),
				box->rect.y + (i < 2 ? 0 : box->rect.height),
				box->rect.x + (i % 2 ? 0 : box->rect.width),
				box->rect.y + (i % 2 ? box->rect.height : 0),
				for_hand
					? ((i < 2) ? XCB_XFIXES_BARRIER_DIRECTIONS_POSITIVE_XY
					           : XCB_XFIXES_BARRIER_DIRECTIONS_NEGATIVE_XY)
					: XCB_XFIXES_BARRIER_DIRECTIONS_ALL,
				!!for_hand, for_hand ? &for_hand->master_pointer : NULL);
}

static void
hand_barricade(Hand *const hand, Box *const box)
{
	hand_unbarricade(hand);
	box_update_barrier(box, hand);
}

static void
hand_update_barrier(Hand *const hand)
{
	Box *const focus = hand->input_focus;
	if (hand->barricade && focus)
		hand_barricade(hand, focus);
}

static void
hand_refocus(Hand *const hand)
{
	Box const *const focus = hand->input_focus;
	assert(!focus || !box_is_container(focus));
	assert(!focus || box_is_focused(focus));

	XDO(xcb_input_xi_set_focus, conn,
			hand_get_wanted_focus(hand),
			XCB_CURRENT_TIME, hand->master_keyboard);
	XDO(xcb_input_xi_set_client_pointer, conn,
			focus ? focus->frame : XCB_WINDOW_NONE,
			hand->master_pointer);

	hand_update_barrier(hand);
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
box_has_any_hand_focus(Box const *const box)
{
	for_each_hand
		if (box == hand->focus)
			return true;

	return false;
}

static bool
box_is_visible(Box const *const box)
{
	bool const ret = !box->concealed || box->parent->focus_seq == box->focus_seq;
	if (!ret && box->parent->focus_seq == root->focus_seq)
		return box_has_any_hand_focus(box->parent);
	return ret;
}

static void
label_assign_all_hands(Label *const label)
{
	label->hands |= -1;
}

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

static bool
box_label_is_visible(Box const *const box)
{
	for_each_hand {
		switch (hand->mode) {
		case HAND_MODE_NULL:
			if (box->hide_label &&
			    /* Show labels for focused containers. */
			    !(box_is_container(box) && box_has_any_hand_focus(box)))
				return false;
			break;

		default:
			return true;
		}
	}

	return true;
}

static void
box_update_label(Box *const box)
{
	if (box == root)
		return;

	box_delete_labels(box);

	if (!box_label_is_visible(box))
		return;

	Label *label = label_new_for(box);
	if (!label)
		return;

	Hand const *naming = NULL;
	for_each_hand
		if (hand->mode_box == box &&
		    HAND_MODE_NAME == hand->mode)
		{
			naming = hand;
			break;
		}

	if (naming) {
		char display_name[membersizeof(Box, name) + 1];
		memcpy(display_name, naming->user_input, sizeof display_name);
		display_name[strnlen(display_name, sizeof display_name)] = '?';
		label_set_name(label, display_name);
	} else {
		label_set_name(label, box->name);
	}
	Point pt = box_compute_position(label->base, box_is_container(box) ? CENTER : RIGHT, TOP, false);
	if (box_is_container(box)) {
		Box const *const monitor = box_get_head(box);
		int const font_size = monitor_convert_pt2px(monitor,
				(Point){ 0, label_font_size }).y;

		pt.y -= font_size;
		for (Box const *b = box;
		     !b->floating &&
		     /* Labels would really obscure each other */
		     b->rect.y < b->parent->rect.y + font_size;
		     b = b->parent)
			pt.y += (box_label_is_visible(b->parent) ? font_size : 0) - (b->rect.y - b->parent->rect.y);
	}
	label_set_position(label, pt.x, pt.y);
	label->type = LABEL_BOX;
	label_assign_all_hands(label);
	label_update(label);
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
		XDO(xcb_delete_property, conn, window, ATOM(WM_STATE));
	else
		XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				window, ATOM(WM_STATE),
				ATOM(WM_STATE), 32, sizeof(xcb_icccm_wm_state_data_t) / sizeof(uint32_t),
				&(xcb_icccm_wm_state_data_t const){
					.state = state,
					.icon = XCB_WINDOW_NONE,
				});
}

static void
box_update(Box *const box);

static void
box_do_layout(Box const *const box)
{
	uint16_t n = box->num_visible;
	if (!n) {
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box *const child = box->children[i];
			if (!child->user_concealed)
				child->concealed = false;
		}
		return;
	}

	uint32_t min_focus_seq = UINT32_MAX;

	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box *const child = box->children[i];
		if ((child->user_concealed && child->concealed) ||
		    child->floating)
			continue;

		if (!n) {
			/* May be equal only if never focused (= 0). */
			if (child->focus_seq <= min_focus_seq)
				continue;

			uint32_t new_min_focus_seq = child->focus_seq;

			/* Find second least. */
			for (uint16_t j = 0; j < i; ++j) {
				Box *const child = box->children[j];
				if ((!child->user_concealed ||
				     !child->concealed) &&
				    min_focus_seq < child->focus_seq &&
				    child->focus_seq < new_min_focus_seq)
					new_min_focus_seq = child->focus_seq;
			}

			min_focus_seq = new_min_focus_seq;
		} else {
			if (child->focus_seq < min_focus_seq)
				min_focus_seq = child->focus_seq;
			--n;
		}
	}

	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box *const child = box->children[i];
		if (!child->user_concealed && !child->floating) {
			bool b = child->focus_seq < min_focus_seq;
			child->concealed = b;
		}
	}
}

static void
box_update_layout(Box const *const box)
{
	box_do_layout(box);

	uint16_t tiles = 0;

	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box *const child = box->children[i];
		if ((child->flagged = box_is_visible(child)) &&
		    !child->floating)
			++tiles;
	}

	xcb_rectangle_t tile = { 0 };
	uint16_t num_columns, num_rows;
	if (!box->vertical) {
		num_columns = compute_num_columns(&(xcb_rectangle_t const){
			.width =  box->rect.width - tile.x,
			.height = box->rect.height - tile.y,
		}, tiles);
		num_rows = (tiles + num_columns - 1) / num_columns;
	} else {
		num_rows = compute_num_columns(&(xcb_rectangle_t const){
			.width =  box->rect.height - tile.y,
			.height = box->rect.width - tile.x,
		}, tiles);
		num_columns = (tiles + num_rows - 1) / num_rows;
	}

	bool const is_monitor = box_is_monitor(box);

	for (uint16_t i = 0; i < box->num_children; ++i) {
		Box *const child = box->children[i];
		if (!child->floating && child->flagged) {
			uint16_t *num = !box->vertical ? &num_columns : &num_rows;
			if ((!box->vertical ? !tile.x : !tile.y) && tiles < *num)
				*num = tiles;

			tile.width = box->rect.width / num_columns;
			tile.height = box->rect.height / num_rows;

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

			if (!tile.x)
				gap.left = is_monitor ? MONITOR_GAP : 0;
			if (box->rect.width < tile.x + 2 * tile.width)
				gap.right = is_monitor ? MONITOR_GAP : 0;

			if (!tile.y)
				gap.top = is_monitor ? MONITOR_GAP : 0;
			if (box->rect.height < tile.y + 2 * tile.height)
				gap.bottom = is_monitor ? MONITOR_GAP : 0;

#define CORRECT_PIXEL(width, num_columns) do { \
uint16_t error = box->rect.width % num_columns; \
if (error) { \
error = box->rect.width / error; \
tile.width += (tile.x / error) != ((tile.x + tile.width + 1 /* Corrected pixel. */) / error); \
} \
} while (0)
			CORRECT_PIXEL(width, num_columns);
			CORRECT_PIXEL(height, num_rows);
#undef CORRECT_PIXEL

			/* Window dimensions need a slight of adjustments:
			 * - Last children eats up all
			 *   remaining space to ensure pixel
			 *   perfect display.
			 * - Size is rounded to the nearest
			 *   mod_{x,y} just because.
			 */
#define ADJUST_SIZE(num_rows, x, width) tile.width = ( \
	1 == tiles || box->rect.width + 1 /* Pixel correction overrun. */ < tile.x + 2 * tile.width \
	? box->rect.width - tile.x \
	: tile.width + (1 == num_rows && 1 < tiles && 0 < child->mod_##x \
	? child->mod_##x / 2 - (tile.width + child->mod_##x / 2) % child->mod_##x \
	: 0))
			/* TODO: Distribute error. */
			ADJUST_SIZE(num_rows, x, width);
			ADJUST_SIZE(num_columns, y, height);
#undef ADJUST_SIZE

			box_set_position(child, box->rect.x + tile.x + gap.left, box->rect.y + tile.y + gap.top);
			box_set_size(child, tile.width - (gap.left + gap.right), tile.height - (gap.top + gap.bottom));

			--tiles;
			if (!box->vertical) {
				tile.x += tile.width;
				if (box->rect.width <= tile.x)
					tile.x = 0, tile.y += tile.height;
			} else {
				tile.y += tile.height;
				if (box->rect.height <= tile.y)
					tile.y = 0, tile.x += tile.width;
			}
		} else if (!child->flagged) {
			box_set_size(child, 0, 0);
		} else if (child->floating) {
			xcb_rectangle_t rect = child->urect;

			int16_t cx = rect.x + rect.width / 2,
			        cy = rect.y + rect.height / 2;

			cx = MAX(cx, box->rect.x);
			cx = MIN(cx, box->rect.x + box->rect.width);
			cy = MAX(cy, box->rect.y);
			cy = MIN(cy, box->rect.y + box->rect.height);

			rect.x = cx - rect.width / 2;
			rect.y = cy - rect.height / 2;

			box_set_position(child, rect.x, rect.y);
			box_set_size(child, rect.width, rect.height);
		}

		box_update(child);
	}
}

static void
box_update_net(Box const *const box)
{
	xcb_atom_t list[10];
	uint32_t i = 0;

	if (!box->rect.width)
		list[i++] = ATOM(_NET_WM_STATE_HIDDEN);
	else if (box->focus_seq == root->focus_seq)
		list[i++] = ATOM(_NET_WM_STATE_FOCUSED);

	if (box->net_fullscreen)
		list[i++] = ATOM(_NET_WM_STATE_FULLSCREEN);

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			box->window, ATOM(_NET_WM_STATE),
			XCB_ATOM_ATOM, 32,
			i, list);
}

static bool
box_update_shape(Box const *const box)
{
	if (!BORDER_RADIUS ||
	    (!BORDER_RADIUS_FOR_SHAPED && box->shaped))
		return false;

	xcb_pixmap_t const pid = xcb_generate_id(conn);
	xcb_gcontext_t const cid = xcb_generate_id(conn);

	XDO(xcb_create_pixmap, conn,
			1, /* Mask is on or off thus 1 bit. */
			pid,
			box->frame,
			box->rect.width, box->rect.height);
	XDO(xcb_create_gc, conn, cid, pid, XCB_GC_FOREGROUND,
			(uint32_t const[]){ true });

	uint16_t r = BORDER_RADIUS;
	r = MIN(r, box->rect.width / 2);
	r = MIN(r, box->rect.height / 2);

	/* Cross between quarter arcs. */
	XDO(xcb_poly_fill_rectangle, conn, pid, cid, 2,
			(xcb_rectangle_t const[]){
				{
					.x = 0,
					.y = r,
					.width = box->rect.width,
					.height = 2 * r < box->rect.height ? box->rect.height - 2 * r : 0,
				},
				{
					.x = r,
					.y = 0,
					.width = 2 * r < box->rect.width ? box->rect.width - 2 * r : 0,
					.height = box->rect.height,
				}
			});

	/* Corners. */
	XDO(xcb_poly_fill_arc, conn, pid, cid, 4,
			(xcb_arc_t const[]){
				{ 0,                       0,                        2 * r, 2 * r, 1 * 90 * 64, 90 * 64 },
				{ box->rect.width - 2 * r, 0,                        2 * r, 2 * r, 0 * 90 * 64, 90 * 64 },
				{ box->rect.width - 2 * r, box->rect.height - 2 * r, 2 * r, 2 * r, 3 * 90 * 64, 90 * 64 },
				{ 0,                       box->rect.height - 2 * r, 2 * r, 2 * r, 2 * 90 * 64, 90 * 64 },
			});
	XDO(xcb_shape_mask, conn,
			XCB_SHAPE_SK_BOUNDING, XCB_SHAPE_SK_BOUNDING,
			box->frame,
			0, 0, /* Offset. */
			pid);
	XDO(xcb_free_pixmap, conn, pid);
	XDO(xcb_free_gc, conn, cid);

	if (box->shaped)
		/* Frame = client bounds /\ radius. */
		XDO(xcb_shape_combine, conn, XCB_SHAPE_SO_INTERSECT,
				XCB_SHAPE_SK_BOUNDING, XCB_SHAPE_SK_BOUNDING,
				box->frame,
				0, 0, /* Offset. */
				box->window);

	return true;
}

/**
 * Not simply floating but it or one of its parent is floating. Float root.
 */
static Box *
box_get_foot(Box const *box)
{
	do
		if (box->floating)
			return (Box *)box;
	while (!box_is_monitor((box = box->parent)));

	return NULL;
}

static void
box_update(Box *const box)
{
	static int depth = 0;

	if (box != root && 3 <= HEAWM_VERBOSE)
		printf(
				"%*.s%p %d (%.*s):"
				" 0x%x->0x%x/0x%x"
				" %ux%u+%d+%d u%ux%u+%d+%d"
				" H=%d C=%d V=%d %c%c%c%c"
				" (%s %s)\"%.15s\" "
				"\n",
				depth, "",
				(void *)box,

				box->focus_seq,

				(int)sizeof box->name, box->name,

				box->leader, box->frame, box->window,

				box->rect.width, box->rect.height, box->rect.x, box->rect.y,
				box->urect.width, box->urect.height, box->urect.x, box->urect.y,

				box->focus_hand,
				box->conceal_seq,
				box->num_visible,
				box->floating ? 'F' : '-',
				box->focus_lock ? 'L' : '-',
				box->user_concealed ? 'U' : '-',
				box->concealed ? 'C' : '-',

				box->class,
				box_get_class_instance(box),
				box->title);

	depth += 2;

	uint32_t mask = 0;
	uint32_t list[7];
	uint8_t i = 0;

	bool const content_changed = box->content_changed;
	bool const position_changed = box->position_changed;
	bool const should_focus = box->should_focus;
	bool const layout_changed = box->layout_changed;
	bool const should_map = box->should_map;
	bool const label_changed = box->label_changed;

	box->position_changed = false;
	box->layout_changed = false;
	box->should_map = false;
	box->content_changed = false;
	box->should_focus = false;
	box->label_changed = false;

	if (position_changed) {
		mask |=
			XCB_CONFIG_WINDOW_X |
			XCB_CONFIG_WINDOW_Y;
		list[i++] = box->rect.x;
		list[i++] = box->rect.y;
	}

	if ((position_changed || layout_changed) && box->parent) {
		if (0 < box->rect.width) {
			mask |=
				XCB_CONFIG_WINDOW_WIDTH |
				XCB_CONFIG_WINDOW_HEIGHT;
			list[i++] = box->rect.width;
			list[i++] = box->rect.height;

			box_update_layout(box);
		} else {
			box_delete_labels(box);
			delete_barrier(box->barrier);

			if (XCB_WINDOW_NONE != box->window) {
				xcb_icccm_set_wm_state(box->window, XCB_ICCCM_WM_STATE_ICONIC);
				box_update_net(box);
				XDO(xcb_unmap_window, conn, box->frame);
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

	if (!box->rect.width)
		goto out;

	if (!box_is_container(box)) {
		if (0 < i) {
			Box const *const foot = box_get_foot(box);
			if (foot) {
				mask |= XCB_CONFIG_WINDOW_SIBLING;
				Body const *const body = &bodies[box->body];
				list[i++] = root->focus_seq == foot->focus_seq
					? body->label_layer
					: body->float_layer;
			}

			mask |= XCB_CONFIG_WINDOW_STACK_MODE;
			list[i++] = XCB_STACK_MODE_BELOW;
			XDO(xcb_configure_window, conn, box->frame, mask, list);

			if (layout_changed || position_changed)
				box_update_barrier(box, NULL);

			if (layout_changed) {
				box_update_shape(box);

				/* Update window size inside frame. */
				mask = XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT;
				list[0] = box->rect.width;
				list[1] = box->rect.height;
				XDO(xcb_configure_window, conn, box->window, mask, list);
			}
		}

		/* Map only after configured. */
		if (should_map) {
			xcb_icccm_set_wm_state(box->window, XCB_ICCCM_WM_STATE_NORMAL);
			box_update_net(box);
			XDO(xcb_map_window, conn, box->frame);
		}

		if ((layout_changed || position_changed || should_focus) && box_is_focused(box) && !box_is_container(box)) {
			box_update_net(box);

			/* Hand focus can only be updated after window is mapped. */
			for_each_hand {
				Box const *const focus = hand->input_focus;
				if (box == focus) {
					if (should_focus)
						hand_focus(hand);
					else
						hand_update_barrier(hand);
				}
			}
		}
	}

	if (layout_changed || position_changed || label_changed)
		box_update_label(box);

out:
	depth -= 2;
}

static void
box_update_net_client_list(Box const *const box)
{
	Body const *const body = &bodies[box->body];

	/* Unneccessary to add since we will update the whole list at the update
	 * call. */
	if (body->net_client_list_changed)
		return;

	XDO(xcb_change_property, conn, XCB_PROP_MODE_APPEND,
			body->screen->root, ATOM(_NET_CLIENT_LIST),
			XCB_ATOM_WINDOW, 32,
			1, &box->window);
}

static void
body_clear_net_client_list(Body const *const body)
{
	XDO(xcb_delete_property, conn, body->screen->root, ATOM(_NET_CLIENT_LIST));
}

static void
body_update_net_client_list(Body *const body)
{
	if (!body->net_client_list_changed)
		return;

	xcb_window_t list[1 << 16];
	uint32_t i = 0;
	xcb_prop_mode_t mode = XCB_PROP_MODE_REPLACE;

	static uint32_t max_num_windows = 0;
	if (!max_num_windows) {
		max_num_windows = xcb_get_maximum_request_length(conn) - sizeof(xcb_change_property_request_t) / sizeof(uint32_t);
		if (ARRAY_SIZE(list) < max_num_windows)
			max_num_windows = ARRAY_SIZE(list);
	}

	Box *box;
	for_each_box(box, root)
		if ((body - bodies) == box->body && !box_is_container(box)) {
			list[i++] = box->window;
			if (max_num_windows == i) {
				XDO(xcb_change_property, conn, mode,
						body->screen->root, ATOM(_NET_CLIENT_LIST),
						XCB_ATOM_WINDOW, 32,
						i, list);
				i = 0;
				mode = XCB_PROP_MODE_APPEND;
			}
		}

	if (0 < i)
		XDO(xcb_change_property, conn, mode,
				body->screen->root, ATOM(_NET_CLIENT_LIST),
				XCB_ATOM_WINDOW, 32,
				i, list);

	body->net_client_list_changed = false;
}

static void
body_update_net(Body *const body)
{
	body_update_net_client_list(body);
}

/**
 * Free resources associated with unused labels.
 *
 * Do cleanup of labels in a separater step instead of after do_update()
 * because this way it provides a little debounce in case number of labels
 * changes frequently, e.g. toggling maximization.
 */
static void
labels_do_gc(void)
{
	for_each_body {
		/* Allow equality so zero elements will get cleaned up. */
		uint32_t half = body->num_labels_created / 2;
		if (body->num_labels_mapped <= half) {
			while (half < body->num_labels_created) {
				Label *label = &body->labels[--body->num_labels_created];
				label_destroy(label);
			}

			/* Labels above created, contain no data to be cleaned up
			 * so we can simply shrink the memory. */
			half = body->num_labels / 2;
			if (body->num_labels_created <= half) {
				void *const p = realloc(body->labels, half * sizeof *body->labels);
				if (p || !half) {
					body->labels = p;
					body->num_labels = half;
				}
			}
		}
	}
}

/**
 * Free unused resources on X server.
 */
static void
do_gc(void)
{
	labels_do_gc();
}

static unsigned
get_rect_overlap(xcb_rectangle_t const *const r, xcb_rectangle_t const *const s) {
#define COMPUTE(x, width) \
	int16_t d##x = (MIN(r->x + r->width, s->x + s->width) - MAX(r->x, s->x)); \
	if (d##x <= 0) \
		return 0;
	REPEAT_XY(COMPUTE);
#undef COMPUTE
	return 1024UL * (dx * dy) / (r->width * r->height);
}

/* When box->parent is hidden it will be just a best-effort "approximation". */
static xcb_rectangle_t const *
box_get_parent_bounds(Box const *const box)
{
	Box const *parent = box;
	do
		parent = parent->parent;
	while (!parent->rect.width);
	return &parent->rect;
}

static xcb_rectangle_t
box_get_chase_rect(Box const *const box)
{
	xcb_rectangle_t const *bounds = box_get_parent_bounds(box);

	/* Mirror X/Y around center of parent. */
	return (xcb_rectangle_t){
		.x = bounds->x + (bounds->x + bounds->width - (box->urect.x + box->urect.width)),
		.y = bounds->y + (bounds->y + bounds->height - (box->urect.y + box->urect.height)),
		.width = box->urect.width,
		.height = box->urect.height,
	};
}

static void
box_chase_box(Box const *box, unsigned const min_percent)
{
	if (box_get_foot(box))
		return;

	for (Box const *parent = box->parent;; parent = parent->parent) {
		for (uint16_t i = 0; i < parent->num_children; ++i) {
			Box *const child = parent->children[i];
			if (!child->floating)
				continue;

			xcb_rectangle_t const target = box_get_chase_rect(child);
			unsigned const percent = get_rect_overlap(&box->rect, &child->rect);
			if (min_percent <= percent &&
			    get_rect_overlap(&box->rect, &target) < percent)
			{
				box_set_urect(child, target);
				box_propagate_change(child);
			}
		}

		if (box_is_monitor(parent))
			break;
	}
}


static void
hand_update_focus(Hand const *const hand)
{
	if (hand->input_focus)
		box_chase_box(hand->input_focus, 1024 / 15);

	if (hand != hands)
		return;

	for_each_body
		XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				body->screen->root, ATOM(_NET_ACTIVE_WINDOW),
				XCB_ATOM_WINDOW, 32,
				1, hand->input_focus && body_index == hand->input_focus->body
					? &hand->input_focus->window
					: &(uint32_t const){ XCB_WINDOW_NONE });
}

/**
 * Send changes to X server.
 */
static void
do_update(void)
{
	do {
		while (root->content_changed)
			box_update(root);

		if (!hands_changed)
			break;
		hands_changed = false;

		for_each_hand {
			if (hand->focus_changed) {
				hand->focus_changed = false;
				hand_update_focus(hand);
			}
		}
	} while (root->content_changed);

	for_each_body {
		assert(body->num_labels_used <= body->num_labels_mapped);
		while (body->num_labels_used < body->num_labels_mapped) {
			Label *label = &body->labels[--body->num_labels_mapped];
			XDO(xcb_unmap_window, conn, label->window);
		}

		body_update_net(body);
	}
}

/* Must be called after box children have changed. */
static void
box_update_children(Box *box)
{
	assert(box_is_container(box));

	uint32_t child_focus_seq = box->focus_seq;

	box->layout_changed = true;

	do {
		uint32_t max_focus_seq = 0;
		for (uint16_t i = 0; i < box->num_children; ++i) {
			Box *const child = box->children[i];
			if (max_focus_seq < child->focus_seq)
				max_focus_seq = child->focus_seq;
		}
		/* We unparented the focused box so maximum dropped. */
		if (box->focus_seq == max_focus_seq)
			break;

		box->content_changed = true;
		/* If we going up along the focused path, child_focus_seq ==
		 * box's currently focused child so we can avoid layout update. */
		box->layout_changed |= box->focus_seq != child_focus_seq;

		child_focus_seq = box->focus_seq;
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

	uint16_t const child_pos = child - parent->children;

	memmove(
		child,
		child + 1,
		(--parent->num_children - child_pos) * sizeof *child
	);

	box_update_children(parent);
	box->parent = NULL;

	return child_pos;
}

static void box_vacuum(Box *box);

static void
hand_focus_box(Hand *hand, Box *box);

/**
 * Fill |*boxes| with the n latest focused box associated with hand
 */
static void
hand_find_recents(Hand const *const hand, Box *root, uint32_t const focus_seq, Box **const boxes, uint32_t const n)
{
	static Box const EMPTY_BOX;

	/* Use a dummy box so we do not have to check for NULL. */
	for (uint32_t i = 0; i < n; ++i)
		/* Cast is safe because we do not write boxes and they
		 * will be NULLed out at the end. */
		boxes[i] = (Box *)&EMPTY_BOX;

	Box *box;
	for_each_box(box, root)
		if (/* Only client windows are interesting. */
		    !box_is_container(box) &&
		    (NULL_HAND == box->focus_hand ||
		     (/* Not focused. */
		      focus_seq != box->focus_seq &&
		      /* But have been focused by us. */
		      hand - hands == box->focus_hand) ||
		      box == hand->input_focus))
		{
			uint32_t i = n;
			while (i > 0 && boxes[i - 1]->focus_seq <= box->focus_seq)
				--i;
			if (i < n) {
				memmove(&boxes[i + 1], &boxes[i], (n - (i + 1)) * sizeof *boxes);
				boxes[i] = box;
			}
		}

	for (uint32_t i = n; 0 < i && &EMPTY_BOX == boxes[--i];)
		boxes[i] = NULL;
}

/**
 * Make sure all hand has focused window.
 */
static void
hands_try_focus_all(uint32_t focus_seq)
{
	for_each_hand {
		if (hand->input_focus)
			continue;

		Box *optimum;
		hand_find_recents(hand, root, focus_seq, &optimum, 1);

		if (optimum) {
			hand_focus_box(hand, optimum);
			focus_seq = root->focus_seq;
		}
	}
}

/**
 * Increment highest focus number after any of the hands focus changed.
 */
static void
hands_update_focus(void)
{
	uint32_t const old_focus_seq = root->focus_seq++;

	for_each_hand {
		Box *box;

		if (!(box = hand->input_focus) &&
		    !(box = hand->focus))
			continue;

		box->focus_hand = hand_index;

		bool child_changed = false;
		bool changed = false;
		do {
			box->content_changed |= changed;
			box->layout_changed |= child_changed;
			changed |= (child_changed = box->focus_seq != old_focus_seq);
			box->focus_seq = root->focus_seq;
		} while ((box = box->parent));
	}
}


static void
hand_do_mode_changes(Hand *const hand)
{
	Box *box;
	for_each_box(box, root) {
		box->label_changed = true;
		box->content_changed = true;
	}

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

	hand->mode = HAND_MODE_NULL;
}

static void
box_free(Box *const box)
{
	free(box->title);
	free(box->class);
	free(box);
}

static void
box_delete(Box *box)
{
	/* If we removed the currently selected box, and there is only one
	 * hand, root->focus_seq will dropped that could make a previously
	 * selected seems like it's selected (possibly by someone other). */
	uint32_t const real_focus_seq = root->focus_seq;
	bool focus_changed = false;

	assert(!box->num_children);

	for_each_hand {
		if (box == hand->mode_box)
			hand_leave_mode(hand);

		/* Forget current focus. */
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

	delete_barrier(box->barrier);
	box_delete_labels(box);

	Box *const box_parent = box->parent;
	box_unparent(box);

	if (focus_changed) {
		/* Strictly after unparent: if there is only one hand and we
		 * have just now deleted its focused box, layout may not be
		 * updated properly, because after unparent the maximum
		 * focus_seq is not focused in any hand. and when we try to
		 * focus that box it seems like it has been already focused.
		 * since root->focus_seq == that_box->focus_seq. */
		hands_update_focus();
		hands_try_focus_all(real_focus_seq);
	}

	if (!box_is_container(box)) {
		XDO(xcb_shape_select_input, conn, box->window, false);
		XDO(xcb_change_window_attributes, conn, box->window,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					XCB_NONE
				});
		xcb_icccm_set_wm_state(box->window, XCB_ICCCM_WM_STATE_WITHDRAWN);
		XDO(xcb_reparent_window, conn, box->window, bodies[box->body].screen->root, 0, 0);
		XDO(xcb_destroy_window, conn, box->frame);
		XDO(xcb_change_save_set, conn, XCB_SET_MODE_DELETE, box->window);

		bodies[box->body].net_client_list_changed = true;
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
	assert(box_is_container(box));

	/* Keep container. */
	if (!box->parent ||
	    box_is_monitor(box) ||
	    1 < box->num_children)
		return;

	/* Delete empty. */
	else if (!box->num_children)
		box_delete(box);

	/* Substitute with its only children. */
	else if (box_is_container(box->children[0]) ||
	         box->floating ||
	         !box_is_super_container(box->parent))
	{
		Box *const child = box->children[0];
		if (box_is_container(child))
			memcpy(child->name, box->name, sizeof box->name);
		child->user_concealed = box->user_concealed;
		child->concealed = box->concealed;
		child->conceal_seq = box->conceal_seq;
		child->hide_label |= box->hide_label;
		child->floating = box->floating;
		child->urect = box->urect;
		box_reparent(box->parent, box_get_pos(box), child);
	}
}

static void
box_realloc(Box **const box, size_t const new_size)
{
	Box *const old = *box;
	Box *const new = check_alloc(realloc(old, new_size));

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
box_reparent(Box *into, uint16_t pos, Box *const box)
{
	assert(box_is_container(into));
	assert(into != box);
	assert(root != box);

	Box *const old_parent = box->parent;

	uint16_t const box_pos = box_unparent(box);
	if (old_parent == into)
		pos -= box_pos < pos;

	box_realloc(&into, offsetof(Box, children[into->num_children + 1]));
	box->parent = into;

	/* Inherit some properties from parent. */
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

	box_update_children(into);
	box_name(box);
}

__attribute__((malloc))
static Box *
box_new(void)
{
	Box *box = check_alloc(calloc(1, sizeof *box + num_hands * sizeof(BoxPointer)));
	box->focus_hand = NULL_HAND;
	/* An initially zeroed out box->{x,y} does not mean box->window is
	 * really at this position so marking it changed will force a
	 * reconfiguration. */
	box->position_changed = true;
	box->vertical = true;
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

	/* Create leg. */
	if (box_is_monitor(into) &&
	    !box_is_container(box) &&
	    !box->floating)
	{
		{
			Box *const container = box_new();
			box_set_placeholder_name(container);
			box_reparent(into, pos, container);
			box_reparent(container, 0, box);
		}
		box_clear_name(box->parent);
		box_name(box->parent);
		return;
	}

	box_reparent(into, pos, box);
}

static void
box_swap(Box *const x, Box *const y)
{
	if (!x || !y)
		return;

	if (box_is_descendant(x, y) ||
	    box_is_descendant(y, x))
		return;

	/* Heads may only be swapped with containers. */
	if ((box_is_monitor(x) && !box_is_container(y)) ||
	    (box_is_monitor(y) && !box_is_container(x)))
		return;

	Box **px, **py;
	for (px = x->parent->children; *px != x; ++px);
	for (py = y->parent->children; *py != y; ++py);
	*px = y, *py = x;

#define SWAP(type, member) do { \
	type const t = x->member; \
	x->member = y->member; \
	y->member = t; \
} while (0)

	SWAP(Box *, parent);
	SWAP(xcb_rectangle_t, urect);
	SWAP(bool, floating);

#undef SWAP

	xcb_rectangle_t xrect = x->rect, yrect = y->rect;
	box_set_size(x, yrect.width, yrect.height);
	box_set_position(x, yrect.x, yrect.y);
	box_set_size(y, xrect.width, xrect.height);
	box_set_position(y, xrect.x, xrect.y);

	box_update_children(x->parent);
	box_update_children(y->parent);

	/* E.g. Empty monitor has been swapped. */
	if (box_is_container(x))
		box_vacuum(x);
	if (box_is_container(y))
		box_vacuum(y);
}

/* TODO: Maybe make hand_grab_* event driven (hand_update_mode and maybe improve that also). */
static void
hand_grab_keyboard(Hand const *const hand)
{
	for_each_body {
		xcb_window_t const root_window = body->screen->root;

		if (HAND_MODE_NULL == hand->mode) {
			XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(root_window,
					hand->master_keyboard,
					XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
					{ XCB_INPUT_MODIFIER_MASK_ANY });

			XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
					XCB_CURSOR_NONE,
					hand->master_keyboard,
					XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
					XCB_INPUT_GRAB_MODE_22_SYNC,
					XCB_INPUT_GRAB_OWNER_NO_OWNER,
					XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
					{
						EFFECTIVE_MASK(XCB_MOD_MASK_4 | 0),
						EFFECTIVE_MASK(XCB_MOD_MASK_4 | XCB_MOD_MASK_SHIFT),
						EFFECTIVE_MASK(XCB_MOD_MASK_4 | XCB_MOD_MASK_CONTROL),
						EFFECTIVE_MASK(XCB_MOD_MASK_4 | XCB_MOD_MASK_1),
					});
		} else {
			XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
					XCB_CURSOR_NONE,
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
		if (hand->focus != hand->input_focus) {
			box_propagate_change(hand->focus)->label_changed = true;
			hand->focus->layout_changed = true;
			hand->focus = hand->input_focus;
		}

		if (hand->latest_input[0] != hand->input_focus) {
			hand->latest_input[1] = hand->latest_input[0];
			hand->latest_input[0] = hand->input_focus;
		}
	}
}

static void
box_propagate_labels_change(Box *const box)
{
	/* Labels may get shifted upwards or downwards when focus changes on a
	 * box with hidden label. */
	if (!box->hide_label)
		return;

	Box *b;
	for_each_box(b, box)
		if (box_is_container(b)) {
			b->label_changed = true;
			/* We walk every box so there will be no discontinuities. */
			b->parent->content_changed = true;
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

	if (hand->focus) {
		box_propagate_labels_change(hand->focus);
		hand->focus->layout_changed |= box_is_container(hand->focus);
	}

	/* Find most upper locked box. */
	Box *locked = box;
	for (Box *b = box; (b = b->parent);)
		if (b->focus_lock)
			locked = b;

	Box *recents[2];
	hand_find_recents(hand, locked, root->focus_seq, recents, ARRAY_SIZE(recents));
	if (4 <= HEAWM_VERBOSE)
		printf("recents: %s <-> %s  new: %s  locked: %s\n",
				recents[0] ? recents[0]->name : "?",
				recents[1] ? recents[1]->name : "?",
				box->name,
				locked ? locked->name : "?");
	if (locked == box ||
	    recents[0] == box)
		recents[1] = NULL;

	box_swap(recents[1], recents[0]);

	Box *const new_input_focus = !box_is_container(box)
		? box
		: recents[0];

	hand->focus = box;

	/* Avoid unintuitive pointer movements. */
	if (hand->input_focus &&
	    new_input_focus &&
	    (/* ...if window under pointer would be focused; */
	     new_input_focus->frame == Box_pointers(hand->input_focus)[hand - hands].window ||
	     /* ...if mouse would be warped in the same window. */
	     Box_pointers(new_input_focus)[hand - hands].window == Box_pointers(hand->input_focus)[hand - hands].window))
		memcpy(
				&Box_pointers(new_input_focus)[hand - hands],
				&Box_pointers(hand->input_focus)[hand - hands],
				sizeof(BoxPointer));

	if ((hand->input_focus = new_input_focus))
		hand->input_focus->should_focus = true;

	box->label_changed = true;
	box->layout_changed |=
		/* All children must be shown. */
		box_is_container(box) ||
		/* Must be restacked. */
		box->floating;
	hands_update_focus();

	box_swap(box, recents[1]);

	hand->check_input = true;
	/* hand_grab_keyboard(hand); */

	box_propagate_labels_change(hand->focus);

	hands_changed = true;
	hand->focus_changed = true;
}

static void
hand_focus_box(Hand *const hand, Box *const box)
{
	/* Focus same-named legs. */
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
box_set_class(Box *const box, xcb_get_property_reply_t const *const reply)
{
	int const len = xcb_get_property_value_length(reply);
	char const *const class = xcb_get_property_value(reply);
	char const *delim;

	free(box->class);
	box->class = NULL;

	if (len < (int)XCB_STRING_MAX &&
	    (delim = memchr(class, '\0', len)) &&
	    class + len - 1 == memchr(delim + 1, '\0', len - ((delim + 1) - class)))
		if ((box->class = malloc(len)))
			memcpy(box->class, class, len);
}

static void
debug_print_atom_name(char const *const name, xcb_atom_t const atom)
{
	if (2 <= HEAWM_VERBOSE) {
		XCB_GET_REPLY(reply, xcb_get_atom_name, atom);
		printf("%s %.*s\n", name, xcb_get_atom_name_name_length(reply), xcb_get_atom_name_name(reply));
	}
}

typedef union {
	xcb_window_t window;
	Box *box;
} BoxOrWindow;

static xcb_get_property_cookie_t
box_update_property(BoxOrWindow box, xcb_atom_t const property, xcb_get_property_cookie_t cookie, bool const from_event)
{
	xcb_get_property_reply_t *reply;
	/* They are set just before we start the second loop. */
	void *data = data;
	int len = len;
	xcb_atom_t type = XCB_ATOM_NONE;

process_reply:
	if (XCB_ATOM_WM_NAME == property || ATOM(_NET_WM_NAME) == property) {
		if (!type) {
			type = XCB_ATOM_WM_NAME == property ? XCB_ATOM_STRING : ATOM(UTF8_STRING);
			len = XCB_STRING_MAX / sizeof(uint32_t);
			goto send_request;
		}

		free(box.box->title);
		if ((box.box->title = malloc(len + 1 /* NUL */))) {
			memcpy(box.box->title, data, len);
			box.box->title[len] = '\0';
		}
	} else if (ATOM(WM_NORMAL_HINTS) == property) {
		if (!type) {
			type = ATOM(WM_SIZE_HINTS);
			len = sizeof(xcb_size_hints_t) / sizeof(uint32_t);
			goto send_request;
		}

		xcb_size_hints_t const *const hints = data;

		if (sizeof *hints == len) {
			if (XCB_ICCCM_SIZE_HINT_P_RESIZE_INC & hints->flags) {
				box.box->mod_y = hints->height_inc;
				box.box->mod_x = hints->width_inc;
			}
		} else {
			box.box->mod_y = 0;
			box.box->mod_x = 0;
		}
	} else if (ATOM(WM_CLIENT_LEADER) == property) {
		if (!type) {
			type = XCB_ATOM_WINDOW;
			len = sizeof(xcb_window_t) / sizeof(uint32_t);
			goto send_request;
		}

		box.box->leader = sizeof box.box->leader == len
			? *(xcb_window_t *)data
			: XCB_WINDOW_NONE;
	} else if (XCB_ATOM_WM_CLASS == property) {
		if (!type) {
			type = XCB_ATOM_STRING;
			len = XCB_STRING_MAX / sizeof(uint32_t);
			goto send_request;
		}

		box_set_class(box.box, reply);
	} else if (ATOM(_NET_WM_STATE) == property) {
		if (!type) {
			type = XCB_ATOM_ATOM;
			len = 10 * sizeof(xcb_atom_t);
			goto send_request;
		}

		box.box->net_fullscreen = false;

		while (len) {
			len -= sizeof(xcb_atom_t);
			xcb_atom_t const atom = *(xcb_atom_t const *)((char *)data + len);
			if (5 <= HEAWM_VERBOSE)
				debug_print_atom_name("_NET_WM_STATE[] =", atom);
			box.box->net_fullscreen |= ATOM(_NET_WM_STATE_FULLSCREEN) == atom;
		}
	} else if (ATOM(_HEAWM_NAME) == property) {
		if (!type) {
			type = XCB_ATOM_STRING;
			len = membersizeof(Box, name);
			goto send_request;
		}

		if ((int)membersizeof(Box, name) < len) {
			fprintf(stderr, "0x%x._HEAWM_NAME = \"%.*s\" is longer than %zu bytes\n",
					box.box->window,
					len, (char *)data,
					membersizeof(Box, name));
		} else {
			memcpy(box.box->name, data, len);
			memset(box.box->name + len, 0, sizeof box.box->name - len);
			if (len < (int)membersizeof(Box, name))
				box.box->name[len] = '\0';
		}
	} else {
		if (5 <= HEAWM_VERBOSE)
			debug_print_atom_name("property", property);
		return (xcb_get_property_cookie_t){ 0 };
	}

	free(reply);
	return (xcb_get_property_cookie_t){ 0 };

send_request:
	if (from_event && !(box.box = box_find_by_window(root, offsetof(Box, window), box.window)))
		return (xcb_get_property_cookie_t){ 0 };

	if (!cookie.sequence) {
		cookie = xcb_get_property(conn, 0, box.box->window, property, (type), 0 /* offset */, len);
		if (!from_event)
			return cookie;
	}

	if (!(reply = xcb_get_property_reply(conn, cookie, NULL)))
		return (xcb_get_property_cookie_t){ 0 };

	data = xcb_get_property_value(reply);
	len = xcb_get_property_value_length(reply);
	goto process_reply;
}

static void
box_reparent_into(Box *const parent, Box *const child)
{
	if (!box_is_container(parent)) {
		Box *container = box_new();

		if (box_get_foot(parent))
			child->floating = false;

		if ((container->floating = parent->floating)) {
			parent->floating = false;
			container->urect = parent->urect;
		}

		box_reparent_checked(parent->parent, box_get_pos(parent), container);

		box_reparent_checked(container, 0, parent), container = parent->parent;
		box_reparent_checked(container, 1, child);

		container->num_visible = 1;
	} else {
		box_reparent_checked(parent, 0, child);
	}
}

static void
box_set_floating(Box *const box, bool floating)
{
	if (floating == box->floating)
		return;
	box->parent->layout_changed = true;
	if (!box->user_concealed)
		box->concealed = false;
	box_propagate_change(box)->floating = floating;
}

static void
box_resize_float(Box *const box, enum FloatResize how)
{
	if (box_is_monitor(box))
		return;

	xcb_rectangle_t rect;
	xcb_rectangle_t const *bounds = box_get_parent_bounds(box);

	if (FLOAT_UNCHANGED == how) {
		rect = box->urect;
		if (!rect.width || !rect.height)
			how = FLOAT_TILE0 + 5;

		if (!rect.x && !rect.y)
			rect.x = (bounds->width - rect.width) / 2,
			rect.y = (bounds->height - rect.height) / 2;
	}

	if (FLOAT_UNCHANGED == how)
		/* Nothing. */;
	else if (FLOAT_TILE0 == how)
		rect = (xcb_rectangle_t){
			.x = bounds->width * 3 / 16,
			.y = bounds->height * 3 / 16,
			.width = bounds->width * 5 / 8,
			.height = bounds->height * 5 / 8,
		};
	else if (FLOAT_TILE0 <= how && how <= FLOAT_TILE9)
		/* TODO: Distribute error. */
		rect = (xcb_rectangle_t){
			.x = (bounds->width / 3) * ((how - FLOAT_TILE1) % 3),
			.y =  (bounds->height / 3) * (2 - (how - FLOAT_TILE1) / 3),
			.width = bounds->width / 3,
			.height = bounds->height / 3,
		};
	else if (FLOAT_RECT == how)
		rect = box->rect;
	else
		abort();

	box_set_urect(box, rect);
	box_set_floating(box, true);
}

/**
 * Manage window.
 */
static Box *
box_window(xcb_window_t const root_window, xcb_window_t const window)
{
	Box *box = box_new();

	box->window = window;
	box->frame = xcb_generate_id(conn);

	struct {
		xcb_atom_t atom;
		xcb_get_property_cookie_t cookie;
	} properties[] = {
#define PROPERTY(atom) { (atom), (xcb_get_property_cookie_t){ 0 } }
		PROPERTY(ATOM(_NET_WM_STATE)),
		PROPERTY(XCB_ATOM_WM_TRANSIENT_FOR),
		PROPERTY(ATOM(WM_CLIENT_LEADER)),
		PROPERTY(ATOM(WM_NORMAL_HINTS)),
		PROPERTY(ATOM(_HEAWM_NAME)),
		PROPERTY(XCB_ATOM_WM_CLASS),
		PROPERTY(XCB_ATOM_WM_NAME),
		PROPERTY(ATOM(_NET_WM_NAME)),
#undef PROPERTY
	};

	xcb_get_geometry_cookie_t gg_cookie;

	for (int get = 1; 0 <= get; --get) {
		if (get)
			gg_cookie = xcb_get_geometry_unchecked(conn, window);
		else {
			xcb_get_geometry_reply_t *const reply =
				xcb_get_geometry_reply(conn, gg_cookie, NULL);
			if (reply) {
				box->urect = (xcb_rectangle_t){
					.x = reply->x,
					.y = reply->y,
					.width = reply->width,
					.height = reply->height,
				};

				free(reply);
			}
		}


		for (size_t i = 0; i < ARRAY_SIZE(properties); ++i)
			if (get || properties[i].cookie.sequence)
				properties[i].cookie = box_update_property((BoxOrWindow){ .box = box }, properties[i].atom, properties[i].cookie, false);
	}

	Hand *box_hand = NULL;
	Box *parent = NULL;
	uint32_t pos;
	bool focus = false;

	if (0 < num_hands) {
		Box *origin;
		origin = box_find_by_window(root, offsetof(Box, leader), box->leader);

		/* Find the hand that could possibly create this window. */
		if (origin) {
			/* Maybe a close dialog, maybe something other. */
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
			/* We use first hand even if its auto_focus is off. */
			box_hand = &hands[0];
			for (uint8_t i = 0; i < num_hands; ++i) {
				if (hands[i].want_focus || hands[i].want_popup) {
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
		/* Attach to the first (probably primary) monitor of body. */
		Body *body = body_get_by_root(root_window);
		Box **head = root->children;
		/* FIXME: Cause issues if body does not have any monitors. */
		while ((body - bodies) != (*head)->body)
			++head;

		parent = *head;
		while (parent->num_children && box_is_container(parent->children[0]))
			parent = parent->children[0];

		/* Place at the end. */
		pos = parent->num_children;
	}

	if (box_hand && box_hand->want_popup) {
		box_hand->want_popup = false;
		box_reparent_into(box_hand->input_focus ? box_hand->input_focus : parent, box);
		focus = true;
	} else if (parent->num_children && box_is_super_float(parent)) {
		box_reparent_checked(parent, pos, box);
		box_resize_float(box, FLOAT_UNCHANGED);
	} else {
		box_reparent_checked(parent, pos, box);
	}

	if (box_hand) {
		box_hand->want_focus = false;
		if (focus && HAND_MODE_NULL == box_hand->mode)
			hand_focus_box(box_hand, box);
	}

	Body *const body = &bodies[parent->body];
	XDO(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			box->frame,
			body->screen->root,
			-1, -1, 1, 1, /* Rect. */
			0, /* Border. */
			XCB_WINDOW_CLASS_INPUT_OUTPUT,
			XCB_COPY_FROM_PARENT,
			XCB_CW_OVERRIDE_REDIRECT |
			XCB_CW_EVENT_MASK,
			(uint32_t const[]){
				true,
				FRAME_WINDOW_EVENT_MASK
			});

	XDO(xcb_change_save_set, conn, XCB_SET_MODE_INSERT, window);

	if (XCHECK(xcb_reparent_window, conn, window, box->frame, 0, 0))
		goto fail;

	if (XCHECK(xcb_map_window, conn, window))
		goto fail;

	if (XCHECK(xcb_change_window_attributes, conn, window,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					CLIENT_WINDOW_EVENT_MASK
				}))
		goto fail;

	XDO(xcb_input_xi_select_events, conn, window, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
					XCB_INPUT_XI_EVENT_MASK_BARRIER_HIT |
					XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
					XCB_INPUT_XI_EVENT_MASK_ENTER));

	XDO(xcb_shape_select_input, conn, window, true);

	box_update_net_client_list(box);

	return box;

fail:
	box_delete(box);
	return NULL;
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

	/* Automatically clean up children. */
	sa.sa_handler = SIG_DFL;
	sigaction(SIGCHLD, &sa, NULL);

	/* Allow receiving every signal. */
	sigemptyset(&sa.sa_mask);
	pthread_sigmask(SIG_SETMASK, &sa.sa_mask, NULL);
}

static void
hands_update(void);

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
		fprintf(stderr, "XInput extension missing; input will not work\n");
	else
		xi_opcode = ext->major_opcode;

	ext = xcb_get_extension_data(conn, &xcb_randr_id);
	if (!ext->present)
		fprintf(stderr, "RandR extension missing; multi-head display will not work properly\n");
	else
		randr_base_event = ext->first_event;

	ext = xcb_get_extension_data(conn, &xcb_shape_id);
	if (!ext->present)
		fprintf(stderr, "Shape extension missing; labels cannot be made transparent\n");
	else
		shape_base_event = ext->first_event;

	ext = xcb_get_extension_data(conn, &xcb_xkb_id);
	if (!ext->present) {
		fprintf(stderr, "XKB extension missing; keyboard input will not work\n");
	} else {
		xkb_base_event = ext->first_event;

		/* Initialize. */
		xcb_xkb_use_extension_reply_t *const reply = xcb_xkb_use_extension_reply(conn,
				xcb_xkb_use_extension_unchecked(conn, XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION),
				NULL);
		if (!reply->supported) {
			fprintf(stderr, "Requested XKB version %d.%d not supported by server, got %d.%d; keyboard input may not work\n",
					XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION,
					reply->serverMajor, reply->serverMinor);
		}

		free(reply);

		xkb_context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
	}

	/* https://www.x.org/releases/X11R7.7/doc/fixesproto/fixesproto.txt */
	ext = xcb_get_extension_data(conn, &xcb_xfixes_id);
	if (!ext->present) {
		fprintf(stderr, "XFixes extension missing; pointer barricading will not work\n");
	} else {
		XCB_GET_REPLY(reply, xcb_xfixes_query_version,
				XCB_XFIXES_MAJOR_VERSION, XCB_XFIXES_MINOR_VERSION);
		if (!reply || !(XCB_XFIXES_MAJOR_VERSION == reply->major_version &&
		                XCB_XFIXES_MINOR_VERSION == reply->minor_version))
		{
			fprintf(stderr, "Requested XFixes version %"PRIu32".%"PRIu32" not supported by server, got %"PRIu32".%"PRIu32"; pointer barricading may not work\n",
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

static bool
load_resource(char **const out, char const *const format, ...)
{
	if (!xrm)
		return false;

	va_list ap;
	char name[128];

	va_start(ap, format);
	memcpy(name, "heawm.", 6);
	vsnprintf(name + 6, sizeof(name) - 6, format, ap);
	va_end(ap);

	return 0 == xcb_xrm_resource_get_string(xrm, name, NULL, out);
}

static void
xrm_update(void)
{
	char *value;

	/*MAN(RESOURCES)
	 * .SS "Label Resources"
	 */

	/*MAN(RESOURCES)
	 * .IP \(bu
	 * .B heawm.label.fontFamily
	 */
	if (load_resource(&value, "label.fontFamily")) {
		free(label_font);
		label_font = value;
	}

	/*MAN(RESOURCES)
	 * .IP \(bu
	 * .B heawm.label.fontSize
	 */
	if (load_resource(&value, "label.fontSize")) {
		label_font_size = strtol(value, NULL, 10);
		free(value);
	}

	/*MAN(RESOURCES)
	 * .IP \(bu
	 * .B heawm.label.strokeWidth
	 */
	if (load_resource(&value, "label.strokeWidth")) {
		label_stroke = strtol(value, NULL, 10);
		free(value);
	}
}

static void
xrm_open(void)
{
	if (xrm)
		xcb_xrm_database_free(xrm);
	if (!(xrm = xcb_xrm_database_from_default(conn)))
		fprintf(stderr, "Could not load X resource manager\n");

	xrm_update();
}

static void
body_setup_windows(Body *const body)
{
	xcb_screen_t const *const screen = body->screen;
	XCB_GET_REPLY(reply, xcb_query_tree, screen->root);
	if (!reply)
		return;

	xcb_window_t const *const children = xcb_query_tree_children(reply);
	int const num_children = xcb_query_tree_children_length(reply);

	xcb_get_window_attributes_cookie_t *const cookies =
		check_alloc(malloc(num_children * sizeof *cookies));

	for (int i = 0; i < num_children; ++i)
		cookies[i] = xcb_get_window_attributes_unchecked(conn, children[i]);

	for (int i = 0; i < num_children; ++i) {
		xcb_get_window_attributes_reply_t *const reply =
			xcb_get_window_attributes_reply(conn, cookies[i], NULL);
		if (!reply)
			continue;

		if (/* Should we manage it? */
		    !reply->override_redirect &&
		    /* Is mapped? */
		    XCB_MAP_STATE_VIEWABLE == reply->map_state)
			box_window(screen->root, children[i]);

		free(reply);
	}

	free(cookies);
	free(reply);
}

static void
body_setup_cursor(Body *const body)
{
	xcb_cursor_context_t *ctx;
	xcb_screen_t *const screen = body->screen;

	if (xcb_cursor_context_new(conn, screen, &ctx) < 0)
		return;

	body->move_cursor = xcb_cursor_load_cursor(ctx, "move");

	xcb_cursor_t const cursor = xcb_cursor_load_cursor(ctx, "default");
	XDO(xcb_change_window_attributes, conn, screen->root,
			XCB_CW_CURSOR, &cursor);
	xcb_free_cursor(conn, cursor);

	/* Note: It does not free created cursors. */
	xcb_cursor_context_free(ctx);
}

static void
body_setup_net_name(Body const *const body, char const *const name)
{
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->net_window, ATOM(_NET_WM_NAME),
			ATOM(UTF8_STRING), 8,
			strlen(name), name);
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->net_window, ATOM(_NET_SUPPORTING_WM_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &body->net_window);
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->screen->root, ATOM(_NET_SUPPORTING_WM_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &body->net_window);

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->net_window, ATOM(_NET_WM_PID),
			XCB_ATOM_CARDINAL, 32,
			sizeof(uint32_t), (uint32_t const[]){ getpid() });
}

static void
body_setup_net_supported(Body *const body)
{
#define xmacro(name) +1
	static uint32_t const NUM_NET_ATOMS = NET_ATOMS;
#undef xmacro

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			body->screen->root, ATOM(_NET_SUPPORTED),
			XCB_ATOM_ATOM, 32,
			NUM_NET_ATOMS, atoms);
}

static void
body_setup_net(Body *const body)
{
	body_setup_net_name(body, WM_NAME);
	body_setup_net_supported(body);
	body_clear_net_client_list(body);
}

static void
body_update_heads(Body *const body);

static void
body_setup_heads(Body *const body)
{
	XDO(xcb_randr_select_input, conn, body->screen->root,
			XCB_RANDR_NOTIFY_MASK_SCREEN_CHANGE);

	body_update_heads(body);
}

static void
body_setup_hands(Body *const body);

static void
body_create_layer(Body *const body, xcb_window_t *const layer)
{
	*layer = xcb_generate_id(conn);
	XDO(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			*layer,
			body->screen->root,
			-1, -1, 1, 1, /* Rect. */
			0, /* Border. */
			XCB_WINDOW_CLASS_INPUT_ONLY,
			XCB_COPY_FROM_PARENT,
			XCB_CW_OVERRIDE_REDIRECT,
			(uint32_t const[]){
				true
			});
}

static void
body_detect_compositor(Body *const body)
{
	if (!(body->composited = !!XCHECK(xcb_composite_redirect_subwindows, conn,
			body->screen->root, XCB_COMPOSITE_REDIRECT_MANUAL)))
		XDO(xcb_composite_unredirect_subwindows, conn,
			body->screen->root, XCB_COMPOSITE_REDIRECT_MANUAL);
}

static void
body_setup(Body *const body)
{
	int error;

	if ((error =
		XCHECK(xcb_change_window_attributes, conn, body->screen->root,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					CLIENT_WINDOW_EVENT_MASK |
					XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
				})))
	{
		fprintf(stderr, "Unable to manage windows on screen %d: %s\n",
				body->screen_index,
				XCB_ACCESS == error
					? "Window manager is already running"
					: "Unknown error");
		/* It is a fatal error because we have input devices that we do
		 * not know if that WM handles or not that will surely cause
		 * conflict. */
		/* Also: It can crash X. */
		exit(EXIT_FAILURE);
	}

	body_create_layer(body, &body->float_layer);
	body_create_layer(body, &body->label_layer);
	body_setup_cursor(body);
	body_setup_heads(body);
	body_setup_hands(body);
	body_setup_net(body);
	body_setup_windows(body);
	body_detect_compositor(body);
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
		sprintf(p + host_size, ":%d.%d", display, body->screen_index);

		setenv("DISPLAY", host, true);
	}
	free(host);
}

static int
body_head_cmp(void const *const p, void const *const q)
{
	Box const *const x = *(Box **)p;
	Box const *const y = *(Box **)q;

	if (x->body != y->body)
		return (int)y->body - (int)x->body;

	/* x is less then y if x is placed in the top-left quadrant relative to
	 * y. Note that order is reversed since we walk heads backwards. */
	return -(x->rect.x <= y->rect.x && x->rect.y <= y->rect.y ? -1 : 1);
}

static char *
head_get_name_reply(xcb_get_atom_name_reply_t *name_reply)
{
	if (!name_reply)
		return NULL;

	int const name_size = xcb_get_atom_name_name_length(name_reply);
	char *name = malloc(sizeof MONITOR_CLASS + name_size + 1 /* NUL */);
	if (name) {
		memcpy(name, MONITOR_CLASS, sizeof MONITOR_CLASS);
		memcpy(name + sizeof MONITOR_CLASS, xcb_get_atom_name_name(name_reply), name_size);
		name[sizeof MONITOR_CLASS + name_size] = '\0';
	}

	free(name_reply);

	return name;
}

static void
body_update_heads(Body *const body)
{
	/* Mark heads of this body as unneeded. */
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		head->flagged = 0;
		if ((body - bodies) == head->body)
			head->urect.width = 0;
	}

	/* Force RROutputChange under Xephyr -resizeable, otherwise monitor
	 * dimensions are not updated. */
	XCB_GET_REPLY(resources, xcb_randr_get_screen_resources, body->screen->root);
	free(resources);

	XCB_GET_REPLY(monitors, xcb_randr_get_monitors, body->screen->root, true);
	if (!monitors) {
		fprintf(stderr, "Failed to query RandR monitors\n");

	screen_as_monitor:;
		Box *const head = box_new();
		head->body = body - bodies;
		head->class = NULL;
		head->num_visible = 1;
		head->flagged = 1; /* Primary. */
		box_set_placeholder_name(head);
		box_reparent(root, 0, head);

		head->urect.width = body->screen->width_in_millimeters;
		head->urect.height = body->screen->height_in_millimeters;
		head->floating = 1;
		box_set_size(head, body->screen->width_in_pixels, body->screen->height_in_pixels);
		box_set_position(head, 0, 0);
	} else {
		int const num_monitors = xcb_randr_get_monitors_monitors_length(monitors);
		if (!num_monitors && !root->num_children)
			goto screen_as_monitor;

		xcb_get_atom_name_cookie_t *const cookies =
			malloc(num_monitors * sizeof *cookies);
		if (!cookies)
			goto done;

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

			char *name = head_get_name_reply(xcb_get_atom_name_reply(conn, cookies[iter.rem - 1], NULL));

			Box *head = body_find_head_by_name(body, name);
			if (!head) {
				head = box_new();
				head->body = body - bodies;
				head->class = name;
				head->num_visible = 1;
				head->floating = 1;
				name = NULL;

				/* Do not name it yet. */
				box_set_placeholder_name(head);
				box_reparent(root, monitor->primary ? 0 : root->num_children, head);
			} else {
				/* Allow renaming empty heads. */
				if (!head->num_children)
					box_set_placeholder_name(head);
			}

			/* FIXME: Maybe swap values if rotated. */
			head->urect.width = monitor->width_in_millimeters;
			head->urect.height = monitor->height_in_millimeters;
			box_set_size(head, monitor->width, monitor->height);
			box_set_position(head, monitor->x, monitor->y);

			/* Primary monitor is named before others so to always get
			 * the lowest available name. */
			head->flagged = !!monitor->primary;

			free(name);
		}

		free(cookies);
	}
done:
	free(monitors);

	qsort(root->children, root->num_children, sizeof *root->children, body_head_cmp);

	/* Whether 1-to-1 mapping is possible among gone and empty monitors. */
	uint16_t gone_with_children = 0,
	         existing_without_children = 0;
	/* Otherwise try to find a good parent for gone monitors. */
	Box *parent = NULL;

	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (!head->urect.width)
			gone_with_children += !!head->num_children;

		if (!head->urect.width ||
		    (body - bodies) != head->body)
			continue;

		if (!parent ||
		    head->num_children < parent->num_children ||
		    parent->flagged < head->flagged)
			parent = head;

		existing_without_children += !head->num_children;
	}

	/* And just after this we can start name primary monitors. */
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (head->flagged && box_has_placeholder_name(head)) {
			box_clear_name(head);
			box_name(head);
		}
	}

	/* And then name remaining boxes. (Mostly means new monitors.) */
	for (uint16_t i = 0; i < root->num_children; ++i) {
		Box *const head = root->children[i];
		if (box_has_placeholder_name(head)) {
			box_clear_name(head);
			box_name(head);
		}
	}

	/* Reparent contents of gone monitors into some visible space. Note that
	 * we must walk backwards since unused heads get cleaned up. */
	for (uint16_t i = root->num_children; 0 < i;) {
		Box *const head = root->children[--i];
		if (!head->urect.width && parent) {
			free(head->class), head->class = NULL;

			if (0 < gone_with_children &&
			    gone_with_children == existing_without_children)
			{
				/* Find first kept head. */
				for (uint16_t j = 0;; ++j) {
					Box *const into_head = root->children[j];
					if (into_head->urect.width &&
					    (body - bodies) == into_head->body &&
					    !into_head->num_children)
					{
						box_swap(into_head, head);
						box_delete(into_head);
						break;
					}
				}
			} else {
				head->urect.height = 0;
				box_reparent(parent, 0, head);
				parent = head->parent;
				box_vacuum(head);
			}
		} else if (!head->urect.width)
			/* A little bit hacky since old value gone. But it
			 * can occur only when there are no connected
			 * monitors so user will perceive nothing from it
			 * anyway. */
			head->urect.width = 1;
	}

	/*MAN(HOOKS)
	 * .TP
	 * .B displaychange
	 * Run whenever display configuration changes, e.g. monitor
	 * connected/disconnected, resolution changed. See xrandr(1).
	 */
	BODY_SPAWN(body, get_script_path("displaychange"));
}

static void
body_setup_hands(Body *const body)
{
	XDO(xcb_input_xi_select_events, conn, body->screen->root, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
					XCB_INPUT_XI_EVENT_MASK_BARRIER_HIT |
					XCB_INPUT_XI_EVENT_MASK_HIERARCHY |
					XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
					XCB_INPUT_XI_EVENT_MASK_ENTER));
}

static void
setup_display(void)
{
	static char const DEFAULT_DISPLAY[] = ":0";

	int preferred_screen;
	conn = check_alloc(xcb_connect(NULL, &preferred_screen));

	for (int error; (error = xcb_connection_has_error(conn));) {
		char const *const display = getenv("DISPLAY");

		if (display) {
			fprintf(stderr, "Could not open display %s: %s\n",
					display, xcb_connection_strerror(error));
		} else {
			fprintf(stderr, "DISPLAY is not set, default to %s\n",
					DEFAULT_DISPLAY);

			if (!setenv("DISPLAY", DEFAULT_DISPLAY, false))
				continue;
		}

		exit(EXIT_FAILURE);
	}

	init_atoms();
	init_extensions();

	root = box_new();

	symbols = xcb_key_symbols_alloc(conn);

	xrm_open();

	/* Prevent windows from changing. */
	XDO(xcb_grab_server, conn);

	xcb_setup_t const *const setup = xcb_get_setup(conn);

	num_bodies = xcb_setup_roots_length(setup);
	bodies = check_alloc(calloc(num_bodies, sizeof *bodies));

	int i = 0;
	for (xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);
	     0 < iter.rem;
	     ++i, xcb_screen_next(&iter))
	{
		xcb_screen_t *const screen = iter.data;
		Body *const body = &bodies[preferred_screen == i ? 0 : i + 1];

		body->screen_index = i;
		body->screen = screen;
		body->visual_type = lookup_visual_type(screen);

		body_setup(body);
	}

	XDO(xcb_ungrab_server, conn);

	hands_update();
}

static void
handle_error(xcb_generic_error_t const *const event)
{
	print_error(event, "(event loop)");
}

static void
box_reparent_children(Box *new_parent, uint16_t pos, Box const *const parent)
{
	assert(box_is_container(new_parent));
	assert(box_is_container(parent));

	uint16_t i = parent->num_children;
	if (!i)
		return;

	Box *const last_child = parent->children[i - 1];

	do {
		Box *const child = 1 < i ? parent->children[0] : last_child;
		if (!child->user_concealed)
			child->concealed = false;

		/* Can occur if parent == new_parent. */
		if (new_parent->num_children < pos)
			pos = new_parent->num_children;
		box_reparent_checked(new_parent, pos++, child);
		new_parent = child->parent;

		/* Last children will be vacuumed upwards automatically. */
	} while (0 < --i);
}

/**
 * Create a new box that contains all related boxes at the place of |box|.
 */
static bool
box_group(Box const *const box)
{
	assert(!box_is_container(box));

	uint16_t num_items = 0;
	/* pos is surely initialized since we iterate over box->parent */
	uint16_t pos = pos;
	bool ignore_leader = false;

retry:
	for (uint16_t i = 0; i < box->parent->num_children; ++i) {
		Box *const child = box->parent->children[i];
		num_items += (child->flagged =
			!child->user_concealed &&
			(!ignore_leader && box->leader
				? box->leader == child->leader
				: box->class && child->class &&
				  !strcmp(box_get_class_instance(box), box_get_class_instance(child))));

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
		box_set_placeholder_name(container);
		box_reparent(box->parent, pos, container);
	}

	Box *const parent = box->parent;

	parent->conceal_seq = ++root->conceal_seq;

	for (uint16_t i = 0; i < parent->num_children;) {
		Box *const child = parent->children[i];

		if (!child->flagged) {
			++i;
			continue;
		}

		Box *const container = parent->children[pos];
		box_reparent(container, container->num_children, child);
		if (i < pos)
			--pos;

		child->concealed = true;
	}

	{
		Box *const container = parent->children[pos];
		box_clear_name(container);
		box_name(container);
	}

	return true;
}

static void
handle_property_notify(xcb_property_notify_event_t const *const event)
{
	box_update_property((BoxOrWindow){ .window = event->window }, event->atom, (xcb_get_property_cookie_t){ 0 }, true);
}

static void
hand_grab_pointer(Hand const *const hand)
{
	for_each_body {
		xcb_window_t const root_window = body->screen->root;

		XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(root_window,
				hand->master_pointer,
				XCB_INPUT_GRAB_TYPE_BUTTON, XCB_GRAB_ANY,
				{ XCB_INPUT_MODIFIER_MASK_ANY });

		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(root_window,
				body->move_cursor,
				hand->master_pointer,
				XCB_INPUT_GRAB_TYPE_BUTTON, XCB_GRAB_ANY,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				XCB_INPUT_GRAB_OWNER_NO_OWNER,
				XCB_INPUT_XI_EVENT_MASK_BARRIER_HIT |
				XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS |
				XCB_INPUT_XI_EVENT_MASK_BUTTON_RELEASE |
				XCB_INPUT_XI_EVENT_MASK_MOTION,
				{
					EFFECTIVE_MASK(XCB_MOD_MASK_4),
					EFFECTIVE_MASK(XCB_MOD_MASK_4 | XCB_MOD_MASK_SHIFT),
				});
	}
}

static Hand *
get_hand_by_master_keyboard(xcb_input_device_id_t const master_keyboard)
{
	for_each_hand
		if (master_keyboard == hand->master_keyboard)
			return hand;

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
	if (event->event != hand_get_wanted_focus(hand) ||
	    /* Focus in event generated for root (and no focus out) but we have
	     * to forcefully focus it again to make it really focused... */
	    !hand->input_focus)
		hand_refocus(hand);
}

static void
handle_unmap_notify(xcb_unmap_notify_event_t const *const event)
{
	if (5 <= HEAWM_VERBOSE)
		printf("unmap notify %x event=%x, from_configure=%d\n", event->window, event->event, event->from_configure);

	/* Ignore reparenting related events. */
	if (event->event != event->window)
		return;

	Box *const box = box_find_by_window(root, offsetof(Box, window), event->window);
	if (box)
		box_delete(box);
}

static void
handle_map_request(xcb_map_request_event_t const *const event)
{
	if (5 <= HEAWM_VERBOSE)
		printf("map request %x\n", event->window);

	Box *const box = box_find_by_window(root, offsetof(Box, window), event->window);
	if (!box)
		box_window(event->parent, event->window);
}

static void
handle_configure_notify(xcb_configure_notify_event_t const *const event)
{
	/* We need this only when XRandR is not available. */
	if (randr_base_event)
		return;

	for_each_body {
		if (event->window != body->screen->root)
			continue;

		body_update_heads(body);
		break;
	}
}

static void
box_send_configure_notify(Box const *const box)
{
	XDO(xcb_send_event, conn, false, box->window,
			XCB_EVENT_MASK_STRUCTURE_NOTIFY,
			XCB_SEND_EVENT_EVENT(xcb_configure_notify_event_t,
				.response_type = XCB_CONFIGURE_NOTIFY,
				.event = box->window,
				.window = box->window,
				.above_sibling = XCB_WINDOW_NONE,

				.x = box->rect.x,
				.y = box->rect.y,
				.width = box->rect.width,
				.height = box->rect.height,

				.border_width = 0,
				/* Surely not if request reached us. */
				.override_redirect = false,
			));
}

/**
 * After changing dimensions of box, reposition it so:
 * - Its center stays still.
 * - If box was previously inside parent or snapped to its bounds make it so
 *   afterwards.
 */
static void
box_set_usize_with_ureposition(Box *const box, uint16_t const width, uint16_t const height)
{
	xcb_rectangle_t rect = (xcb_rectangle_t){
		.x = box->urect.x + (box->urect.width - width) / 2,
		.y = box->urect.y + (box->urect.height - height) / 2,
		.width = width,
		.height = height,
	};

	xcb_rectangle_t const *bounds = box_get_parent_bounds(box);
#define CHECK(x, width) \
	if (box->urect.x == bounds->x || \
	    (bounds->x <= box->urect.x && \
	     rect.x < bounds->x)) \
		rect.x = bounds->x; \
	else if (box->urect.x + box->urect.width == bounds->x + bounds->width || \
	         (box->urect.x + box->urect.width <= bounds->x + bounds->width && \
	          bounds->x + bounds->width < rect.x + rect.width)) \
		rect.x = box->urect.x + box->urect.width - rect.width;
		REPEAT_XY(CHECK)
#undef CHECK

	box_set_urect(box, rect);
}

static void
handle_configure_request(xcb_configure_request_event_t const *const event)
{
	Box *box;
	if (!(box = box_find_by_window(root, offsetof(Box, window), event->window)))
		return;

	/* Save boundaries but only care about them when floating. */
	if ((XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT) ==
	    ((XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT) & event->value_mask))
		box_set_usize_with_ureposition(box, event->width, event->height);

	if ((XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y) ==
	    ((XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y) & event->value_mask))
		box_set_uposition(box, event->x, event->y);

	if (box->floating)
		box_propagate_change(box);

	box_send_configure_notify(box);
}

static void
handle_expose(xcb_expose_event_t const *const event)
{
	/* Act only on the last expose event in the row. */
	if (0 < event->count)
		return;

	for_each_body
		for (uint32_t j = 0; j < body->num_labels_used; ++j) {
			Label const *const label = &body->labels[j];
			if (label->window == event->window) {
				label_repaint(label, false);
				return;
			}
		}
}

static void
wm_close_window(xcb_window_t const window)
{
	XDO(xcb_send_event, conn, false, window,
			XCB_EVENT_MASK_NO_EVENT/* Client messages cannot be masked. */,
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
	for_each_box(box, root) {
		if (box_is_container(box))
			continue;

		if (!box->close_by_force) {
			box->close_by_force = true;
			wm_close_window(box->window);
		} else {
			XDO(xcb_kill_client, conn, box->window);
		}

	}
}

static void
handle_client_message(xcb_client_message_event_t const *const event)
{
	Box *const box = box_find_by_window(root, offsetof(Box, window), event->window);
	debug_print_atom_name("client message", event->type);
	if (!box)
		return;

	if (ATOM(_NET_CLOSE_WINDOW) == event->type) {
		box_close(box);
	} else if (ATOM(_NET_ACTIVE_WINDOW) == event->type) {
		if (XCB_EWMH_CLIENT_SOURCE_TYPE_NORMAL == event->data.data32[0])
			return;

		Hand *hand = &hands[NULL_HAND == box->focus_hand ? 0 : box->focus_hand];
		hand_focus_box(hand, box);
	} else if (ATOM(_NET_WM_STATE) == event->type) {
		if (32 == event->format &&
		    (ATOM(_NET_WM_STATE_FULLSCREEN) == event->data.data32[1] ||
		     ATOM(_NET_WM_STATE_FULLSCREEN) == event->data.data32[2]))
		{
			switch (event->data.data32[0]) {
			case XCB_EWMH_WM_STATE_REMOVE:
				box->net_fullscreen = false;
				break;

			case XCB_EWMH_WM_STATE_ADD:
				box->net_fullscreen = true;
				break;

			case XCB_EWMH_WM_STATE_TOGGLE:
				box->net_fullscreen ^= true;
				break;
			}

			box_update_net(box);

			/* Same brainfucked programs like Chromium thinks it stands above
			 * window manager so without an sane reasons it internally
			 * resizes itself on entering fullscreen. To teach respect, give
			 * a slap for such programs (solely for educational purposes). */
			box_send_configure_notify(box);
		}
	}
}

static Hand *
hand_find_by_master_pointer(xcb_input_device_id_t const pointer);

static void
hand_update(Hand *const hand, uint8_t index, char const *const name, int const name_size)
{
	/*MAN(RESOURCES)
	 * .SS "Hand Resources"
	 * The following resources can be used to customize
	 * hands. They are specified by the following patterns
	 * and checked in order:
	 * .
	 * .IP \(bu
	 * \fBheawm.hand.\fINAME\fB.*\fR: Match hand by
	 * its \fINAME\fR, e.g. \(lqVirtual core\(rq. \fINAME\fR
	 * is coming from
	 * .B XInput
	 * \(lq\fINAME\fR pointer\(rq and
	 * \(lq\fINAME\fR keyboard\(rq master device pair.
	 * .
	 * .IP \(bu
	 * \fBheawm.hand.\fIINDEX\fB.*\fR: Match hand by its 1-based \fIINDEX\fR.
	 * .
	 * .IP \(bu
	 * \fBheawm.hand.*\fR: Match all hands. Can be useful for specifying default settings.
	 * .
	 * .TP
	 * .B color
	 * Specifies focus color in
	 * .B 0xRRGGBB
	 * or
	 * .B #RRGGBB
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
	hand->color = 0xfe0202 /*, 0xffaf5f*/;
	for (char *value;
	     load_resource(&value, "hand.%.*s.color", name_size, name) ||
	     load_resource(&value, "hand.%u.color", index) ||
	     load_resource(&value, "hand.color");)
	{
		if (1 != sscanf(value, "0x%6x", &hand->color) &&
		    1 != sscanf(value, "#%6x", &hand->color))
			fprintf(stderr, "Invalid color resource value: %s\n",
					value);
		free(value);
		break;
	}

	/* It is safe to call because we know that hand currently
	 * has no focus. */
	hand_refocus(hand);
}

static void
hand_setup(Hand *const hand)
{
	hand->want_focus = true;

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

	XDO(xcb_xkb_select_events, conn, hand->master_keyboard,
			REQUIRED_EVENTS,
			0,
			REQUIRED_EVENTS,
			REQUIRED_MAP_PARTS,
			REQUIRED_MAP_PARTS,
			NULL);

	hand->barrier = generate_barrier_ids();
}

static void
hand_destroy(Hand *const hand)
{
	hand_unbarricade(hand);
}

static void
devices_destroy(void)
{
	for (uint8_t i = 0; i < num_devices; ++i) {
		Device *const device = &devices[i];
		xkb_keymap_unref(device->keymap);
	}
}

static void
hands_update(void)
{
	XCB_GET_REPLY(reply, xcb_input_xi_query_device, XCB_INPUT_DEVICE_ALL);
	if (!reply)
		return;

	/* Map old hand indexes to new hand indexes for correct stack history. */
	/* According to specification devices above 127 are invisible to clients. */
	uint8_t hand_map[NULL_HAND + 1];

	/* Reassign boxes of deattached hands to an invalid hand. */
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

	Hand *const new_hands = check_alloc(calloc(new_num_hands, sizeof *new_hands));
	Hand *hand = new_hands;

	devices_destroy();

	num_devices = xcb_input_xi_query_device_infos_length(reply);
	devices = check_alloc(realloc(devices, num_devices * sizeof(*devices)));
	Device *device = devices;

	for (xcb_input_xi_device_info_iterator_t iter = xcb_input_xi_query_device_infos_iterator(reply);
	     0 < iter.rem;
	     xcb_input_xi_device_info_next(&iter))
	{
		xcb_input_xi_device_info_t const *const input_device = iter.data;

		/* Create hand for every master device pair. It is enough to look
		 * for pointer master devices because they always come in pair. */
		if (XCB_INPUT_DEVICE_TYPE_MASTER_POINTER == input_device->type) {
			Hand *const old_hand = hand_find_by_master_pointer(input_device->deviceid);
			if (old_hand) {
				*hand = *old_hand;
				hand_map[old_hand - hands] = hand - new_hands;
			} else {
				hand->master_pointer = input_device->deviceid;
				hand->master_keyboard = input_device->attachment;

				hand_setup(hand);
			}

			int const name_size = xcb_input_xi_device_info_name_length(input_device) - strlen(" pointer");
			char const *const name = xcb_input_xi_device_info_name(input_device);

			hand_update(hand, hand - new_hands, name, name_size);
			++hand;
		}

		/* Also record every slave and master device and attach it to head (master device). */
		device->id = input_device->deviceid;

		device->hand = new_hands;
		while (device->hand->master_pointer != input_device->attachment &&
		       device->hand->master_keyboard != input_device->attachment)
			++device->hand;

		device->keymap = NULL;
		++device;
	}
	assert(device - devices == num_devices);

	free(reply);

	/* Update focus history. */
	Box *box;
	for_each_box(box, root) {
		box->focus_hand = hand_map[box->focus_hand];
		if (box_is_container(box))
			continue;

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

	for (uint8_t i = 0; i < num_hands; ++i)
		if (NULL_HAND == hand_map[i])
			hand_destroy(&hands[i]);

	/* Forget deattached hands' focus by incrementing focus number of
	 * hands alive. */
	hands_update_focus();

	free(hands);
	hands = new_hands;
	num_hands = new_num_hands;

	hands_try_focus_all(root->focus_seq);

	/*MAN(HOOKS)
	 * .TP
	 * .B inputchange
	 * Run whenever input devices change, e.g. keyboard plugged/unplugged,
	 * master device added. See xinput(1).
	 */
	SPAWN(get_script_path("inputchange"));
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
		hands_update();
}

static Hand *
hand_find_by_master_pointer(xcb_input_device_id_t const master_pointer)
{
	for_each_hand
		if (master_pointer == hand->master_pointer)
			return hand;

	return NULL;
}

/** Modifiers we are interested in. */
#define KEY_MOD_MASK ( \
(	XCB_MOD_MASK_CONTROL \
|	XCB_MOD_MASK_1 \
|	XCB_MOD_MASK_2 \
|	XCB_MOD_MASK_3 \
|	XCB_MOD_MASK_4 \
|	XCB_MOD_MASK_5 \
) & ~XCB_MOD_MASK_NUM_LOCK)

static void
hand_handle_input_key_normal(xcb_input_key_press_event_t const *const event, Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
	(void)event;

	if (repeating)
		return;

	hand->want_focus |= XKB_KEY_Return == sym;
	hand->check_input = false;
	hand_assign_latest_input(hand);
}

static Box *
hand_get_latest_input(Hand const *const hand)
{
	Box *ret = hand->latest_input[hand->focus == hand->latest_input[0]];
	if (!ret) {
		Box *recents[2];
		hand_find_recents(hand, root, root->focus_seq, recents, 2);
		ret = recents[hand->focus == recents[0]];
	}
	return ret;
}

static bool
hand_input_try_jump(Hand *const hand)
{
	Box *box = hand->focus
		? (box_is_container(hand->focus) ? hand->focus : hand->focus->parent)
		: NULL;
	bool const complete = box_find_by_name(&box, hand->user_input);
	bool ret = false;

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
		if (SPAWN(get_script_path("autostart"), name))
			hand->want_focus = true;
		goto reset_input;
	}

	/* If focus would not change, try focus an interesting window. */
	if (box == hand->focus)
		box = hand_get_latest_input(hand);

	if (!box)
		goto reset_input;

	hand_focus_box(hand, box);
	ret = true;

reset_input:
	if (complete)
		hand_input_reset(hand);

	return ret;
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

static void
hand_update_mode(Hand *const hand)
{
	if (HAND_MODE_NULL != hand->mode) {
		hand_input_reset(hand);
		hand_do_mode_changes(hand);
	}
}

static void
hand_start_name(Hand *const hand)
{
	if (!hand->focus)
		return;

	hand->mode = HAND_MODE_NAME;
	hand->mode_box = hand->focus;
}

static void
hand_start_move(Hand *const hand)
{
	if (!hand->focus)
		return;

	hand->mode = HAND_MODE_MOVE;
	hand->mode_box = hand->focus;

	if (box_get_foot(hand->mode_box))
		return;

	Box *const box = hand_get_latest_input(hand);
	if (box && !box_is_descendant(hand->mode_box, box))
		hand_focus_box(hand, box);
}

static Box *
box_get_parent_checked(Box const *const box)
{
	if (box->parent &&
	    /* Is addressable by user. */
	    *box->parent->name)
		return box->parent;

	return NULL;
}

static void
hand_focus_parent(Hand *const hand)
{
	if (hand->focus && box_get_parent_checked(hand->focus))
		hand_focus_box(hand, hand->focus->parent);
}

static void
box_maximize(Box *box, bool const recursive)
{
	box_set_floating(box, false);
	box = box->parent;

	assert(box_is_container(box));

	if (box == root)
		return;

	box_propagate_change(box);

	++root->conceal_seq;

	uint16_t conceal_seq = box->conceal_seq;
	int zoom = 1 != box->num_visible;
	do {
		if (!zoom) {
			if (box->conceal_seq != conceal_seq)
				break;
			box->num_visible = 0;
		} else if (1 != box->num_visible) {
			box->conceal_seq = root->conceal_seq;
			box->num_visible = 1;
		}
		box->layout_changed = true;
	} while ((recursive && !box->floating) && (box = box->parent, true));
}

static void
hand_focus_child(Hand *const hand)
{
	if (hand->focus && hand->input_focus && hand->focus != hand->input_focus) {
		Box *box = hand->input_focus;
		while (box->parent != hand->focus)
			box = box->parent;
		hand_focus_box(hand, box);
	}
}

static void
box_step_num_visible(Box *box, int const dir)
{
	box = box_is_container(box) ? box : box->parent;

	if (0 < dir)
		++box->num_visible;

	if (box->num_children < box->num_visible)
		box->num_visible = box->num_children;

	if (dir < 0 && 0 < box->num_visible)
		--box->num_visible;

	box->label_changed = true;
	box_propagate_change(box)->layout_changed = true;
}

static void
hand_focus_parent_float(Hand *const hand)
{
	for (Box *box = hand->focus; (box = box->parent);)
		if (box->floating) {
			hand_focus_box(hand, box);
			return;
		}
}

static bool
hand_handle_input_key_super(Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod- { A-Za-z }...
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

		hand->want_popup = true;
		SPAWN(config.terminal, "-e", get_script_path("quickstart"));
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

		hand->want_popup = true;
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

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Esc
	 * Escape from wherever you are.
	 */
	case XKB_KEY_Escape:
		if (!hand->input_focus)
			break;

		hand_focus_box(hand, hand->input_focus);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-!
	 * .RB "Refer to " Mod-Ctrl-t
	 */
	case XKB_KEY_exclam:
		hand_start_move(hand);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-.
	 * .RB "Refer to " Mod-Ctrl-f
	 */
	case XKB_KEY_period:
		if (!repeating &&
		    hand->focus)
			box_maximize(hand->focus, true);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-$
	 * .RB "Refer to " Mod-Ctrl-n
	 */
	case XKB_KEY_dollar:
		hand_start_name(hand);
		break;


	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-;
	 * .RB "Refer to " Mod-Ctrl-k
	 */
	case XKB_KEY_semicolon:
		hand_focus_parent(hand);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-,
	 * .RB "Refer to " Mod-Ctrl-j
	 */
	case XKB_KEY_comma:
		hand_focus_child(hand);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-{
	 * .RB "Refer to " Mod-Ctrl-x
	 */
	case XKB_KEY_braceleft:
	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-}
	 * .RB "Refer to " Mod-Ctrl-a
	 */
	case XKB_KEY_braceright:
		if (repeating)
			break;

		if (!hand->focus)
			break;

		box_step_num_visible(hand->focus, sym == XKB_KEY_braceright ? 1 : -1);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-#
	 * Focus floating parent.
	 */
	case XKB_KEY_numbersign:
		if (repeating)
			break;

		if (!hand->focus)
			break;

		hand_focus_parent_float(hand);
		break;

	default:
		return true;
	}

	hand_update_mode(hand);

	return false;
}

static void
box_explode(Box *const box, bool const vertical, bool const inner)
{
	if (box_is_monitor(box))
		/* TODO: Possibly we should do something. */
		return;

	if (box_is_monitor(box->parent) || inner) {
		Box *const container = box_new();
		memcpy(container->name, box->name, sizeof container->name);
		container->hide_label = box->hide_label;
		box_reparent(box->parent, box_get_pos(box), container);

		box_clear_name(box);
		box->hide_label = false;
		box->label_changed = true;
		box->concealed = false;
		box->user_concealed = false;
		box_reparent(container, 0, box);
		return;
	}

	for (uint8_t i = 0; i < 3; ++i) {
		Box *const split = box_new();
		box_set_placeholder_name(split);
		box_reparent(box->parent, i, split);
	}

	Box *const parent = box->parent;

	size_t const pos_offset = vertical ? offsetof(Box, rect.x)     : offsetof(Box, rect.y);
	size_t const dim_offset = vertical ? offsetof(Box, rect.width) : offsetof(Box, rect.height);
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

	/* Vacuum may reparent splits. */
	Box *const splits[3] = {
		parent->children[0],
		parent->children[1],
		parent->children[2],
	};

	for (uint8_t i = 0; i < 3; ++i) {
		Box *const split = splits[i];
		box_clear_name(split);
		box_name(split);
		/* box->parent will surely receive a child */
		if (split != box->parent)
			box_vacuum(split);
	}
}

static void
hand_handle_input_key_mode(xcb_input_key_press_event_t const *const event, Hand *const hand, xcb_keysym_t const sym)
{
	switch (hand->mode) {
	case HAND_MODE_MOVE:
		switch (KEY_MOD_MASK & event->mods.base) {
		case XCB_MOD_MASK_4:
			if (hand_handle_input(hand, sym)) {
				hand_input_try_jump(hand);
				return;
			}
			/* FALLTHROUGH */
		case XCB_MOD_MASK_4 | XCB_MOD_MASK_CONTROL:
		case XCB_MOD_MASK_4 | XCB_MOD_MASK_1:
			switch (sym) {
			case XKB_KEY_k:
				hand_focus_parent(hand);
				break;

			case XKB_KEY_j:
				hand_focus_child(hand);
				break;
			}
			/* FALLTHROUGH */
		case 0:
			switch (sym) {
			case XKB_KEY_minus:
				hand_focus_box(hand, box_get_head(hand->focus));
				goto append;

			case XKB_KEY_numbersign:
				hand_focus_parent_float(hand);
				return;

			case XKB_KEY_semicolon:
				hand_focus_parent(hand);
				return;

			case XKB_KEY_colon:
				hand_focus_child(hand);
				return;

			case XKB_KEY_period:
				hand_focus_box(hand, hand->mode_box);
				return;

			case XKB_KEY_X:
				if (!box_is_container(hand->mode_box))
					hand->mode_box = hand->mode_box->parent;
				box_reparent_children(hand->focus->parent, box_get_pos(hand->focus), hand->mode_box);
				break;

			case XKB_KEY_x:
				if (!box_is_container(hand->mode_box))
					hand->mode_box = hand->mode_box->parent;
				box_reparent_children(hand->focus->parent, box_get_pos(hand->focus) + 1, hand->mode_box);
				break;

			case XKB_KEY_s:
			case XKB_KEY_S:
				box_swap(hand->focus, hand->mode_box);
				break;

			case XKB_KEY_b:
			case XKB_KEY_P:
			insert:
				if (root == hand->focus->parent ||
				    hand->focus->floating)
					goto into;
				box_reparent_checked(hand->focus->parent, box_get_pos(hand->focus), hand->mode_box);
				goto set_tiled;

			case XKB_KEY_a:
			case XKB_KEY_p:
			append:
				if (root == hand->focus->parent ||
				    hand->focus->floating)
					goto into;
				box_reparent_checked(hand->focus->parent, box_get_pos(hand->focus) + 1, hand->mode_box);
				goto set_tiled;

			case XKB_KEY_I:
				box_reparent_checked(hand->focus->parent, box_get_pos(hand->focus), hand->mode_box);
				break;

			case XKB_KEY_i:
			into:
				box_reparent_into(hand->focus, hand->mode_box);
				break;

			case XKB_KEY_h:
				box_explode(hand->focus, false, false);
				goto insert;

			case XKB_KEY_j:
				box_explode(hand->focus, true, false);
				goto append;

			case XKB_KEY_k:
				box_explode(hand->focus, true, false);
				goto insert;

			case XKB_KEY_l:
				box_explode(hand->focus, false, false);
				goto append;

			case XKB_KEY_H:
				box_explode(hand->focus, false, true);
				goto insert;

			case XKB_KEY_J:
				box_explode(hand->focus, true, true);
				goto append;

			case XKB_KEY_K:
				box_explode(hand->focus, true, true);
				goto insert;

			case XKB_KEY_L:
				box_explode(hand->focus, false, true);
				goto append;

			case XKB_KEY_f:
				box_resize_float(hand->mode_box, FLOAT_UNCHANGED);
				break;

			case XKB_KEY_0...XKB_KEY_9:
				box_resize_float(hand->mode_box, FLOAT_TILE0 + (sym - XKB_KEY_0));
				break;

			case XKB_KEY_KP_0...XKB_KEY_KP_9:
				box_resize_float(hand->mode_box, FLOAT_TILE0 + (sym - XKB_KEY_KP_0));
				break;

			case XKB_KEY_t:
				box_set_urect(hand->mode_box, box_get_chase_rect(hand->mode_box));
				box_propagate_change(hand->mode_box);
				break;

			case XKB_KEY_c:
				box_chase_box(hand->mode_box, 0);
				break;

			default:
				return;

			set_tiled:
				box_set_floating(hand->mode_box, false);
				break;
			}
			break;

		default:
			return;
		}
		break;

	case HAND_MODE_NAME:
		if (!(KEY_MOD_MASK & event->mods.base) &&
		    hand_handle_input(hand, sym))
		{
			box_propagate_change(hand->mode_box)->label_changed = true;
			return;
		}

		if (XKB_KEY_Return == sym ||
		    sizeof hand->user_input == strnlen(hand->user_input, sizeof hand->user_input))
		{
			memcpy(hand->mode_box->name, hand->user_input, sizeof hand->user_input);
			char *const c = &hand->mode_box->name[0];
			*c = box_is_container(hand->mode_box) ? toupper(*c) : tolower(*c);
			box_name(hand->mode_box);
			return;
		} else if (*hand->user_input) {
			return;
		/* Accept some special commands until there is no user input. */
		} else if (XKB_KEY_minus == sym ||
		           XKB_KEY_plus == sym)
		{
			bool const hide_label = XKB_KEY_minus == sym;
			/* Propagate hide_label when only children. */
			Box *b = hand->mode_box;
			do
				b->hide_label = hide_label;
			while ((b = b->parent) && 1 == b->num_children);

			break;
		} else if (XKB_KEY_slash == sym ||
		           XKB_KEY_asterisk == sym)
		{
			bool const hide_label = XKB_KEY_slash == sym;
			if (box_is_container(hand->mode_box)) {
				Box *b;
				for_each_box(b, hand->mode_box)
					b->hide_label = hide_label;
			} else {
				Body const *const body = &bodies[hand->mode_box->body];
				for (uint32_t i = 0; i < body->num_labels_used; ++i) {
					Label const *label = &body->labels[i];
					if (get_rect_overlap(&hand->mode_box->rect, &label->base->rect))
						label->base->hide_label = hide_label;
				}
			}
			break;
		}
		return;

	default:
		return;
	}

	hand_leave_mode(hand);
}

static void
hand_center_pointer(Hand const *const hand, Box const *const box)
{
	XDO(xcb_input_xi_warp_pointer, conn,
			XCB_WINDOW_NONE,
			bodies[box->body].screen->root,
			0, 0, 0, 0,
			/* (x + width / 2) << 16 */
			(box->rect.x << 16) + (box->rect.width  << 15),
			(box->rect.y << 16) + (box->rect.height << 15),
			hand->master_pointer);
}

static bool
hand_handle_input_key_command(Hand *const hand, xcb_keysym_t const sym, bool const repeating)
{
	switch (sym) {
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
	 * .BR Mod-Ctrl-t "ake [ " Mod- { A-Za-z "}... ]... \fIhow\fR"
	 * Take box to somewhere else.
	 * .I how
	 * can be:
	 * .RS
	 * .TP
	 * .BR a fter,\  p aste
	 * Move box after focused one.
	 * .TP
	 * .BR b efore,\  P aste
	 * Move box before focused one.
	 * .TP
	 * .BR i nto
	 * Move box into container. If box is not a container, make a container
	 * out of it.
	 * .TP
	 * .BR h ,\  j ,\  k ,\  l
	 * Place box visually at the given direction.
	 * .TP
	 * .BR H ,\  J ,\  K ,\  L
	 * Make container from box before move.
	 * .TP
	 * .BR s wap
	 * Swap box with focused one.
	 * .TP
	 * .BR x tract
	 * Take out every children of box and place them after focused box.
	 * .TP
	 * .BR X tract
	 * Just like
	 * .B x
	 * but place before.
	 * .TP
	 * .BR f loat
	 * Make box floating inside its parent.
	 * .TP
	 * .BR 1..9
	 * .B f
	 * and place according to usual numeric keypad layout.
	 * .TP
	 * .BR 0
	 * .B f
	 * and place according to Golden Ratio.
	 * .TP
	 * .BR t
	 * Place to alternative position.
	 * .TP
	 * .BR c hase,\  c lear
	 * Chase all floating windows to alternative position.
	 * .RE
	 */
	case XKB_KEY_t:
		hand_start_move(hand);
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
				if (set != box->focus_lock) {
					box->focus_lock = set;
					box->layout_changed = true;
				}
			while (box->num_children <= 1 && (box = box->parent));
		else
			/* set focus lock from input_focus up to focus */
			do
				if (set != (box = box->parent)->focus_lock) {
					box->focus_lock = set;
					box->layout_changed = true;
				}
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
		if (repeating)
			break;

		if (!hand->focus)
			break;

		hand->focus->user_concealed = !hand->focus->user_concealed;
		hand->focus->concealed = sym == XKB_KEY_s || sym == XKB_KEY_plus
			? false
			: hand->focus->user_concealed;

		hand->focus->label_changed = true;
		hand->focus->parent->layout_changed = true;
		box_propagate_change(hand->focus);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-Ctrl-a
	 * Decrease number of visible children.
	 */
	case XKB_KEY_a:
	/*MAN(Keybindings)
	 * .TP
	 * .B Mod-Ctrl-x
	 * Increase number of visible children.
	 */
	case XKB_KEY_x:
		if (repeating)
			break;

		if (!hand->focus)
			break;

		box_step_num_visible(hand->focus, sym == XKB_KEY_a ? 1 : -1);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-m aximize
	 * Toggle between all and one visible children.
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
		if (!repeating &&
		    hand->focus)
			box_maximize(hand->focus, XKB_KEY_f == sym);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-|
	 * Toggle between vertical and horizontal fill modes.
	 */
	case XKB_KEY_backslash:
	case XKB_KEY_bar:
		if (repeating)
			break;

		if (!hand->focus)
			break;

		hand->focus->parent->vertical ^= true;
		box_propagate_change(hand->focus->parent)->layout_changed = true;
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-k
	 * Focus parent.
	 */
	case XKB_KEY_k:
		hand_focus_parent(hand);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-j
	 * Focus child.
	 */
	case XKB_KEY_j:
		hand_focus_child(hand);
		break;

	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-n "ame {" A-Za-z "}... " Return
	 * Name focused box. When left empty, an automatic name will be assigned
	 * to the box.
	 * .IP
	 * Some special keys are also recogzined, namely:
	 * .RS
	 * .TP
	 * .BR + ,\  \-
	 * Show/hide name label.
	 * .TP
	 * .BR "*" ,\  /
	 * On a container show/hide name labels recursively; otherwise
	 * show/hide any labels that obscures part of the window.
	 * .RE
	 */
	case XKB_KEY_n:
		if (repeating)
			break;

		hand_start_name(hand);
		break;

	/* wlose/xlose */
	/*MAN(Keybindings)
	 * .TP
	 * .BR Mod-Ctrl-w indow
	 * Close box just like user would click
	 * \*(lqX\*(rq in the title bar. Kill by force for the second time.
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
	 * .BR Mod-Ctrl-z z
	 * Center pointer inside box.
	 */
	case XKB_KEY_z:
		if (hand->focus)
			hand_center_pointer(hand, hand->focus);
		break;

	default:
		return true;
	}

	hand_update_mode(hand);

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
	if (!device->keymap) {
		fprintf(stderr, "Could not load keymap\n");
		goto out;
	}

	xkb_keysym_t const *syms;
	int const n = xkb_keymap_key_get_syms_by_level(device->keymap, event->detail, 0,
			/* Only care about Shift when user really presses. */
			(XCB_MOD_MASK_SHIFT & event->mods.base ? 1 : 0), &syms);
	if (!n)
		goto out;

	xkb_keysym_t const sym = syms[0];
	if (5 <= HEAWM_VERBOSE)
		printf("key=0x%x mods=%d,%d,%d,%d deviceid=%d sourceid=%d (root=%x)\n", sym,
				event->mods.effective,
				event->mods.base,
				event->mods.latched,
				event->mods.locked,
				event->deviceid, event->sourceid, event->root);

	enum HandMode const old_mode = hand->mode;

	switch (hand->mode) {
	case HAND_MODE_NULL:
	{
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
	XDO(xcb_input_xi_allow_events, conn, XCB_CURRENT_TIME, event->deviceid,
			propagate
				? XCB_INPUT_EVENT_MODE_REPLAY_DEVICE
				: XCB_INPUT_EVENT_MODE_SYNC_DEVICE,
			0, 0);
}

static void
box_snap(Box const *const box, uint16_t distance, int16_t *const px, int16_t *const py, xcb_rectangle_t const *const self)
{
	Box const *const parent = box->parent;

	int16_t const x = *px;
	int16_t const y = *py;
	int16_t tx = x, ty = y;
	uint16_t dx = distance, dy = dx, d;

#define TRY_SNAP(x, width, self_width, to_width) \
	if ((d = abs(x + self->width self_width - to->x - to->width to_width)) < d##x) \
		d##x = d, t##x = to->x + to->width to_width - self->width self_width;
#define TRY_SNAP_AXIS(x, width) \
	TRY_SNAP(x, width, *0, *0) \
	TRY_SNAP(x, width, *0, *1) \
	TRY_SNAP(x, width, *1, *0) \
	TRY_SNAP(x, width, *1, *1) \
	TRY_SNAP(x, width, *0, /2) \
	TRY_SNAP(x, width, /2, /2) \
	TRY_SNAP(x, width, *1, /2)

	xcb_rectangle_t const *to;

	/* Snap to children. */
	for (uint16_t i = 0; i < parent->num_children; ++i) {
		Box const *const child = parent->children[i];
		if (box == child)
			continue;

		to = &child->rect;
		REPEAT_XY(TRY_SNAP_AXIS);
	}

	/* Snap to container boundaries. */
	to = &box->parent->rect;
	REPEAT_XY(TRY_SNAP_AXIS);

#undef TRY_SNAP
#undef TRY_SNAP_AXIS

	*px = tx, *py = ty;
}

static void
handle_input_motion(xcb_input_motion_event_t const *const event)
{
	Device *const device = get_device_by_id(event->sourceid);
	assert(device);
	Hand *const hand = device->hand;

	if (HAND_MODE_POINTER_MOVE == hand->mode) {
		int16_t x = (event->root_x >> 16) + hand->mode_rect.x,
		        y = (event->root_y >> 16) + hand->mode_rect.y;

		uint16_t const distance = XCB_MOD_MASK_SHIFT & event->mods.base ? UINT16_MAX : SNAP_DISTANCE;
		box_snap(hand->mode_box, distance, &x, &y, &hand->mode_box->urect);
		box_set_uposition(hand->mode_box, x, y);
		if (hand->mode_box->position_changed)
			box_propagate_change(hand->mode_box);
	}
}

static void
handle_input_button_release(xcb_input_button_press_event_t const *const event)
{
	Device *const device = get_device_by_id(event->sourceid);
	assert(device);
	Hand *const hand = device->hand;

	if (HAND_MODE_POINTER_MOVE == hand->mode) {
		hand_leave_mode(hand);
		hand_grab_pointer(hand);
	}
}

static void
handle_input_button_press(xcb_input_button_press_event_t const *const event)
{
	Device *const device = get_device_by_id(event->sourceid);
	assert(device);
	Hand *const hand = device->hand;

	if (1 == event->detail) {
		/*MAN(Keybindings)
		 * .TP
		 * .B Mod-PrimaryButtonPress
		 * Focus window and start moving a floating box. Force snap points with
		 * .BR Shift .
		 */
		Body *const body = body_get_by_root(event->root);
		Box *const box = find_box_in_body_by_window(body, offsetof(Box, frame), event->child);
		if (!box)
			return;

		if (HAND_MODE_NULL == hand->mode) {
			Box *const foot = box_get_foot(box);
			if (foot) {
				hand->mode = HAND_MODE_POINTER_MOVE;
				hand->mode_box = foot;
				hand->mode_rect.x = hand->mode_box->urect.x - (event->root_x >> 16);
				hand->mode_rect.y = hand->mode_box->urect.y - (event->root_y >> 16);
				hand_update_mode(hand);
				hand_grab_pointer(hand);
			}
		}

		hand_focus_box(hand, box);
	} else if (3 == event->detail) {
		/*MAN(Keybindings)
		 * .TP
		 * .B Mod-SecondaryButtonPress
		 * Resize floating box. Force snap points with
		 * .BR Shift .
		 */
		if (!hand->focus)
			return;

		Box *foot = box_get_foot(hand->focus);

		if (!foot) {
			box_resize_float(hand->focus, FLOAT_RECT);
			foot = box_get_foot(hand->focus);
		}

		if (!foot)
			return;

		int16_t x = event->root_x >> 16,
			y = event->root_y >> 16;
		xcb_rectangle_t rect = foot->urect;

#define EXTEND(x, width) \
	if (x < rect.x + rect.width / 2) \
		rect.width += rect.x - x, \
		rect.x = x; \
	else \
		rect.width = x - rect.x;
		REPEAT_XY(EXTEND);
#undef EXTEND

		int16_t xw = rect.x + rect.width,
			yh = rect.y + rect.height;

		static xcb_rectangle_t const NULL_RECT = { 0 };

		uint16_t const distance = XCB_MOD_MASK_SHIFT & event->mods.base ? UINT16_MAX : SNAP_DISTANCE;
		box_snap(foot, distance, &rect.x, &rect.y, &NULL_RECT);
		box_snap(foot, distance, &xw, &yh, &NULL_RECT);

		box_set_uposition(foot, rect.x, rect.y);
		box_set_usize(foot, xw - rect.x, yh - rect.y);

		box_propagate_change(foot);
	} else if (4 == event->detail || 5 == event->detail) {
		/*MAN(Keybindings)
		 * .TP
		 * .BR Mod-WheelUp ,\  Mod-WheelDown
		 * Resize floating box.
		 */
		Body *const body = body_get_by_root(event->root);
		Box *const box = find_box_in_body_by_window(body, offsetof(Box, frame), event->child);
		if (!box)
			return;

		if (box->floating) {
			uint16_t const coeff = 100 + 8 * (4 == event->detail ? 1 : -1);
			box_set_usize_with_ureposition(box,
					box->urect.width * coeff / 100,
					box->urect.height * coeff / 100);
			box_propagate_change(box);
		}
	}
}

static void
handle_input_enter(xcb_input_enter_event_t const *const event)
{
	if (XCB_INPUT_NOTIFY_MODE_NORMAL != event->mode)
		return;

	XDO(xcb_input_xi_set_client_pointer, conn, event->child, event->deviceid);
}

static void
handle_input_barrier_hit(xcb_input_barrier_hit_event_t const *const event)
{
	if (hand_find_by_barrier(event->barrier))
		return;

	/* Must be released otherwise moving pointer in and out across barrier
	 * will not notify us for some time. It is also seems important to not
	 * to select BARRIER_LEAVE event. */
	XDO(xcb_input_xi_barrier_release_pointer, conn, 1,
			&(xcb_input_barrier_release_pointer_info_t const){
				.deviceid = event->deviceid,
				.barrier = event->barrier,
				.eventid = event->eventid,
			});

	/* Only first hit is interesting. */
	if (event->dtime)
		return;

	uint16_t d = event->dx.integral * event->dx.integral + event->dy.integral * event->dy.integral;

	if (350 <= d)
		return;

	Box *const box = box_find_by_barrier(root, event->barrier);
	if (!box || root->focus_seq == box->focus_seq)
		return;

	/* Moved inside-out. */
	uint8_t i = event->barrier - box->barrier;
	if ((0 == i && event->dy.integral <= 0) ||
	    (3 == i && 0 <= event->dy.integral) ||
	    (1 == i && event->dx.integral <= 0) ||
	    (2 == i && 0 <= event->dx.integral))
		return;

	xcb_rectangle_t const target = box_get_chase_rect(box);
	box_set_urect(box, target);
	box_propagate_change(box);
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

	case XCB_INPUT_KEY_PRESS:
		handle_input_key_press((void const *)event);
		break;

	case XCB_INPUT_ENTER:
		handle_input_enter((void const *)event);
		break;

	case XCB_INPUT_BARRIER_HIT:
		handle_input_barrier_hit((void const *)event);
		break;

	case XCB_INPUT_BUTTON_PRESS:
		handle_input_button_press((void const *)event);
		break;

	case XCB_INPUT_BUTTON_RELEASE:
		handle_input_button_release((void const *)event);
		break;

	case XCB_INPUT_MOTION:
		handle_input_motion((void const *)event);
		break;
	}
}

static void
handle_shape_notify(xcb_shape_notify_event_t const *const event)
{
	Box *const box = box_find_by_window(root, offsetof(Box, window), event->affected_window);
	if (!box)
		return;

	if (XCB_SHAPE_SK_BOUNDING == event->shape_kind)
		box->shaped = event->shaped;

	if (box_update_shape(box))
		/* Nothing, already done. */;
	else if (event->shaped)
		XDO(xcb_shape_combine, conn, XCB_SHAPE_SO_SET,
				event->shape_kind, event->shape_kind,
				box->frame,
				0, 0, /* Offset. */
				box->window);
	else
		XDO(xcb_shape_mask, conn,
				event->shape_kind, event->shape_kind,
				box->frame,
				0, 0, /* Offset. */
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
	Device *const device = get_device_by_id(device_id);
	if (!device) {
		/* It is currently unclear under what circumstances can it occur,
		 * but it occurs. */
		return;
	}

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

static int
run(void)
{
	struct pollfd pfd;
	pfd.fd = xcb_get_file_descriptor(conn);
	pfd.events = POLLIN;

	assert(!xcb_connection_has_error(conn));

	for (xcb_generic_event_t *event;; free(event)) {
		while (!(event = xcb_poll_for_event(conn)) &&
		       /* Update screen if we have no more pending requests. */
		       /* FIXME: Investigate whether starvation could be a problem. */
		       (do_update(), xcb_flush(conn),
		        /* Make XCB poll()-ing again to see whether we
		         * have received something after flushing. poll()
		         * should be level-triggered so not sure why it
		         * is needed... */
		        !(event = xcb_poll_for_event(conn))))
		{
			int res = poll(&pfd, 1, GC_INTERVAL);
			if (!res)
				do_gc();
			else if (res < 0 || (pfd.revents & ~POLLIN))
				return EXIT_FAILURE;
		}

		if (5 <= HEAWM_VERBOSE)
			printf("event = (%d)%s\n", event->response_type, xcb_event_get_label(event->response_type));

		switch (XCB_EVENT_RESPONSE_TYPE(event)) {
#define EVENT(type, handler) case type: handler((void *)event); break;
		EVENT(XCB_ERROR_NOTIFY,      handle_error);
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
				"%s/%s", env, WM_NAME);
	} else if ((env = getenv("HOME"))) {
		struct stat st;

		snprintf(config.heawm_home, sizeof config.heawm_home,
				"%s/.%s", env, WM_NAME);

		if (stat(config.heawm_home, &st) || !S_ISDIR(st.st_mode))
			snprintf(config.heawm_home, sizeof config.heawm_home,
					"%s/.config/%s", env, WM_NAME);
	} else {
		strcpy(config.heawm_home, ".");
	}
	/* Make sure environment variable set so can be used by scripts. */
	setenv("HEAWM_HOME", config.heawm_home, false);

	config.heawm_home_size = strlen(config.heawm_home);
	if (sizeof config.heawm_home < config.heawm_home_size + 1 /* / */ + SCRIPT_SIZE_MAX + 1 /* NUL */) {
		fprintf(stderr, "HEAWM_HOME is too long\n");
		abort();
	}
	config.heawm_home[config.heawm_home_size++] = '/';

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
			printf(VERSION"\n");
			exit(EXIT_SUCCESS);

		case '?':
		case ':':
			fprintf(stderr, "Option -%c is invalid or requires an argument\n",
					optopt);
			exit(EXIT_FAILURE);

		default:
			abort();
		}
	}

	init_config();

	setup_display();

	/*MAN(HOOKS)
	 * .TP
	 * .B startup
	 * Run on program (re)start.
	 */
	SPAWN(get_script_path("startup"));

	return run();
}
