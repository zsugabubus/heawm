#define _GNU_SOURCE

#include <assert.h>
#include <cairo/cairo-xcb.h>
#include <cairo/cairo.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/poll.h>
#include <sys/queue.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <xcb/composite.h>
#include <xcb/randr.h>
#include <xcb/shape.h>
#include <xcb/xcb.h>
#include <xcb/xcb_atom.h>
#include <xcb/xcb_cursor.h>
#include <xcb/xcb_event.h>
#include <xcb/xcb_ewmh.h>
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/xinput.h>
#include <xcb/xkb.h>
#include <xcb/xproto.h>
#include <xkbcommon/xkbcommon-x11.h>
#include <xkbcommon/xkbcommon.h>

#include "win_hash.h"

/*
 * Checklist:
 * - xcb_send_event uses XCB_SEND_EVENT_EVENT.
 */

#define DEBUG_XTRACE 0

#define sizeof_member(type, member) (sizeof(((type *)0)->member))

#define XASSERT(cond) do { \
	if (!(cond)) \
		abort(); \
} while (0)

#ifndef M_PHI
# define M_PHI 1.6180339887 /** Golden Ratio */
#endif

#define SWAP(x, y, type) do { \
	type tmp = x; \
	x = y; \
	y = tmp; \
} while (0)

#define ARRAY_SIZE(...) \
	(sizeof (__VA_ARGS__) / sizeof *(__VA_ARGS__))

#define ARRAY_FOREACH(var, array) \
	for (var = array; var < (&array)[1]; ++var)

#define ARRAY_IFOREACH(i, array) \
	for (size_t i = 0; i < ARRAY_SIZE(array); ++i)

/* It would be so fucking hard to copy-paste BSD's queue.h. */
#ifndef TAILQ_FOREACH_SAFE
# define TAILQ_FOREACH_SAFE(var, head, field, temp_var) \
	for (var = TAILQ_FIRST(head); \
	     var && (temp_var = TAILQ_NEXT(var, field), 1); \
	     var = temp_var)
#endif

#ifndef LIST_FOREACH_SAFE
# define LIST_FOREACH_SAFE(var, head, field, temp_var) \
	for (var = LIST_FIRST(head); \
	     var && (temp_var = LIST_NEXT(var, field), 1); \
	     var = temp_var)
#endif

#define TAILQ_SWAP(elm1, list1, elm2, list2, elmname, field) do { \
	struct elmname after1, after2; \
 \
	TAILQ_INSERT_AFTER(list1, elm1, &after1, field); \
	TAILQ_INSERT_AFTER(list2, elm2, &after2, field); \
 \
	TAILQ_REMOVE(list2, elm2, field); \
	TAILQ_REMOVE(list1, elm1, field); \
 \
	TAILQ_INSERT_BEFORE(&after1, elm2, field); \
	TAILQ_INSERT_BEFORE(&after2, elm1, field); \
 \
	TAILQ_REMOVE(list1, &after1, field); \
	TAILQ_REMOVE(list2, &after2, field); \
} while (0)

#if DEBUG_XTRACE

struct xdo_cookie {
	xcb_void_cookie_t cookie;
	int line;
	char const *request;
};

# define XDO_COOKIE(xcb_request, ...) (struct xdo_cookie const){ \
	.line = __LINE__, \
	.request = #xcb_request, \
	.cookie = xcb_request##_checked(__VA_ARGS__) \
}

# define XDO(xcb_request, ...) \
	(void)XDO_CHECK(xcb_request, __VA_ARGS__)

# define XDISCARD(xcb_request, conn, ...) do { \
	XGET(reply, xcb_request, conn, __VA_ARGS__); \
	free(reply); \
} while (0)

# define XGET_COOKIE(cookie, xcb_request, ...) \
	__attribute__((unused)) \
	int const cookie##_line = __LINE__; \
	xcb_request##_cookie_t cookie = ((void)xcb_request##_unchecked, xcb_request(__VA_ARGS__));

# define XGET_REPLY(reply, xcb_request, conn, cookie) \
	xcb_request##_reply_t *reply; do { \
	xcb_generic_error_t *reply##_e; \
	reply = xcb_request##_reply(conn, cookie, &reply##_e); \
	if (reply##_e) { \
		fprintf(stderr, "[line %d in %s] ", cookie##_line, #xcb_request); \
		print_error(reply##_e); \
		free(reply##_e); \
	} \
} while (0)

#else

struct xdo_cookie {
	xcb_void_cookie_t cookie;
};

# define XDO_COOKIE(xcb_request, ...) (struct xdo_cookie const){ \
	.cookie = xcb_request##_checked(__VA_ARGS__) \
}

# define XDO(xcb_request, ...) do { \
	(void)xcb_request##_checked; \
	xcb_void_cookie_t void_cookie = xcb_request(__VA_ARGS__); \
	(void)void_cookie; /* Only for the cast. */ \
} while (0)

# define XDISCARD(xcb_request, conn, ...) \
	xcb_discard_reply(conn, xcb_request##_unchecked(conn, __VA_ARGS__).sequence)

# define XGET_COOKIE(cookie, xcb_request, ...) \
	xcb_request##_cookie_t cookie = xcb_request##_unchecked(__VA_ARGS__);

# define XGET_REPLY(reply, xcb_request, conn, cookie) \
	xcb_request##_reply_t *reply = xcb_request##_reply(conn, cookie, NULL);

#endif

#define XGET_REPLY_LOCAL(reply, xcb_request, conn, cookie) \
	xcb_request##_cookie_t _cookie = (cookie); \
	__attribute__((unused)) \
	int const _cookie##_line = __LINE__; \
	XGET_REPLY(reply, xcb_request, conn, _cookie)

#define XDO_CHECK(xcb_request, ...) \
	xdo_check(XDO_COOKIE(xcb_request, __VA_ARGS__))

#define XGET(reply, xcb_request, conn, ...) \
	XGET_COOKIE(reply##_cookie, xcb_request, conn, __VA_ARGS__); \
	XGET_REPLY(reply, xcb_request, conn, reply##_cookie); \

/* xcb_send_event() reads a fixed number of bytes for the event, thus we should
 * provide padding bytes when the actual size of the event is smaller. */
#define XCB_SEND_EVENT_EVENT(type, ... /* initalizer */) \
	(char const *)&(union { \
		type event; \
		char pad[sizeof_member(xcb_send_event_request_t, event)]; \
	} const){ \
		.event = __VA_ARGS__ \
	}

#define XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(grab_window, deviceid, grab_type, detail, ...) \
	XDO(xcb_input_xi_passive_ungrab_device, conn, (grab_window), \
			(detail), (deviceid), \
			ARRAY_SIZE((uint32_t const[])__VA_ARGS__), \
			(grab_type), \
			(uint32_t const[])__VA_ARGS__)

#define XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(grab_window, cursor, deviceid, grab_type, detail, grab_mode, owner_events, mask, ...) \
	XDISCARD(xcb_input_xi_passive_grab_device, conn, XCB_CURRENT_TIME, grab_window, \
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

enum { XCB_MOD_MASK_NUM_LOCK = XCB_MOD_MASK_2, };

#define EFFECTIVE_MASK(mask) \
	                                            (mask), \
	XCB_MOD_MASK_LOCK |                         (mask), \
	                    XCB_MOD_MASK_NUM_LOCK | (mask), \
	XCB_MOD_MASK_LOCK | XCB_MOD_MASK_NUM_LOCK | (mask)

enum { XCB_ERROR_NOTIFY = 0 };

#define RGB8_TO_FLOATS(color) \
	(uint8_t)((color) >> 16) / 256., \
	(uint8_t)((color) >> 8 ) / 256., \
	(uint8_t)((color)      ) / 256.

#define WM_NAME "heawm"
#define LABEL_INSTANCE "Label"
#define WIN_FRAME_INSTANCE "Frame"

static xcb_screen_t *screen;
static xcb_visualtype_t *visual_type;
static xcb_cursor_t move_cursor;
static xcb_cursor_t default_cursor;
static xcb_window_t active_window;

static uint8_t xi_opcode;
static uint8_t shape_first_event;
static uint8_t randr_first_event;
static uint8_t xkb_first_event;
static struct xkb_context *xkb_context;

static sigset_t saved_set;
/* There is no fine grained window change tracking. Whenever something related
 * to windows change server_update_wins() should run, that reconfigures all
 * windows. */
static bool wins_changed;

#define ATOM(name) ((xcb_atom_t const)atoms[ATOM_##name])

#define NET_ATOMS \
	xmacro(_NET_ACTIVE_WINDOW) \
	xmacro(_NET_CLIENT_LIST) \
	xmacro(_NET_CLOSE_WINDOW) \
	xmacro(_NET_SUPPORTED) \
	xmacro(_NET_SUPPORTING_WM_CHECK) \
	xmacro(_NET_WM_NAME) \
	xmacro(_NET_WM_PID) \
	xmacro(_NET_WM_STATE) \
	xmacro(_NET_WM_STATE_DEMANDS_ATTENTION) \
	xmacro(_NET_WM_STATE_FOCUSED) \
	xmacro(_NET_WM_STATE_FULLSCREEN) \
	xmacro(_NET_WM_STATE_HIDDEN) \
	xmacro(_NET_WM_WINDOW_TYPE) \
	xmacro(_NET_WM_WINDOW_TYPE_SPLASH) \

#define HEAWM_ATOMS \
	xmacro(_HEAWM_NAME) \
	xmacro(_HEAWM_LABEL) \

#define ATOMS \
	/* Must start with _NET_*. See screen_setup_net(). */ \
	NET_ATOMS \
	HEAWM_ATOMS \
	xmacro(WM_CLIENT_LEADER) \
	xmacro(WM_WINDOW_ROLE) \
	xmacro(WM_DELETE_WINDOW) \
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
#define xmacro(name) ATOM_##name,
	ATOMS
#undef xmacro
};

static xcb_atom_t atoms[ARRAY_SIZE(ATOM_NAMES)];

static xcb_connection_t *conn;

static char const *label_font = "monospace";
static int32_t label_font_size_pt = 17;
static uint32_t label_bg = 0xff0000;
static uint32_t label_stroke_color = 0x000000;
static uint32_t label_fg = 0xffff00;
static uint32_t label_urgent_bg = 0xffdf5f;

static uint32_t mod_super = XCB_MOD_MASK_4;

struct device {
	struct user *user;
	struct xkb_keymap *keymap; /* NULL if not keyboard */
};

static struct device devices[UINT8_MAX + 1];

struct user {
	TAILQ_ENTRY(user) link;
	struct win *focused_win; /* May NULL. */
	struct tab *alt_tab; /* May NULL. */
	struct win *alt_win; /* May NULL. */
	char *name;
	enum mode {
		MODE_NORMAL,
		MODE_COMMAND,
		MODE_MOUSE,
		MODE_INSERT,
		MODE_LABEL,
	} mode;
	uint32_t num;
	int16_t pointer_x, pointer_y;
	xcb_input_device_id_t master_pointer, master_keyboard;
};

static TAILQ_HEAD(, user) users = TAILQ_HEAD_INITIALIZER(users);

struct proc {
	LIST_ENTRY(proc) link;
	struct user *user;
	pid_t pid;
	void *data;
};

static LIST_HEAD(, proc) procs = LIST_HEAD_INITIALIZER(procs);
static TAILQ_HEAD(latest_wins, win) latest_wins = TAILQ_HEAD_INITIALIZER(latest_wins);

struct output {
	TAILQ_ENTRY(output) link;
	xcb_rectangle_t geom;
	char *name;
	bool primary;
};

static TAILQ_HEAD(, output) outputs = TAILQ_HEAD_INITIALIZER(outputs);

struct session {
	TAILQ_ENTRY(session) link;
	TAILQ_HEAD(session_tabs, tab) tabs;
	/* INVARIANT: w->mapped => w->tab->session->mapped */
	bool mapped;
};

static TAILQ_HEAD(sessions, session) sessions = TAILQ_HEAD_INITIALIZER(sessions);

struct tab {
	TAILQ_ENTRY(tab) link;
	struct session *session;
	struct output *output;
	TAILQ_HEAD(tab_wins, win) wins;
	/* INVARIANT: w->mapped => w->tab->mapped */
	bool mapped;

	struct win *zoomed_win;
	char master; /* NIL: off, hjkl: on */
	uint8_t mfact; /* [1,9] */
	bool monocle;
	int32_t cols; /* <0: fix rows; =0: auto; >0: fix cols. */
};

struct win_label {
	xcb_window_t window;
	xcb_pixmap_t shape;
	xcb_rectangle_t geom;
	cairo_t *cr;
	cairo_t *shape_cr;
	cairo_surface_t *surface;
	cairo_surface_t *shape_surface;
	bool mapped;
};

struct win {
	TAILQ_ENTRY(win) link;
	TAILQ_ENTRY(win) older;
	struct tab *tab;

	xcb_rectangle_t geom, user_geom;
	xcb_input_fp1616_t pointer_x, pointer_y;
	char *name;
	char label;
	bool urgent: 1;
	bool floating: 1;
	bool shaped: 1;
	bool mapped: 1;
	uint8_t focused;

	xcb_window_t window;
	xcb_window_t frame;
	xcb_window_t leader;
	xcb_window_t transient_for;
	char *title;
	char *class_instance, *class_class;
	char *role;
	xcb_atom_t type;
	pid_t pid;
	bool fullscreen: 1;

	struct win_label win_label;
};

static void
print_error(xcb_generic_error_t const *error)
{
	fprintf(stderr, "X error: "
#if DEBUG_XTRACE
			"%s"
#else
			"error code: %d"
#endif
			", sequence: %d, resource id: %d=0x%x, major code: %d, minor code: %d\n",
#if DEBUG_XTRACE
			xcb_event_get_error_label(error->error_code),
#else
			error->error_code,
#endif
			error->sequence,
			error->resource_id, error->resource_id,
			error->major_code, error->minor_code);
}

static int
xdo_check(struct xdo_cookie cookie)
{
	int ret = 0;

	xcb_generic_error_t *error = xcb_request_check(conn, cookie.cookie);
	if (error) {
#if DEBUG_XTRACE
		fprintf(stderr, "[line %d in %s] ", cookie.line, cookie.request);
#endif
		print_error(error);
		ret = error->error_code;
		free(error);
	}

	return ret;
}

struct grid {
	xcb_rectangle_t geom;
	uint32_t cols, rows, last_cols;
	uint32_t x, y;
};

static void
grid_init(struct grid *g, int32_t ntiles, xcb_rectangle_t const *geom, int32_t cols)
{
	if (!cols) {
		uint32_t optimum = UINT32_MAX;
		g->rows = 0;
		while (++cols <= ntiles) {
			uint32_t rows = (ntiles + cols - 1) / cols;
			uint32_t last_cols = ntiles - (rows - 1) * cols;
			/* Length of internal separators. */
			uint32_t perimeter =
				geom->width * (rows - 1) +
				(geom->height / rows) * (
					(cols - 1)      * (rows - 1) +
					(last_cols - 1) * 1
				);
			if (perimeter < optimum && rows != g->rows) {
				g->cols = cols;
				g->rows = rows;
				g->last_cols = last_cols;
				optimum = perimeter;
			}
		}
	} else {
		if (cols < 0) {
			cols = -cols;
			if (ntiles < cols)
				cols = ntiles;
			g->cols = (ntiles + cols - 1) / cols;
			g->rows = (ntiles + g->cols - 1) / g->cols;
		} else if (0 < cols) {
			if (ntiles < cols)
				cols = ntiles;
			g->cols = cols;
			g->rows = (ntiles + cols - 1) / cols;
		}
		g->last_cols = ntiles - (g->rows - 1) * g->cols;
	}

	g->geom = *geom;
	g->x = 0;
	g->y = 0;
}

static xcb_rectangle_t
grid_next(struct grid *g)
{
	if (g->cols <= g->x) {
		++g->y, g->x = 0;
		if (g->y + 1 == g->rows)
			g->cols = g->last_cols;
	}

	int16_t gap = 1;
	int16_t w = g->geom.width - (g->cols - 1) * gap;
	int16_t h = g->geom.height - (g->rows - 1) * gap;

	xcb_rectangle_t ret;

	ret.x      = w * g->x       / g->cols;
	ret.y      = h * g->y       / g->rows;
	ret.width  = w * (g->x + 1) / g->cols - ret.x;
	ret.height = h * (g->y + 1) / g->rows - ret.y;

	ret.x += g->geom.x + g->x * gap;
	ret.y += g->geom.y + g->y * gap;

	++g->x;

	return ret;
}

static struct win *
tab_get_latest_slave_win(struct tab const *t)
{
	struct win const *w;
	TAILQ_FOREACH_REVERSE(w, &latest_wins, latest_wins, older)
		if (w->tab == t && !w->floating && (!t->master || TAILQ_PREV(w, tab_wins, link)))
			return (struct win *)w;
	return NULL;
}

static void
win_update_shape(struct win const *w, xcb_shape_kind_t kind)
{
	if (w->shaped) {
		XDO(xcb_shape_combine, conn, XCB_SHAPE_SO_SET,
				kind, kind,
				w->frame,
				0, 0,
				w->window);
		if (w->win_label.mapped && kind != XCB_SHAPE_SK_INPUT)
			XDO(xcb_shape_mask, conn, XCB_SHAPE_SO_UNION,
					kind,
					w->frame,
					w->win_label.geom.x,
					w->win_label.geom.y,
					w->win_label.shape);
	} else {
		XDO(xcb_shape_mask, conn,
				kind, kind,
				w->frame,
				0, 0,
				XCB_PIXMAP_NONE);
	}
}

static double
screen_pt2px(int32_t pt)
{
	return (double)(pt * 254 * screen->width_in_pixels)
		/ (720 * screen->width_in_millimeters);
}

static bool
win_is_alt(struct win const *w)
{
	struct user *u;
	TAILQ_FOREACH(u, &users, link)
		if (w == u->alt_win)
			return true;
	return false;
}

static void
win_label_new(struct win_label *wl)
{
	wl->window = xcb_generate_id(conn);
	wl->shape = xcb_generate_id(conn);
}

static void
win_label_del(struct win_label *wl)
{
	XDO(xcb_destroy_window, conn, wl->window);
	XDO(xcb_free_pixmap, conn, wl->shape);
	cairo_destroy(wl->cr);
	cairo_destroy(wl->shape_cr);
	cairo_surface_destroy(wl->surface);
	cairo_surface_destroy(wl->shape_surface);
}

static void
win_label_render(struct win *w, bool shape)
{
	struct win_label const *wl = &w->win_label;
	if (!wl->mapped)
		return;

	int font_size = screen_pt2px(label_font_size_pt);

	char text[2] = {
		w->label,
		'\0',
	};

	cairo_surface_t *surface = shape ? wl->shape_surface : wl->surface;
	cairo_t *cr = shape ? wl->shape_cr : wl->cr;

	if (!shape)
		cairo_push_group(cr);
	cairo_save(cr);

	cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	cairo_select_font_face(cr, label_font,
			CAIRO_FONT_SLANT_NORMAL,
			CAIRO_FONT_WEIGHT_BOLD);
	cairo_set_font_size(cr, font_size);

	if (shape) {
		cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
		cairo_rectangle(cr, 0, 0, wl->geom.width, wl->geom.height);
		cairo_fill(cr);
	}

	cairo_translate(cr, wl->geom.width / 2, wl->geom.height / 2);

	cairo_text_extents_t te;
	cairo_text_extents(cr, text, &te);

	int radius = font_size / 2;

	if (w->focused) {
		if (shape) {
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
		} else {
			cairo_set_source_rgb(cr, RGB8_TO_FLOATS(label_bg));
			cairo_move_to(cr, 0, 0);
		}
		cairo_arc(cr, 0, 0, radius * sqrt(M_PHI), 0, 2 * M_PI);
		cairo_fill(cr);
	} else if (w->urgent) {
		if (shape) {
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
		} else {
			cairo_set_source_rgb(cr, RGB8_TO_FLOATS(label_urgent_bg));
			cairo_move_to(cr, 0, 0);
		}
		cairo_arc(cr, 0, 0, radius * sqrt(M_PHI), 0, 2 * M_PI);
		cairo_fill(cr);
	} else if (win_is_alt(w)) {
		if (shape) {
			cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
		} else {
			cairo_set_source_rgb(cr, RGB8_TO_FLOATS(label_bg));
			cairo_move_to(cr, 0, 0);
		}
		cairo_set_line_width(cr, 1);
		cairo_arc(cr, 0, 0, radius * sqrt(M_PHI) - 1, 0, 2 * M_PI);
		cairo_stroke(cr);
	}

	double te_left = -(te.width + te.x_bearing) / 2;
	double te_top = -te.y_bearing - te.height / 2;

	cairo_set_antialias(cr, CAIRO_ANTIALIAS_BEST);

	if (!shape)
		cairo_set_source_rgb(cr, RGB8_TO_FLOATS(label_stroke_color));
	else
		cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
	cairo_move_to(cr, te_left, te_top);
	cairo_text_path(cr, text);
	cairo_set_line_width(cr, 2.5);
	cairo_stroke(cr);

	if (!shape)
		cairo_set_source_rgb(cr, RGB8_TO_FLOATS(label_fg));
	cairo_move_to(cr, te_left, te_top);
	cairo_show_text(cr, text);

	cairo_restore(cr);
	if (!shape) {
		cairo_pop_group_to_source(cr);
		cairo_paint(cr);
	}

	cairo_surface_flush(surface);
}

static void
win_label_shape_and_render(struct win *w)
{
	struct win_label *wl = &w->win_label;
	if (!wl->mapped)
		return;

	win_label_render(w, true);
	XDO(xcb_shape_mask, conn,
			XCB_SHAPE_SO_SET, XCB_SHAPE_SK_BOUNDING,
			wl->window,
			0, 0,
			wl->shape);
	if (w->shaped)
		win_update_shape(w, XCB_SHAPE_SK_BOUNDING);
	win_label_render(w, false);
}

static void
win_label_set_mapped(struct win *w, bool mapped)
{
	struct win_label *wl = &w->win_label;
	if (wl->mapped == mapped)
		return;
	wl->mapped = mapped;

	win_label_shape_and_render(w);

	if (wl->mapped)
		XDO(xcb_map_window, conn, wl->window);
	else
		XDO(xcb_unmap_window, conn, wl->window);
}

static void
win_send_configure_notify(struct win const *w)
{
	XDO(xcb_send_event, conn, false, w->window,
			XCB_EVENT_MASK_STRUCTURE_NOTIFY,
			XCB_SEND_EVENT_EVENT(xcb_configure_notify_event_t, {
				.response_type = XCB_CONFIGURE_NOTIFY,
				.event = w->window,
				.window = w->window,
				.above_sibling = XCB_WINDOW_NONE,
				.x = w->geom.x,
				.y = w->geom.y,
				.width = w->geom.width,
				.height = w->geom.height,
				.border_width = 0,
				/* Surely not if we manage the window. */
				.override_redirect = false,
			}));
}

static void
win_set_geom(struct win *w, xcb_rectangle_t const *geom)
{
	if (!memcmp(&w->geom, geom, sizeof w->geom))
		return;

	/* Maintain north-east gravity. */
	struct win_label *wl = &w->win_label;
	wl->geom.x += geom->width - w->geom.width;

	w->geom = *geom;

	uint32_t mask =
		XCB_CONFIG_WINDOW_X |
		XCB_CONFIG_WINDOW_Y |
		XCB_CONFIG_WINDOW_WIDTH |
		XCB_CONFIG_WINDOW_HEIGHT;
	uint32_t values[6];
	values[0] = w->geom.x;
	values[1] = w->geom.y;
	values[2] = w->geom.width;
	values[3] = w->geom.height;
	if (w->floating) {
		mask |= XCB_CONFIG_WINDOW_STACK_MODE;
		values[4] = XCB_STACK_MODE_ABOVE;
	} else {
		mask |= XCB_CONFIG_WINDOW_STACK_MODE;
		values[4] = XCB_STACK_MODE_BELOW;
	}
	XDO(xcb_configure_window, conn, w->frame, mask, values);

	mask = XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT;
	values[0] = w->geom.width;
	values[1] = w->geom.height;
	XDO(xcb_configure_window, conn, w->window, mask, values);
	/* Not sure why it is needed. */
	win_send_configure_notify(w);

	if (wl->mapped && w->shaped)
		win_update_shape(w, XCB_SHAPE_SK_BOUNDING);
}

static void
win_update_net(struct win *w)
{
	xcb_atom_t list[3];
	uint32_t i = 0;

	if (!w->mapped)
		list[i++] = ATOM(_NET_WM_STATE_HIDDEN);
	else if (w->focused)
		list[i++] = ATOM(_NET_WM_STATE_FOCUSED);

	if (w->fullscreen)
		list[i++] = ATOM(_NET_WM_STATE_FULLSCREEN);

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			w->window, ATOM(_NET_WM_STATE),
			XCB_ATOM_ATOM, 32,
			i, list);
}

static void
xcb_icccm_set_wm_state(xcb_window_t window, xcb_icccm_wm_state_t state)
{
	typedef struct {
		uint32_t state;
		xcb_window_t icon;
	} xcb_icccm_wm_state_data_t;

	/* http://tronche.com/gui/x/icccm/sec-4.html#s-4.1.3.1 */
	if (XCB_ICCCM_WM_STATE_WITHDRAWN == state)
		XDO(xcb_delete_property, conn, window, ATOM(WM_STATE));
	else
		XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				window, ATOM(WM_STATE),
				ATOM(WM_STATE), 32,
				sizeof(xcb_icccm_wm_state_data_t) / sizeof(uint32_t),
				&(xcb_icccm_wm_state_data_t const){
					.state = state,
					.icon = XCB_WINDOW_NONE,
				});
}

static void
win_set_mapped(struct win *w, bool mapped)
{
	if (w->mapped == mapped)
		return;
	w->mapped = mapped;

	win_update_net(w);
	if (w->mapped) {
		xcb_icccm_set_wm_state(w->window, XCB_ICCCM_WM_STATE_NORMAL);
		XDO(xcb_map_window, conn, w->frame);
	} else {
		xcb_icccm_set_wm_state(w->window, XCB_ICCCM_WM_STATE_ICONIC);
		XDO(xcb_unmap_window, conn, w->frame);
	}
}

static void
win_update_label(struct win *w, char label)
{
	if (w->label == label)
		return;
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			w->window, ATOM(_HEAWM_LABEL),
			ATOM(UTF8_STRING), 8,
			label != '\0', &label);
}

static void
win_set_auto_label(struct win *w)
{
	uint16_t counts[UINT8_MAX + 1];
	memset(&counts, 0, sizeof counts);

	struct win *ww;
	TAILQ_FOREACH(ww, &latest_wins, older)
		++counts[(unsigned)ww->label];

	char new_label = ' ';
	counts[(unsigned)new_label] = UINT16_MAX;

	for (char c = 'a'; c <= 'z'; ++c)
		if (counts[(unsigned)c] < counts[(unsigned)new_label])
			new_label = c;

	win_update_label(w, new_label);
}

static void
tab_update_wins(struct tab *t)
{
	bool mapped = t->session->mapped && t->output;
	if (t->mapped == mapped && !mapped)
		return;
	t->mapped = mapped;

	struct win *w;

	if (!t->mapped) {
		TAILQ_FOREACH(w, &t->wins, link)
			win_set_mapped(w, false);
		return;
	}

	xcb_rectangle_t geom = t->output->geom;

	if (t->zoomed_win && TAILQ_FIRST(&sessions) == t->session) {
		win_set_geom(t->zoomed_win, &geom);
		win_set_mapped(t->zoomed_win, true);
		win_label_set_mapped(t->zoomed_win, false);
		TAILQ_FOREACH(w, &t->wins, link)
			if (w != t->zoomed_win)
				win_set_mapped(w, false);
		return;
	}

	xcb_rectangle_t master_geom = geom;
	xcb_rectangle_t slave_geom = geom;

	int32_t tiles = 0;
	TAILQ_FOREACH(w, &t->wins, link)
		tiles += !w->floating;

	switch (t->master) {
	case '\0':
		slave_geom = geom;
		break;

#define xmacro(h, l, x, width) \
	case h: \
	case l: \
	{ \
		int16_t size = geom.width * t->mfact / 10; \
		int16_t gap = 1, gap_start = gap / 2, gap_end = gap - gap_start; \
		if (h == t->master) { \
			master_geom.width = size - gap_end; \
			slave_geom.x += size + gap_start; \
			slave_geom.width -= size + gap_start; \
		} else { \
			slave_geom.width = master_geom.width - size - gap_end; \
			master_geom.x += master_geom.width - (size - gap_start); \
			master_geom.width = size - gap_start; \
		} \
		break; \
	}
	xmacro('h', 'l', x, width)
	xmacro('k', 'j', y, height)
#undef xmacro
	}

	int32_t nmaster = t->master ? 1 : 0;
	int32_t nslave = tiles - nmaster;
	struct win *mono_slave = t->monocle ? tab_get_latest_slave_win(t) : NULL;

	struct grid slave_grid;
	grid_init(&slave_grid, nslave, &slave_geom, t->cols);

	TAILQ_FOREACH(w, &t->wins, link) {
		xcb_rectangle_t tile;
		if (w->floating) {
			tile = w->user_geom;

#define xmacro(x, width) do { \
	int16_t min = geom.x, max = geom.x + geom.width - tile.width; \
	if (max < min) \
		SWAP(min, max, int16_t); \
	if (tile.x < min) \
		tile.x = min; \
	else if (max < tile.x) \
		tile.x = max; \
} while (0)
			xmacro(x, width);
			xmacro(y, height);
#undef xmacro
		} else if (0 < nmaster) {
			--nmaster;
			tile = master_geom;
		} else if (0 < nslave) {
			--nslave;
			if (w == mono_slave) {
				tile = slave_geom;
			} else if (mono_slave) {
				win_set_mapped(w, false);
				continue;
			} else {
				tile = grid_next(&slave_grid);
			}
		} else {
			win_set_mapped(w, false);
			continue;
		}

		win_set_geom(w, &tile);
		win_set_mapped(w, true);
		win_label_set_mapped(w, true);
	}
}

static bool
session_is_floating(struct session const *s)
{
	struct tab *t;
	struct win *w;
	TAILQ_FOREACH(t, &s->tabs, link)
	TAILQ_FOREACH(w, &t->wins, link)
		if (!w->floating)
			return false;
	return true;
}

static void
session_update_wins(struct session *s)
{
	bool mapped = TAILQ_FIRST(&sessions) == s || session_is_floating(s);
	if (s->mapped == mapped && !mapped)
		return;
	s->mapped = mapped;

	struct tab *t;
	TAILQ_FOREACH(t, &s->tabs, link)
		tab_update_wins(t);
}

static void
server_update_wins(void)
{
	if (!wins_changed)
		return;
	wins_changed = false;

	struct session *s;
	TAILQ_FOREACH(s, &sessions, link)
		session_update_wins(s);
}

static void
session_update_tabs(struct session *s)
{
	struct output *o = TAILQ_FIRST(&outputs);
	struct tab *t;
	TAILQ_FOREACH(t, &s->tabs, link) {
		t->output = o;
		if (o)
			o = TAILQ_NEXT(o, link);
	}

	wins_changed = true;
}

static void
tab_swap(struct tab *x, struct tab *y)
{
	TAILQ_SWAP(x, &x->session->tabs, y, &y->session->tabs, tab, link);
	SWAP(x->session, y->session, struct session *);
	session_update_tabs(x->session);
	session_update_tabs(y->session);
}

static void
output_del(struct output *o)
{
	struct session *s;
	TAILQ_FOREACH(s, &sessions, link) {
		struct tab *t;
		TAILQ_FOREACH(t, &s->tabs, link) {
			if (t->output == o) {
				t->output = NULL;
				break;
			}
		}
		session_update_tabs(s);
	}

	TAILQ_REMOVE(&outputs, o, link);
	free(o->name);
	free(o);
}

static struct session *
session_new(void)
{
	struct session *s;
	XASSERT(s = calloc(1, sizeof *s));

	TAILQ_INSERT_TAIL(&sessions, s, link);
	TAILQ_INIT(&s->tabs);

	wins_changed = true;

	return s;
}

static void
session_del(struct session *s)
{
	assert(TAILQ_EMPTY(&s->tabs));
	TAILQ_REMOVE(&sessions, s, link);
	free(s);
}

static struct tab *
tab_new(struct session *s)
{
	struct tab *t;
	XASSERT(t = calloc(1, sizeof *t));

	TAILQ_INIT(&t->wins);
	TAILQ_INSERT_TAIL(&s->tabs, t, link);
	t->session = s;
	session_update_tabs(t->session);

	return t;
}

static void
tab_del(struct tab *t)
{
	assert(TAILQ_EMPTY(&t->wins));

	struct user *u;
	TAILQ_FOREACH(u, &users, link)
		if (t == u->alt_tab)
			u->alt_tab = NULL;

	struct tab *last = TAILQ_LAST(&t->session->tabs, session_tabs);
	if (last != t && t->output) {
		TAILQ_REMOVE(&t->session->tabs, last, link);
		TAILQ_INSERT_BEFORE(t, last, link);
	}

	TAILQ_REMOVE(&t->session->tabs, t, link);
	if (TAILQ_EMPTY(&t->session->tabs))
		session_del(t->session);
	else
		session_update_tabs(t->session);

	free(t);
}

static struct output *
output_new(bool primary, xcb_rectangle_t geom)
{
	struct output *o;
	XASSERT(o = calloc(1, sizeof *o));

	TAILQ_INSERT_TAIL(&outputs, o, link);
	o->geom = geom;
	o->primary = primary;

	struct session *s;
	TAILQ_FOREACH(s, &sessions, link)
		session_update_tabs(s);

	return o;
}


static struct output *
output_new_from_screen(void)
{
	struct output *o = output_new(true, (xcb_rectangle_t){
		.x = 0,
		.y = 0,
		.width = screen->width_in_pixels,
		.height = screen->height_in_pixels,
	});

	XASSERT(o->name = strdup("SCREEN"));

	return o;
}

static char *
get_atom_name(xcb_atom_t atom)
{
	XGET(reply, xcb_get_atom_name, conn, atom);
	char *ret;
	if (reply) {
		char *buf = xcb_get_atom_name_name(reply);
		int sz = xcb_get_atom_name_name_length(reply);
		ret = strndup(buf, sz);
		free(reply);
	} else {
		ret = strdup("");
	}
	XASSERT(ret);
	return ret;
}

__attribute__((unused))
static void
print_atom(char const *name, xcb_atom_t atom)
{
	char *s = get_atom_name(atom);
	fprintf(stderr, "%s %s\n", name, s);
	free(s);
}

static struct output *
output_new_from_randr_monitor(xcb_randr_monitor_info_t const *monitor)
{
	struct output *o = output_new(monitor->primary, (xcb_rectangle_t){
		.x = monitor->x,
		.y = monitor->y,
		.width = monitor->width,
		.height = monitor->height,
	});

	XASSERT(o->name = get_atom_name(monitor->name));

	return o;
}

static struct proc *
proc_new(struct user *u)
{
	struct proc *proc;
	XASSERT(proc = calloc(1, sizeof *proc));

	LIST_INSERT_HEAD(&procs, proc, link);
	proc->user = u;

	return proc;
}

static void
proc_del(struct proc *proc)
{
	LIST_REMOVE(proc, link);
	free(proc->data);
	free(proc);
}

static struct proc *
proc_find(pid_t pid)
{
	struct proc *proc;
	LIST_FOREACH(proc, &procs, link)
		if (proc->pid == pid)
			return proc;
	return NULL;
}

static pid_t
proc_fork(struct proc *proc)
{
	pid_t pid = fork();
	if (!pid)
		sigprocmask(SIG_SETMASK, &saved_set, NULL);
	else if (0 < pid)
		proc->pid = pid;
	return pid;
}

static void
proc_execvp(struct proc *proc, char const *file, ...)
{
	size_t argc;
	va_list ap;

	va_start(ap, file);
	for (argc = 0; va_arg(ap, char *); ++argc);
	va_end(ap);

	char *argv[argc + 1 /* NULL */];

	va_start(ap, file);
	for (size_t i = 0; i <= argc; ++i)
		argv[i] = va_arg(ap, char *);
	va_end(ap);

	pid_t pid = proc_fork(proc);
	if (!pid) {
		setsid();
		execvp(file, argv);
		_exit(127);
	} else if (pid < 0) {
		proc_del(proc);
	}
}

static uint32_t
user_number(struct user const *u, uint32_t def)
{
	return u->num ? u->num : def;
}

static xcb_window_t
user_get_focus_target(struct user *u)
{
	if (u->focused_win)
		return u->focused_win->window;
	return screen->root;
}

static void
user_update_input_focus(struct user *u)
{
	XDO(xcb_input_xi_set_focus, conn,
			user_get_focus_target(u),
			XCB_CURRENT_TIME, u->master_keyboard);
	XDO(xcb_input_xi_set_client_pointer, conn,
			u->focused_win ? u->focused_win->window : XCB_WINDOW_NONE,
			u->master_pointer);
}

static struct device *
device_find_by_id(xcb_input_device_id_t deviceid)
{
	assert(deviceid < ARRAY_SIZE(devices));
	return &devices[deviceid];
}

static struct win *
tab_get_latest_win(struct tab const *t)
{
	struct win const *w;
	TAILQ_FOREACH_REVERSE(w, &latest_wins, latest_wins, older)
		if (w->tab == t)
			return (struct win *)w;
	return NULL;
}

static struct win *
session_get_latest_win(struct session const *s)
{
	struct win const *w;
	TAILQ_FOREACH_REVERSE(w, &latest_wins, latest_wins, older)
		if (w->tab->session == s)
			return (struct win *)w;
	return NULL;
}

static void
user_save_pointer(struct user *u)
{
	XGET(reply, xcb_input_xi_query_pointer, conn,
			screen->root, u->master_pointer);
	if (!reply)
		return;

	struct win *w = win_hash_get(reply->child);
	if (w) {
		w->pointer_x = reply->root_x - (w->geom.x << 16);
		w->pointer_y = reply->root_y - (w->geom.y << 16);
	}
	free(reply);
}

static void
user_restore_pointer(struct user *u)
{
	struct win *w = u->focused_win;
	if (!w)
		return;

	/* Center pointer by default. */
	if (!w->pointer_x && !w->pointer_y) {
		w->pointer_x = (w->geom.width << 16) / 2;
		w->pointer_y = (w->geom.height << 16) / 2;
	}

	XDO(xcb_input_xi_warp_pointer, conn,
			XCB_WINDOW_NONE,
			w->frame,
			0, 0, 0, 0,
			w->pointer_x, w->pointer_y,
			u->master_pointer);
}

static void
screen_set_active_window(struct win const *w)
{
	xcb_window_t window = w ? w->window : XCB_WINDOW_NONE;
	if (window == active_window)
		return;
	active_window = window;
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			screen->root, ATOM(_NET_ACTIVE_WINDOW),
			XCB_ATOM_WINDOW, 32,
			1, &active_window);
}

static struct user *
user_new(xcb_input_xi_device_info_t const *xdev)
{
	struct user *u;
	XASSERT(u = calloc(1, sizeof *u));

	TAILQ_INSERT_TAIL(&users, u, link);
	u->master_pointer = xdev->deviceid;
	u->master_keyboard = xdev->attachment;

	char const *name = xcb_input_xi_device_info_name(xdev);
	int sz = xcb_input_xi_device_info_name_length(xdev) - strlen(" pointer");
	XASSERT(u->name = strndup(name, sz));

	return u;
}

static void
user_grab_keyboard(struct user *u)
{
	XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(screen->root,
			u->master_keyboard,
			XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
			{ XCB_INPUT_MODIFIER_MASK_ANY });

	switch (u->mode) {
	case MODE_NORMAL:
		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(screen->root,
				XCB_CURSOR_NONE,
				u->master_keyboard,
				XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				XCB_INPUT_GRAB_OWNER_NO_OWNER,
				XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
				{
					EFFECTIVE_MASK(mod_super | 0),
					EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_SHIFT),
					EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_CONTROL),
					EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_SHIFT | XCB_MOD_MASK_CONTROL),
				});

		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(screen->root,
				XCB_CURSOR_NONE,
				u->master_keyboard,
				XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				XCB_INPUT_GRAB_OWNER_NO_OWNER,
				XCB_INPUT_XI_EVENT_MASK_KEY_RELEASE,
				{
					EFFECTIVE_MASK(mod_super | 0),
					EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_SHIFT),
					EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_CONTROL),
					EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_SHIFT | XCB_MOD_MASK_CONTROL),
				});
		break;

	case MODE_INSERT:
		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(screen->root,
				XCB_CURSOR_NONE,
				u->master_keyboard,
				XCB_INPUT_GRAB_TYPE_KEYCODE, 9 /* Ah. Lazy. FOR EACH keycode generates XKB_KEY_Escape. */,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				XCB_INPUT_GRAB_OWNER_NO_OWNER,
				XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
				{
					EFFECTIVE_MASK(mod_super),
				});
		break;

	default:
		XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(screen->root,
				XCB_CURSOR_NONE,
				u->master_keyboard,
				XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
				XCB_INPUT_GRAB_MODE_22_ASYNC,
				XCB_INPUT_GRAB_OWNER_NO_OWNER,
				XCB_INPUT_XI_EVENT_MASK_KEY_PRESS,
				{ XCB_INPUT_MODIFIER_MASK_ANY });
		break;
	}
}

static void
user_grab_pointer(struct user *u)
{
	XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(screen->root,
			u->master_pointer,
			XCB_INPUT_GRAB_TYPE_BUTTON, XCB_GRAB_ANY,
			{ XCB_INPUT_MODIFIER_MASK_ANY });

	XCB_INPUT_XI_PASSIVE_GRAB_DEVICE_WRAPPER(screen->root,
			move_cursor,
			u->master_pointer,
			XCB_INPUT_GRAB_TYPE_BUTTON, XCB_GRAB_ANY,
			XCB_INPUT_GRAB_MODE_22_ASYNC,
			XCB_INPUT_GRAB_OWNER_NO_OWNER,
			XCB_INPUT_XI_EVENT_MASK_BUTTON_PRESS |
			XCB_INPUT_XI_EVENT_MASK_BUTTON_RELEASE |
			XCB_INPUT_XI_EVENT_MASK_MOTION,
			{
				EFFECTIVE_MASK(mod_super),
				EFFECTIVE_MASK(mod_super | XCB_MOD_MASK_SHIFT),
			});
}

static void
user_disable(struct user *u)
{
	XDO(xcb_xkb_select_events, conn, u->master_keyboard,
			0, 0, 0, 0, 0, NULL);

	XCB_INPUT_XI_PASSIVE_UNGRAB_DEVICE_WRAPPER(screen->root,
			u->master_keyboard,
			XCB_INPUT_GRAB_TYPE_KEYCODE, XCB_GRAB_ANY,
			{ XCB_INPUT_MODIFIER_MASK_ANY });

}

static void
user_enable(struct user *u)
{
	user_grab_pointer(u);
	user_grab_keyboard(u);

	enum {
		PER_CLIENT_FLAGS =
			/* Send good state in events. */
			XCB_XKB_PER_CLIENT_FLAG_GRABS_USE_XKB_STATE |
			XCB_XKB_PER_CLIENT_FLAG_LOOKUP_STATE_WHEN_GRABBED,
		REQUIRED_EVENTS =
			XCB_XKB_EVENT_TYPE_MAP_NOTIFY |
			XCB_XKB_EVENT_TYPE_NEW_KEYBOARD_NOTIFY,
		REQUIRED_MAP_PARTS =
			XCB_XKB_MAP_PART_KEY_TYPES |
			XCB_XKB_MAP_PART_KEY_SYMS |
			XCB_XKB_MAP_PART_MODIFIER_MAP |
			XCB_XKB_MAP_PART_EXPLICIT_COMPONENTS |
			XCB_XKB_MAP_PART_KEY_ACTIONS |
			XCB_XKB_MAP_PART_VIRTUAL_MODS |
			XCB_XKB_MAP_PART_VIRTUAL_MOD_MAP,
	};

	XDISCARD(xcb_xkb_per_client_flags, conn,
			u->master_keyboard,
			PER_CLIENT_FLAGS,
			PER_CLIENT_FLAGS,
			0, 0, 0);

	XDO(xcb_xkb_select_events, conn, u->master_keyboard,
			REQUIRED_EVENTS,
			0,
			REQUIRED_EVENTS,
			REQUIRED_MAP_PARTS,
			REQUIRED_MAP_PARTS,
			NULL);
}

static void
user_set_mode(struct user *u, enum mode mode)
{
	if (u->mode == mode)
		return;
	u->mode = mode;
	user_grab_keyboard(u);

	if (MODE_LABEL == u->mode)
		win_update_label(u->focused_win, '?');

	user_update_input_focus(u);
}

static void
user_focus_win(struct user *u, struct win *w)
{
	struct win *oldw = u->focused_win;
	if (oldw == w)
		return;

	struct win *oldaltw = u->alt_win;
	u->alt_win = oldw;
	u->focused_win = w;
	if (TAILQ_FIRST(&users) == u)
		screen_set_active_window(w);

	if (MODE_MOUSE == u->mode || (MODE_LABEL == u->mode && !w))
		user_set_mode(u, MODE_NORMAL);

	struct tab *oldt = NULL;
	if (oldw) {
		--oldw->focused;

		if (!w || oldw->tab != w->tab)
			u->alt_tab = oldw->tab;

		oldt = oldw->tab;
		win_label_shape_and_render(oldw);
		win_update_net(oldw);
	}

	if (oldaltw && (oldaltw != w && oldaltw != oldw))
		win_label_shape_and_render(oldaltw);

	if (!w) {
		user_update_input_focus(u);
		return;
	}

	TAILQ_REMOVE(&latest_wins, w, older);
	TAILQ_INSERT_TAIL(&latest_wins, w, older);
	++w->focused;
	w->urgent = false;
	win_label_shape_and_render(w);
	win_update_net(w);

	if (w->tab->zoomed_win)
		w->tab->zoomed_win = w;

	while (TAILQ_FIRST(&sessions) != w->tab->session) {
		struct session *s = TAILQ_FIRST(&sessions);
		TAILQ_REMOVE(&sessions, s, link);
		TAILQ_INSERT_TAIL(&sessions, s, link);
		wins_changed = true;
	}

	if (!w->tab->output) {
		struct session *s = w->tab->session;
		if (oldt && oldt->output) {
			if (oldt->session != s) {
				struct tab *t;
				TAILQ_FOREACH(t, &s->tabs, link)
					if (t->output == oldt->output)
						break;
				if (!t)
					t = TAILQ_FIRST(&s->tabs);
				oldt = t;
			}
		} else {
			oldt = TAILQ_FIRST(&s->tabs);
		}
		tab_swap(oldt, w->tab);
	}

	user_save_pointer(u);
	wins_changed = true;
	server_update_wins();
	user_restore_pointer(u);
	user_update_input_focus(u);
}

static void
user_del(struct user *u)
{
	struct proc *proc, *tmpproc;
	LIST_FOREACH_SAFE(proc, &procs, link, tmpproc)
		if (u == proc->user)
			proc_del(proc);

	user_focus_win(u, NULL);

	TAILQ_REMOVE(&users, u, link);
	free(u->name);
	free(u);
}

static void
user_jump_tabs(struct user *u, int dir)
{
	struct win *w = u->focused_win;
	if (!w)
		return;
	struct tab *curt = w->tab, *t;
	if (!curt->output)
		return;
	struct session *s = curt->session;

	struct tab *not;
	TAILQ_FOREACH(not, &s->tabs, link)
		if (!not->output)
			break;
	if (!not)
		return;

	if (0 <= dir) {
		t = not;
		TAILQ_REMOVE(&s->tabs, not, link);
		TAILQ_INSERT_BEFORE(curt, not, link);
		TAILQ_REMOVE(&s->tabs, curt, link);
		TAILQ_INSERT_TAIL(&s->tabs, curt, link);
	} else {
		t = TAILQ_LAST(&s->tabs, session_tabs);
		TAILQ_REMOVE(&s->tabs, t, link);
		TAILQ_INSERT_BEFORE(curt, t, link);
		if (not != t) {
			TAILQ_REMOVE(&s->tabs, curt, link);
			TAILQ_INSERT_BEFORE(not, curt, link);
		}
	}
	session_update_tabs(s);

	user_focus_win(u, tab_get_latest_win(t));
}

static void
win_forget_pointer(struct win *w)
{
	w->pointer_x = 0;
	w->pointer_y = 0;
}

static void
user_jump_sessions(struct user *u, int dir)
{
	struct session *s;
	if (0 <= dir) {
		s = TAILQ_FIRST(&sessions);
		if (!s)
			return;
		s = TAILQ_NEXT(s, link);
	} else {
		s = TAILQ_LAST(&sessions, sessions);
	}
	if (!s)
		return;

	user_focus_win(u, session_get_latest_win(s));
}

static void
device_del_all(void)
{
	struct device *dev;
	ARRAY_FOREACH(dev, devices)
		xkb_keymap_unref(dev->keymap);
	memset(devices, 0, sizeof devices);
}

static void
user_update_all(void)
{
	XGET(reply, xcb_input_xi_query_device, conn, XCB_INPUT_DEVICE_ALL);
	if (!reply)
		return;

	struct user *u, *tmpu;
	struct user *newu = NULL;

	device_del_all();
	TAILQ_FOREACH(u, &users, link)
		user_disable(u);

	for (xcb_input_xi_device_info_iterator_t iter = xcb_input_xi_query_device_infos_iterator(reply);
	     0 < iter.rem;
	     xcb_input_xi_device_info_next(&iter))
	{
		xcb_input_xi_device_info_t const *xdev = iter.data;

		/* Master devices always come in pair. */
		if (XCB_INPUT_DEVICE_TYPE_MASTER_POINTER == xdev->type) {
			TAILQ_FOREACH(u, &users, link)
				if (xdev->deviceid == u->master_pointer)
					break;

			if (u) {
				TAILQ_REMOVE(&users, u, link);
				TAILQ_INSERT_TAIL(&users, u, link);
			} else {
				u = user_new(xdev);
			}

			user_enable(u);

			if (!newu)
				newu = u;
		}

		assert(xdev->deviceid < ARRAY_SIZE(devices));
		struct device *dev = &devices[xdev->deviceid];

		TAILQ_FOREACH(u, &users, link) {
			if (xdev->attachment == u->master_pointer ||
			    xdev->attachment == u->master_keyboard)
			{
				dev->user = u;
				break;
			}
		}
	}

	free(reply);

	TAILQ_FOREACH_SAFE(u, &users, link, tmpu) {
		if (u == newu)
			break;
		user_del(u);
	}

	TAILQ_FOREACH(u, &users, link)
		user_update_input_focus(u);
}

static struct user *
user_find_by_device(xcb_input_device_id_t deviceid)
{
	struct device *dev = device_find_by_id(deviceid);
	return dev ? dev->user : NULL;
}

static struct win *
user_find_win(struct user const *u, char label)
{
	struct win *w = u->focused_win;
	if (w && w->label == label) {
		for (;;) {
			if (TAILQ_NEXT(w, link)) {
				w = TAILQ_NEXT(w, link);
			} else if (TAILQ_NEXT(w->tab, link)) {
				w = TAILQ_FIRST(&TAILQ_NEXT(w->tab, link)->wins);
			} else {
				w = TAILQ_FIRST(&TAILQ_FIRST(&w->tab->session->tabs)->wins);
			}

			if (label != w->label)
				continue;

			return w;
		}
	}

	TAILQ_FOREACH_REVERSE(w, &latest_wins, latest_wins, older) {
		if (label != w->label)
			continue;

		return w;
	}
	return NULL;
}

static void
win_update_name(struct win const *w, char const *name)
{
	if (!strcmp(w->name, name))
		return;
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			w->window, ATOM(_HEAWM_NAME),
			ATOM(UTF8_STRING), 8,
			strlen(name), name);
}

static void
win_prop_update_title(struct win *w, void *data, int sz)
{
	/* Ignore zero-sized reply meaning property is not supported (or
	 * something like this), e.g. Firefox. */
	if (!sz)
		return;

	free(w->title);
	XASSERT(w->title = strndup(data, sz));
}

static void
win_prop_update_heawm_name(struct win *w, void *data, int sz)
{
	if (!sz)
		return;

	free(w->name);
	XASSERT(w->name = strndup(data, sz));
}

static void
win_prop_update_heawm_label(struct win *w, void *data, int sz)
{
	if (!sz)
		return;

	w->label = *(char *)data;
	win_label_shape_and_render(w);
}

static void
win_prop_update_role(struct win *w, void *data, int sz)
{
	free(w->role);
	w->role = sz ? strndup(data, sz) : NULL;
}

static void
win_prop_update_leader(struct win *w, void *data, int sz)
{
	w->leader = sizeof(xcb_window_t) == sz
		? *(xcb_window_t *)data
		: XCB_WINDOW_NONE;
}

static void
win_prop_update_transient_for(struct win *w, void *data, int sz)
{
	w->transient_for = sizeof(xcb_window_t) == sz
		? *(xcb_window_t *)data
		: XCB_WINDOW_NONE;
}

static void
win_prop_update_type(struct win *w, void *data, int sz)
{
	w->type = sizeof(xcb_window_t) == sz
		? *(xcb_window_t *)data
		: XCB_WINDOW_NONE;
}

static void
win_prop_update_pid(struct win *w, void *data, int sz)
{
	w->pid = sizeof(uint32_t) == sz
		? *(uint32_t *)data
		: 0;
}

static void
win_prop_update_class(struct win *w, void *data, int sz)
{
	free(w->class_instance);
	XASSERT(w->class_class = w->class_instance = strdup(""));

	if (sz < 2)
		return;

	if (((char *)data)[sz - 1])
		return;

	char const *sep = memchr(data, '\0', sz - 1);
	if (!sep)
		return;

	XASSERT(w->class_instance = malloc(sz));
	memcpy(w->class_instance, data, sz);
	w->class_class = w->class_instance + (sep - (char *)data) + 1;

	if (!*w->name)
		win_update_name(w, w->class_class);
}

static void
win_prop_update_state(struct win *w, void *data, int sz)
{
	w->fullscreen = false;

	while ((int)sizeof(xcb_atom_t) <= sz) {
		sz -= sizeof(xcb_atom_t);
		xcb_atom_t atom = *(xcb_atom_t const *)((char *)data + sz);
		w->fullscreen |= ATOM(_NET_WM_STATE_FULLSCREEN) == atom;
	}
}

static void
win_prop_update_hints(struct win *w, void *data, int sz)
{
	xcb_icccm_wm_hints_t const *hints = data;
	if (sz < (int)sizeof *hints)
		return;

	bool urgent = XCB_ICCCM_WM_HINT_X_URGENCY & hints->flags;
	if (w->urgent < urgent) {
		w->urgent = urgent;
		win_label_shape_and_render(w);
	}
}

#define PROPERTIES \
	xmacro(XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, win_prop_update_hints) \
	xmacro(XCB_ATOM_WM_NAME, XCB_ATOM_STRING, win_prop_update_title) \
	xmacro(ATOM(_NET_WM_NAME), ATOM(UTF8_STRING), win_prop_update_title) \
	xmacro(ATOM(_HEAWM_NAME), ATOM(UTF8_STRING), win_prop_update_heawm_name) \
	xmacro(ATOM(_HEAWM_LABEL), ATOM(UTF8_STRING), win_prop_update_heawm_label) \
	xmacro(XCB_ATOM_WM_CLASS, XCB_ATOM_STRING, win_prop_update_class) \
	xmacro(ATOM(WM_WINDOW_ROLE), XCB_ATOM_STRING, win_prop_update_role) \
	xmacro(ATOM(WM_CLIENT_LEADER), XCB_ATOM_WINDOW, win_prop_update_leader) \
	xmacro(XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW, win_prop_update_transient_for) \
	xmacro(ATOM(_NET_WM_STATE), XCB_ATOM_ATOM, win_prop_update_state) \
	xmacro(ATOM(_NET_WM_WINDOW_TYPE), XCB_ATOM_ATOM, win_prop_update_type) \
	xmacro(ATOM(_NET_WM_PID), XCB_ATOM_CARDINAL, win_prop_update_pid) \

/** List of window properties to be watched. */
static struct prop {
	xcb_atom_t atom;
	xcb_atom_t type;
	void (*handle)(struct win *, void *, int);
} properties[] = {
#define xmacro(...) { 0 },
	PROPERTIES
#undef xmacro
};

static void
win_swap(struct win *x, struct win *y)
{
	if (x == y)
		return;
	TAILQ_SWAP(x, &x->tab->wins, y, &y->tab->wins, win, link);
	bool xzoomed = x->tab->zoomed_win == x;
	bool yzoomed = y->tab->zoomed_win == y;
	if (xzoomed)
		x->tab->zoomed_win = y;
	if (yzoomed)
		y->tab->zoomed_win = x;
	SWAP(x->tab, y->tab, struct tab *);
	wins_changed = true;
	x->tab->mapped =
	y->tab->mapped =
	x->tab->session->mapped =
	y->tab->session->mapped = true;
}

static bool
win_move_top(struct win *w)
{
	struct win *topw = TAILQ_FIRST(&w->tab->wins);
	if (topw == w)
		return false;
	win_swap(topw, w);
	return true;
}

static void
user_break_win(struct user *u)
{
	struct win *w = u->focused_win;
	if (!w)
		return;

	/* Tab already contains a single element. */
	if (!TAILQ_NEXT(TAILQ_FIRST(&w->tab->wins), link))
		return;

	struct tab *oldt = w->tab;
	struct session *s = oldt->session;
	struct tab *newt = tab_new(s);

	if (oldt->zoomed_win == w)
		oldt->zoomed_win = NULL;
	TAILQ_REMOVE(&oldt->wins, w, link);
	TAILQ_INSERT_HEAD(&newt->wins, w, link);
	w->tab = newt;
	u->alt_tab = oldt;

	TAILQ_REMOVE(&s->tabs, newt, link);
	TAILQ_INSERT_BEFORE(oldt, newt, link);
	TAILQ_REMOVE(&s->tabs, oldt, link);
	TAILQ_INSERT_TAIL(&s->tabs, oldt, link);
	session_update_tabs(s);
}

static void
win_close(struct win *w)
{
	XDO(xcb_send_event, conn, false, w->window,
			XCB_EVENT_MASK_NO_EVENT/* Client messages cannot be masked. */,
			XCB_SEND_EVENT_EVENT(xcb_client_message_event_t, {
				.response_type = XCB_CLIENT_MESSAGE,
				.format = 32,
				.window = w->window,
				.type = ATOM(WM_PROTOCOLS),
				.data = {
					.data32 = {
						ATOM(WM_DELETE_WINDOW),
					},
				},
			}));
}

static void
win_kill(struct win *w)
{
	XDO(xcb_kill_client, conn, w->window);
}

static void
user_focus_urgent_win(struct user *u)
{
	struct win *found = NULL;
	int found_pri = -1;

	struct win *w;
	TAILQ_FOREACH_REVERSE(w, &latest_wins, latest_wins, older) {
		if (!w->urgent)
			continue;

		int pri = (
			((u->focused_win && u->focused_win->tab == w->tab) << 2) |
			(!!w->tab->output << 1) |
			(1 << 0)
		) << (w->tab->session == TAILQ_FIRST(&sessions) ? 3 : 0);
		if (found_pri < pri)
			found = w;
	}

	if (found)
		user_focus_win(u, (struct win *)found);
}

static void
user_dump_tree(struct user *u)
{
	char tmppath[] = "/tmp/"WM_NAME".layoutXXXXXX";
	FILE *f = fdopen(mkstemp(tmppath), "w");
	if (!f)
		return;
	unsigned sel_lnum = 0;
	unsigned lnum = 0;

	struct session *s;
	TAILQ_FOREACH(s, &sessions, link) {
		struct tab *t;
		TAILQ_FOREACH(t, &s->tabs, link) {
			struct win *w;
			TAILQ_FOREACH(w, &t->wins, link) {
				++lnum;
				fprintf(f, "%x\t%c\t%s\t%s,%s\t%s\n",
						w->window,
						w->label,
						w->name,
						w->class_instance,
						w->class_class,
						w->title
						);

				if (u->focused_win == w)
					sel_lnum = lnum;
			}
			++lnum;
			fputc('\n', f);
		}
		++lnum;
		fputc('\n', f);
	}


	if (fclose(f) < 0) {
		unlink(tmppath);
		return;
	}

	char cmd[128];
	sprintf(cmd, "%d", sel_lnum);
	struct proc *proc = proc_new(u);
	XASSERT(proc->data = strdup(tmppath));
	proc_execvp(proc, "sh", "sh", "-c", "${TERMINAL?} -e sh -c '${EDITOR?} +$2 -- \"$1\" || unlink \"$1\"' sh \"$@\"", "sh", tmppath, cmd, NULL);
}

static void
win_set_user_geom(struct win *w, xcb_rectangle_t const *rect)
{
	if (memcmp(&w->user_geom, rect, sizeof w->geom)) {
		w->user_geom = *rect;
		wins_changed = true;
	}
}

static void
user_jump_wins(struct user *u, int dir)
{
	struct win *w = u->focused_win;
	if (!w)
		return;
	struct tab *t = w->tab;
	if (0 <= dir) {
		w = TAILQ_NEXT(w, link);
		if (!w)
			w = TAILQ_FIRST(&t->wins);
	} else {
		w = TAILQ_PREV(w, tab_wins, link);
		if (!w)
			w = TAILQ_LAST(&t->wins, tab_wins);
	}
	user_focus_win(u, w);
}

static void
user_swap_win(struct user *u, struct win *w)
{
	if (!u->focused_win || !w)
		return;
	if (u->focused_win->tab == w->tab)
		win_swap(u->focused_win, w);
	else
		tab_swap(u->focused_win->tab, w->tab);
	user_focus_win(u, w);
}

static void
output_update_all(void)
{
	for (struct output *o; (o = TAILQ_FIRST(&outputs));)
		output_del(o);

	if (!randr_first_event) {
		(void)output_new_from_screen();
		return;
	}

	/* Force RROutputChange under Xephyr -resizeable, otherwise monitor
	 * dimensions are not updated.
	 *
	 * From randrproto.txt:
	 * This request explicitly asks the server to ensure that the
	 * configuration data is up-to-date wrt the hardware. If that requires
	 * polling, this is when such polling would take place.
	 */
	XDISCARD(xcb_randr_get_screen_resources, conn, screen->root);

	XGET(monitors, xcb_randr_get_monitors, conn, screen->root, true);
	if (!monitors) {
		(void)output_new_from_screen();
		return;
	}

	for (xcb_randr_monitor_info_iterator_t iter = xcb_randr_get_monitors_monitors_iterator(monitors);
	     0 < iter.rem;
	     xcb_randr_monitor_info_next(&iter))
		(void)output_new_from_randr_monitor(iter.data);

	free(monitors);
}

static void
screen_update_client_list(void)
{
	xcb_prop_mode_t mode = XCB_PROP_MODE_REPLACE;
	uint32_t i = 0;
	xcb_window_t list[128];

	for (struct win const *w = TAILQ_FIRST(&latest_wins);; w = TAILQ_NEXT(w, older)) {
		if (!w || ARRAY_SIZE(list) == i) {
			XDO(xcb_change_property, conn, mode,
					screen->root, ATOM(_NET_CLIENT_LIST),
					XCB_ATOM_WINDOW, 32,
					i, list);
			i = 0;
			mode = XCB_PROP_MODE_APPEND;
		}
		if (!w)
			break;
		list[i++] = w->window;
	}
}

static void
screen_append_client_list(struct win const *w)
{
	XDO(xcb_change_property, conn, XCB_PROP_MODE_APPEND,
			screen->root, ATOM(_NET_CLIENT_LIST),
			XCB_ATOM_WINDOW, 32,
			1, &w->window);
}

static xcb_get_property_cookie_t
win_prop_get(struct win const *w, struct prop const *prop)
{
	XGET_COOKIE(cookie, xcb_get_property, conn, 0, w->window,
			prop->atom, prop->type,
			0, (256 << 10) / sizeof(uint32_t));
	return cookie;
}

static void
win_prop_handle_reply(struct win *w, struct prop const *prop,
		xcb_get_property_cookie_t cookie)
{
	XGET_REPLY_LOCAL(reply, xcb_get_property, conn, cookie);
	if (!reply)
		return;

	int sz = xcb_get_property_value_length(reply);
	uint8_t expected_format = (
		XCB_ATOM_STRING == prop->type ? 8 :
		ATOM(UTF8_STRING) == prop->type ? 8 :
		32
	);
	if (0 < sz &&
	    (reply->type != prop->type ||
	     reply->format != expected_format))
	{
		fprintf(stderr, "Property dropped because bad type/format\n");
	} else {
		void *data = xcb_get_property_value(reply);
		prop->handle(w, data, sz);
	}

	free(reply);
}

static xcb_visualtype_t *
screen_get_visualtype(xcb_screen_t const *screen)
{
	for (xcb_depth_iterator_t depth_iter = xcb_screen_allowed_depths_iterator(screen);
	     0 < depth_iter.rem;
	     xcb_depth_next(&depth_iter))
		for (xcb_visualtype_iterator_t visual_iter = xcb_depth_visuals_iterator(depth_iter.data);
		     0 < visual_iter.rem;
		     xcb_visualtype_next(&visual_iter))
			if (screen->root_visual == visual_iter.data->visual_id)
				return visual_iter.data;

	return NULL;
}

static char const *
xcb_connection_strerror(int error)
{
	switch (error) {
	case XCB_CONN_ERROR:                   return "Stream error";
	case XCB_CONN_CLOSED_EXT_NOTSUPPORTED: return "Extension not supported";
	case XCB_CONN_CLOSED_MEM_INSUFFICIENT: return "Cannot allocate memory";
	case XCB_CONN_CLOSED_REQ_LEN_EXCEED:   return "Maximum request length exceeded";
	case XCB_CONN_CLOSED_PARSE_ERR:        return "Malformed display string";
	case XCB_CONN_CLOSED_INVALID_SCREEN:   return "Invalid screen";
	default:                               return "Unknown error";
	}
}

static void
win_del(struct win *w)
{
	TAILQ_REMOVE(&w->tab->wins, w, link);
	if (w->tab->zoomed_win == w)
		w->tab->zoomed_win = tab_get_latest_win(w->tab);
	wins_changed = true;

	TAILQ_REMOVE(&latest_wins, w, older);

	screen_update_client_list();

	win_hash_del(w->window);
	win_hash_del(w->frame);
	win_hash_del(w->win_label.window);

	struct user *u;
	TAILQ_FOREACH(u, &users, link) {
		if (u->focused_win == w) {
			struct win *focused_win = NULL;
			if (!focused_win)
				focused_win = tab_get_latest_win(w->tab);
			if (!focused_win)
				focused_win = session_get_latest_win(w->tab->session);
			if (!focused_win)
				focused_win = TAILQ_LAST(&latest_wins, latest_wins);
			user_focus_win(u, focused_win);
		}

		if (u->alt_win == w)
			u->alt_win = NULL;
	}

	win_label_del(&w->win_label);

	XDO(xcb_shape_select_input, conn, w->window, false);
	XDO(xcb_change_window_attributes, conn, w->window,
			XCB_CW_EVENT_MASK,
			(uint32_t const[]){
				XCB_NONE
			});
	xcb_icccm_set_wm_state(w->window, XCB_ICCCM_WM_STATE_WITHDRAWN);
	XDO(xcb_reparent_window, conn, w->window, screen->root, 0, 0);
	XDO(xcb_destroy_window, conn, w->frame);
	XDO(xcb_change_save_set, conn, XCB_SET_MODE_DELETE, w->window);

	if (TAILQ_EMPTY(&w->tab->wins))
		tab_del(w->tab);

	free(w->name);
	free(w->title);
	free(w->class_instance);
	free(w->role);
	free(w);
}

static struct win *
win_new(xcb_window_t window)
{
	bool poisoned = false;

	struct win *w;
	XASSERT(w = calloc(1, sizeof *w));

	w->window = window;
	w->frame = xcb_generate_id(conn);
	w->urgent = true;
	struct win_label *wl = &w->win_label;
	win_label_new(wl);

	XASSERT(w->name = strdup(""));
	XASSERT(w->title = strdup(""));
	XASSERT(w->class_class = w->class_instance = strdup(""));

	TAILQ_INSERT_HEAD(&latest_wins, w, older);

	screen_append_client_list(w);

	win_hash_set(w->window, w);
	win_hash_set(w->frame, w);
	win_hash_set(wl->window, w);

	int font_size = screen_pt2px(label_font_size_pt);
	wl->geom.width = font_size * sqrt(M_PHI) + 2 * 1 /* px at both sides */;
	wl->geom.height = wl->geom.width;
	wl->geom.x = 1 /* Initial frame width. */ - wl->geom.width - 10;
	wl->geom.y = font_size;

	struct xdo_cookie const void_cookies[] = {
		XDO_COOKIE(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
				w->frame,
				screen->root,
				-1, -1, 1, 1, /* Rect. */
				0, /* Border. */
				XCB_WINDOW_CLASS_INPUT_OUTPUT,
				XCB_COPY_FROM_PARENT,
				XCB_CW_OVERRIDE_REDIRECT |
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					true,
					XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
				}),

		XDO_COOKIE(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				w->frame, XCB_ATOM_WM_CLASS,
				XCB_ATOM_STRING, 8,
				sizeof WIN_FRAME_INSTANCE "\0" WM_NAME,
				WIN_FRAME_INSTANCE "\0" WM_NAME),

		XDO_COOKIE(xcb_change_save_set, conn, XCB_SET_MODE_INSERT, window),

		XDO_COOKIE(xcb_reparent_window, conn, window, w->frame, 0, 0),

		XDO_COOKIE(xcb_change_window_attributes, conn, window,
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					XCB_EVENT_MASK_PROPERTY_CHANGE |
					XCB_EVENT_MASK_STRUCTURE_NOTIFY |
					XCB_EVENT_MASK_FOCUS_CHANGE |
					XCB_EVENT_MASK_ENTER_WINDOW,
				}),

		XDO_COOKIE(xcb_input_xi_select_events, conn, window, 1,
				XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
						XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
						XCB_INPUT_XI_EVENT_MASK_ENTER)),

		XDO_COOKIE(xcb_shape_select_input, conn, window, true),

		XDO_COOKIE(xcb_map_window, conn, window),

		/* Label. */
		XDO_COOKIE(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
				wl->window,
				w->frame,
				wl->geom.x, wl->geom.y,
				wl->geom.width, wl->geom.height,
				0, /* Border. */
				XCB_WINDOW_CLASS_INPUT_OUTPUT,
				XCB_COPY_FROM_PARENT,
				XCB_CW_BIT_GRAVITY |
				XCB_CW_WIN_GRAVITY |
				XCB_CW_OVERRIDE_REDIRECT |
				XCB_CW_SAVE_UNDER |
				XCB_CW_EVENT_MASK,
				(uint32_t const[]){
					XCB_GRAVITY_STATIC,
					XCB_GRAVITY_NORTH_EAST,
					true,
					true,
					XCB_EVENT_MASK_EXPOSURE,
				}),

		XDO_COOKIE(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				wl->window, XCB_ATOM_WM_CLASS,
				XCB_ATOM_STRING, 8,
				sizeof LABEL_INSTANCE "\0" WM_NAME,
				LABEL_INSTANCE "\0" WM_NAME),

		XDO_COOKIE(xcb_create_pixmap, conn,
				1, /* Mask is on or off thus a single bit. */
				wl->shape,
				wl->window,
				wl->geom.width, wl->geom.height),

		/* We need to provide a valid pixmap so we re-use the bounding
		 * mask with a large enough offset, making effective input
		 * region empty. */
		XDO_COOKIE(xcb_shape_mask, conn,
				XCB_SHAPE_SO_SET, XCB_SHAPE_SK_INPUT,
				wl->window,
				INT16_MAX, INT16_MAX,
				wl->shape),
	};

	XGET_COOKIE(geom_cookie, xcb_get_geometry, conn, window);
	XGET_COOKIE(shape_cookie, xcb_shape_query_extents, conn, window);

	xcb_get_property_cookie_t prop_cookies[ARRAY_SIZE(properties)];
	ARRAY_IFOREACH(i, properties)
		prop_cookies[i] = win_prop_get(w, &properties[i]);

	/* Process replies. */
	ARRAY_IFOREACH(i, void_cookies)
		if (xdo_check(void_cookies[i]))
			poisoned = true;

	XGET_REPLY(geom_reply, xcb_get_geometry, conn, geom_cookie);
	if (geom_reply) {
		w->user_geom = (xcb_rectangle_t){
			.x = geom_reply->x,
			.y = geom_reply->y,
			.width = geom_reply->width,
			.height = geom_reply->height,
		};
		free(geom_reply);
	}

	XGET_REPLY(shape_reply, xcb_shape_query_extents, conn, shape_cookie);
	if (shape_reply) {
		w->shaped = shape_reply->bounding_shaped;
		if (w->shaped) {
			win_update_shape(w, XCB_SHAPE_SK_BOUNDING);
			win_update_shape(w, XCB_SHAPE_SK_INPUT);
		}
		free(shape_reply);
	}

	ARRAY_IFOREACH(i, properties)
		win_prop_handle_reply(w, &properties[i], prop_cookies[i]);

	wl->shape_surface = cairo_xcb_surface_create_for_bitmap(conn,
			screen, wl->shape,
			wl->geom.width, wl->geom.height);
	wl->shape_cr = cairo_create(wl->shape_surface);
	wl->surface = cairo_xcb_surface_create(conn,
			wl->window, visual_type,
			wl->geom.width, wl->geom.height);
	wl->cr = cairo_create(wl->surface);

	if (ATOM(_NET_WM_WINDOW_TYPE_SPLASH) == w->type)
		w->floating = true;

	struct win *parent = win_hash_get(w->transient_for);
	if (!w->tab && parent) {
		TAILQ_INSERT_AFTER(&parent->tab->wins, parent, w, link);
		w->tab = parent->tab;
		w->label = parent->label;
		w->floating = true;
#define xmacro(x, width) \
		if (parent->geom.width / 2 < w->user_geom.width) \
			w->user_geom.width = parent->geom.width; \
		w->user_geom.x = parent->geom.x + (parent->geom.width - w->user_geom.width) / 2;
		xmacro(x, width);
		xmacro(y, height);
#undef xmacro
		wins_changed = true;

		struct user *u;
		TAILQ_FOREACH(u, &users, link)
			if (parent == u->focused_win)
				user_focus_win(u, w);
	}

	if (!w->tab) {
		struct proc *proc = proc_find(w->pid);
		if (proc && proc->user->focused_win) {
			struct win *afterw = proc->user->focused_win;
			TAILQ_INSERT_AFTER(&afterw->tab->wins, afterw, w, link);
			w->tab = afterw->tab;
			if (proc->data)
				w->floating = true;
			wins_changed = true;
			user_focus_win(proc->user, w);
			if (proc->data && w->tab->output) {
				w->user_geom = w->tab->output->geom;
				wins_changed = true;
			}
		}
	}

	if (!w->tab && XCB_WINDOW_NONE != w->leader) {
		struct win *leaderw = NULL;
		TAILQ_FOREACH_REVERSE(leaderw, &latest_wins, latest_wins, older)
			if (w->leader == leaderw->leader && w != leaderw)
				break;
		if (leaderw) {
			TAILQ_INSERT_AFTER(&leaderw->tab->wins, leaderw, w, link);
			w->tab = leaderw->tab;
			wins_changed = true;

			struct user *u;
			TAILQ_FOREACH(u, &users, link)
				if (u->focused_win && w->tab == u->focused_win->tab)
					user_focus_win(u, w);
		}
	}

	if (!w->tab && TAILQ_NEXT(w, older)) {
		struct win *after = TAILQ_LAST(&latest_wins, latest_wins);
		TAILQ_INSERT_AFTER(&after->tab->wins, after, w, link);
		w->tab = after->tab;
		wins_changed = true;

		struct user *u;
		TAILQ_FOREACH(u, &users, link)
			if (u->focused_win && w->tab == u->focused_win->tab)
				user_focus_win(u, w);
	}

	if (!w->tab) {
		struct session *s = session_new();
		struct tab *t = tab_new(s);
		TAILQ_INSERT_TAIL(&t->wins, w, link);
		w->tab = t;
		wins_changed = true;

		struct user *u;
		TAILQ_FOREACH(u, &users, link)
			user_focus_win(u, w);
	}

	if (!w->label)
		win_set_auto_label(w);

	if (poisoned) {
		win_del(w);
		return NULL;
	}

	return w;
}

static void
win_new_from_children(xcb_window_t parent)
{
	XGET(reply, xcb_query_tree, conn, parent);
	if (!reply)
		return;

	xcb_window_t const *children = xcb_query_tree_children(reply);
	int n = xcb_query_tree_children_length(reply);

	xcb_get_window_attributes_cookie_t *cookies;
	XASSERT(cookies = malloc(n * sizeof *cookies));

	for (int i = 0; i < n; ++i) {
		XGET_COOKIE(cookie, xcb_get_window_attributes, conn, children[i]);
		cookies[i] = cookie;
	}

	for (int i = 0; i < n; ++i) {
		XGET_REPLY_LOCAL(reply, xcb_get_window_attributes, conn, cookies[i]);
		if (!reply)
			continue;

		if (!reply->override_redirect &&
		    XCB_MAP_STATE_VIEWABLE == reply->map_state)
			(void)win_new(children[i]);

		free(reply);
	}

	free(cookies);
	free(reply);
}

static void
load_tree(struct proc *proc)
{
	FILE *f = fopen(proc->data, "r");
	unlink(proc->data);
	if (!f)
		return;

	struct session *s;
	TAILQ_FOREACH(s, &sessions, link)
		s->mapped = false;

	struct session *curs = NULL;
	struct tab *curt = NULL;
	struct win *focusw = NULL;
	int blanks = 2;

	char *line = NULL;
	size_t linesz = 0;
	while (0 < getline(&line, &linesz, f)) {
		char *p = line;
		int window = strtol(p, &p, 16);
		if (!window) {
			++blanks;
			continue;
		}
		p += '\t' == *p;
		char label = *p;
		if ('\t' == label)
			label = '\0';
		else
			p += !!label;
		p += '\t' == *p;
		char *name = p;
		name[strcspn(name, "\n\t")] = '\0';

		struct win *w = win_hash_get(window);
		if (!w)
			continue;

		if (2 <= blanks) {
			curs = session_new();
			curs->mapped = true;
		}
		if (1 <= blanks) {
			curt = tab_new(curs);
			/* |= w->mapped. */
			curt->mapped = true;
			curt->master = w->tab->master;
			curt->mfact = w->tab->mfact;
			curt->cols = w->tab->cols;
			curt->monocle = w->tab->monocle;
		}
		blanks = 0;

		if (w->tab->zoomed_win == w)
			curt->zoomed_win = w;

		TAILQ_REMOVE(&w->tab->wins, w, link);
		if (TAILQ_EMPTY(&w->tab->wins))
			tab_del(w->tab);
		TAILQ_INSERT_TAIL(&curt->wins, w, link);
		w->tab = curt;

		win_update_label(w, label);
		win_update_name(w, name);

		if (' ' == *line)
			focusw = w;
	}
	free(line);
	fclose(f);

	TAILQ_FOREACH(s, &sessions, link) {
		if (s->mapped)
			continue;
		s->mapped = true;

		struct tab *t;
		TAILQ_FOREACH(t, &s->tabs, link) {
			struct win *w;
			TAILQ_FOREACH(w, &t->wins, link)
				win_close(w);
		}
	}

	wins_changed = true;
	if (focusw)
		user_focus_win(proc->user, focusw);
}

static void
screen_setup_net(void)
{
	xcb_window_t net_window = xcb_generate_id(conn);
	XDO(xcb_create_window, conn, XCB_COPY_FROM_PARENT,
			net_window,
			screen->root,
			0, 0, 1, 1, 0,
			XCB_WINDOW_CLASS_INPUT_ONLY,
			XCB_COPY_FROM_PARENT,
			0, NULL);

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			net_window, ATOM(_NET_WM_NAME),
			ATOM(UTF8_STRING), 8,
			strlen(WM_NAME), WM_NAME);

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			net_window, ATOM(_NET_WM_PID),
			XCB_ATOM_CARDINAL, 32,
			1, (uint32_t const[]){ getpid() });

	{
#define xmacro(name) +1
		enum { NUM_NET_ATOMS = NET_ATOMS };
#undef xmacro

		XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
				screen->root, ATOM(_NET_SUPPORTED),
				XCB_ATOM_ATOM, 32,
				NUM_NET_ATOMS, atoms);
	}

	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			net_window, ATOM(_NET_SUPPORTING_WM_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &net_window);
	XDO(xcb_change_property, conn, XCB_PROP_MODE_REPLACE,
			screen->root, ATOM(_NET_SUPPORTING_WM_CHECK),
			XCB_ATOM_WINDOW, 32,
			1, &net_window);
}

static void
screen_setup_outputs(void)
{
	if (randr_first_event)
		XDO(xcb_randr_select_input, conn, screen->root,
				XCB_RANDR_NOTIFY_MASK_SCREEN_CHANGE);
	output_update_all();
}

static void
screen_setup_users(void)
{
	XDO(xcb_input_xi_select_events, conn, screen->root, 1,
			XI_EVENT_MASK(XCB_INPUT_DEVICE_ALL,
					XCB_INPUT_XI_EVENT_MASK_HIERARCHY |
					XCB_INPUT_XI_EVENT_MASK_FOCUS_IN |
					XCB_INPUT_XI_EVENT_MASK_ENTER));
	user_update_all();
}

static void
screen_setup(void)
{
	xcb_cursor_context_t *ctx;
	XASSERT(0 <= xcb_cursor_context_new(conn, screen, &ctx));
	move_cursor = xcb_cursor_load_cursor(ctx, "move");
	default_cursor = xcb_cursor_load_cursor(ctx, "default");
	xcb_cursor_context_free(ctx);

	int error = XDO_CHECK(xcb_change_window_attributes, conn,
			screen->root,
			XCB_CW_EVENT_MASK |
			XCB_CW_CURSOR,
			(uint32_t const[]){
				XCB_EVENT_MASK_STRUCTURE_NOTIFY |
				XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT,
				default_cursor,
			});
	if (error) {
		if (XCB_ACCESS == error)
			fprintf(stderr, "Window manager is already running");
		exit(EXIT_FAILURE);
	}

	screen_setup_net();
	screen_setup_outputs();
	screen_setup_users();
	screen_update_client_list();
	win_new_from_children(screen->root);
}

static void
user_feed_command_key(struct user *u, xkb_keysym_t keysym,
		xcb_input_key_press_event_t const *event)
{
	if (XCB_INPUT_KEY_RELEASE == event->event_type)
		return;

	if (XKB_KEY_Escape == keysym) {
		user_set_mode(u, MODE_NORMAL);
		return;
	}

	struct win *w = u->focused_win;
	if (!w)
		return;
	struct tab *t = w->tab;

	if (XKB_KEY_0 <= keysym && keysym <= XKB_KEY_9) {
		u->num = keysym - XKB_KEY_0;
		return;
	}

	switch (keysym) {
	case XKB_KEY_h:
	case XKB_KEY_j:
	case XKB_KEY_k:
	case XKB_KEY_l:
		t->mfact = user_number(u, !t->master ? 6 : t->mfact);
		t->master = keysym - XKB_KEY_a + 'a';
		t->zoomed_win = NULL;
		win_move_top(w);
		break;

	case XKB_KEY_plus:
	case XKB_KEY_minus:
		t->mfact += (XKB_KEY_plus == keysym) == !TAILQ_PREV(w, tab_wins, link) ? 1 : -1;
		break;

	case XKB_KEY_s:
		t->mfact = user_number(u, 6 == t->mfact ? 5 : 6);
		break;

	case XKB_KEY_G:
		t->master = '\0';
		/* FALLTHROUGH */
	case XKB_KEY_g:
		t->monocle = false;
		t->cols = 0;
		break;

	case XKB_KEY_R:
		t->master = '\0';
		/* FALLTHROUGH */
	case XKB_KEY_r:
		t->monocle = false;
		t->cols = -user_number(u, 1);
		break;

	case XKB_KEY_C:
		t->master = '\0';
		/* FALLTHROUGH */
	case XKB_KEY_c:
		t->monocle = false;
		t->cols = user_number(u, 1);
		break;

	case XKB_KEY_M:
		t->master = '\0';
		/* FALLTHROUGH */
	case XKB_KEY_m:
		t->monocle ^= true;
		break;

	case XKB_KEY_q:
		win_close(w);
		break;

	case XKB_KEY_Q:
		win_kill(w);
		break;

	case XKB_KEY_i:
		user_set_mode(u, MODE_INSERT);
		return;

	case XKB_KEY_F:
	{
		struct win *w;
		TAILQ_FOREACH(w, &t->wins, link)
			w->floating = false;
	}
		break;

	case XKB_KEY_f:
		if (1 <= u->num && u->num <= 9) {
			w->floating = true;
			int x = (u->num - 1) % 3, y = 2 - (u->num - 1) / 3;
			xcb_rectangle_t rect = w->tab->output->geom;
			rect.x += x * rect.width / 3;
			rect.y += y * rect.height / 3;
			rect.width = (x + 1) * rect.width / 3 - rect.x;
			rect.height = (y + 1) * rect.height / 3 - rect.y;
			win_set_user_geom(w, &rect);
		} else {
			w->floating ^= true;
		}
		break;

	default:
		return;
	}

	if (9 < t->mfact)
		t->mfact = 9;
	else if (t->mfact < 1)
		t->mfact = 1;
	wins_changed = true;
	user_set_mode(u, MODE_NORMAL);
}

static void
user_feed_normal_key(struct user *u, xkb_keysym_t keysym,
		xcb_input_key_press_event_t const *event)
{
	if (XCB_INPUT_KEY_RELEASE == event->event_type)
		return;

	if (XKB_KEY_a <= keysym && keysym <= XKB_KEY_z) {
		char label = keysym - XKB_KEY_a + 'a';
		struct win *w = user_find_win(u, label);
		if (w)
			user_focus_win(u, w);
	} else if (XKB_KEY_A <= keysym && keysym <= XKB_KEY_Z) {
		char label = keysym - XKB_KEY_A + 'a';
		struct win *w = user_find_win(u, label);
		if (w)
			user_swap_win(u, w);
	} else switch (keysym) {
	case XKB_KEY_Tab:
		if (u->alt_win)
			user_focus_win(u, u->alt_win);
		break;

	case XKB_KEY_period:
		if (u->focused_win) {
			struct tab *t = u->focused_win->tab;
			t->zoomed_win = t->zoomed_win ? NULL : u->focused_win;
			wins_changed = true;
		}
		break;

	case XKB_KEY_space:
		if (u->focused_win && !win_move_top(u->focused_win) && u->alt_win)
			user_swap_win(u, u->alt_win);
		break;

	case XKB_KEY_comma:
	case XKB_KEY_slash:
		user_dump_tree(u);
		break;

	case XKB_KEY_semicolon:
		if (u->focused_win)
			user_set_mode(u, MODE_LABEL);
		break;

	case XKB_KEY_Return:
	{
		struct proc *proc = proc_new(u);
		proc_execvp(proc, "sh", "sh", "-c", "${TERMINAL?}", NULL);
	}
		break;

	case XKB_KEY_parenleft:
		user_jump_sessions(u, -1);
		break;

	case XKB_KEY_parenright:
		user_jump_sessions(u, 1);
		break;

	case XKB_KEY_braceleft:
		user_jump_tabs(u, -1);
		break;

	case XKB_KEY_braceright:
		user_jump_tabs(u, 1);
		break;

	case XKB_KEY_bracketleft:
		user_jump_wins(u, -1);
		break;

	case XKB_KEY_bracketright:
		user_jump_wins(u, 1);
		break;

	case XKB_KEY_Escape:
		user_set_mode(u, MODE_COMMAND);
		u->num = 0;
		break;

	case XKB_KEY_plus:
	case XKB_KEY_minus:
		user_feed_command_key(u, keysym, event);
		break;

	case XKB_KEY_asterisk:
		if (u->focused_win) {
			win_forget_pointer(u->focused_win);
			user_restore_pointer(u);
		}
		break;

	case XKB_KEY_numbersign:
		if (u->alt_tab)
			user_focus_win(u, tab_get_latest_win(u->alt_tab));
		break;

	case XKB_KEY_ampersand:
		user_break_win(u);
		break;

	case XKB_KEY_exclam:
		user_focus_urgent_win(u);
		break;
	}
}

static void
user_feed_label_key(struct user *u, xkb_keysym_t keysym,
		xcb_input_key_press_event_t const *event)
{
	if (XCB_INPUT_KEY_RELEASE == event->event_type)
		return;

	if (XKB_KEY_a <= keysym && keysym <= XKB_KEY_z) {
		win_update_label(u->focused_win, keysym - XKB_KEY_a + 'a');
	} else switch (keysym) {
	case XKB_KEY_semicolon:
		win_set_auto_label(u->focused_win);
		break;
	}

	user_set_mode(u, MODE_NORMAL);
}

static void
user_feed_key(struct user *u, xkb_keysym_t keysym, xcb_input_key_press_event_t const *event)
{
	switch (u->mode) {
	case MODE_NORMAL:
		if (mod_super == (~XCB_MOD_MASK_SHIFT & event->mods.base))
			user_feed_normal_key(u, keysym, event);
		else if ((mod_super | XCB_MOD_MASK_CONTROL) == (~XCB_MOD_MASK_SHIFT & event->mods.base))
			user_feed_command_key(u, keysym, event);
		break;

	case MODE_COMMAND:
		user_feed_command_key(u, keysym, event);
		break;

	case MODE_MOUSE:
		break;

	case MODE_INSERT:
		if (XCB_INPUT_KEY_PRESS == event->event_type &&
		    mod_super == event->mods.base &&
		    XKB_KEY_Escape == keysym)
			user_set_mode(u, MODE_NORMAL);
		break;

	case MODE_LABEL:
		user_feed_label_key(u, keysym, event);
		break;
	}
}

static void
handle_child(int sig)
{
	(void)sig;

	int status;
	pid_t pid;
	while (0 < (pid = waitpid(-1, &status, WNOHANG))) {
		struct proc *proc = proc_find(pid);
		if (!proc)
			continue;

		if (proc->data)
			load_tree(proc);

		proc_del(proc);
	}
}

static void
handle_error(xcb_generic_error_t const *event)
{
	print_error(event);
}

static void
handle_expose(xcb_expose_event_t const *event)
{
	/* Act only on the last expose event in the row. */
	if (0 < event->count)
		return;

	struct win *w = win_hash_get(event->window);
	if (w)
		win_label_render(w, false);
}

static void
handle_unmap_notify(xcb_unmap_notify_event_t const *event)
{
	/* Ignore events originating from xcb_reparent_window(). */
	if (event->event != event->window)
		return;

	struct win *w = win_hash_get(event->window);
	if (w)
		win_del(w);
}

static void
handle_map_request(xcb_map_request_event_t const *event)
{
	struct win *w = win_hash_get(event->window);
	if (!w)
		win_new(event->window);
}

static void
handle_configure_notify(xcb_configure_notify_event_t const *event)
{
	if (screen->root != event->window)
		return;

	/* Handled by RandR if available. */
	if (randr_first_event)
		return;

	screen->width_in_pixels = event->width;
	screen->height_in_pixels = event->height;

	output_update_all();
}

static void
handle_configure_request(xcb_configure_request_event_t const *event)
{
	struct win *w = win_hash_get(event->window);
	if (!w)
		return;

	/* Do not annoy users. */
	if (!w->floating) {
#define xmacro(X, x) \
	if (XCB_CONFIG_WINDOW_##X & event->value_mask) \
		w->user_geom.x = event->x;
		xmacro(X, x)
		xmacro(Y, y)
		xmacro(WIDTH, width)
		xmacro(HEIGHT, height)
#undef xmacro
	}

	win_send_configure_notify(w);
}

static void
handle_property_notify(xcb_property_notify_event_t const *event)
{
#if 0
	print_atom("prop", event->atom);
#endif

	struct win *w = win_hash_get(event->window);
	if (!w)
		return;

	struct prop *prop;
	ARRAY_FOREACH(prop, properties) {
		if (event->atom != prop->atom)
			continue;

		win_prop_handle_reply(w, prop, win_prop_get(w, prop));
		return;
	}
}

static void
handle_client_message(xcb_client_message_event_t const *event)
{
	struct win *w = win_hash_get(event->window);
#if 0
	print_atom("client message", event->type);
#endif
	if (!w)
		return;

	if (ATOM(_NET_CLOSE_WINDOW) == event->type) {
		win_close(w);
	} else if (ATOM(_NET_ACTIVE_WINDOW) == event->type) {
		if (XCB_EWMH_CLIENT_SOURCE_TYPE_NORMAL == event->data.data32[0])
			return;

		if (!TAILQ_EMPTY(&users))
			user_focus_win(TAILQ_FIRST(&users), w);
	} else if (ATOM(_NET_WM_STATE) == event->type) {
		if (!(32 == event->format &&
		     (ATOM(_NET_WM_STATE_FULLSCREEN) == event->data.data32[1] ||
		      ATOM(_NET_WM_STATE_FULLSCREEN) == event->data.data32[2])))
			return;

		switch (event->data.data32[0]) {
		case XCB_EWMH_WM_STATE_REMOVE:
			w->fullscreen = false;
			break;

		case XCB_EWMH_WM_STATE_ADD:
			w->fullscreen = true;
			break;

		case XCB_EWMH_WM_STATE_TOGGLE:
			w->fullscreen ^= true;
			break;
		}

		win_update_net(w);
		/* For some mysterious reasons, some programs (like Chromium)
		 * resize themselves on going fullscreen though window size has
		 * not changed. WHAT. THE. FUCK. ARE THEY DOING? */
		win_send_configure_notify(w);
	}
}

static void
handle_input_key(xcb_input_key_press_event_t const *event)
{
	xcb_input_device_id_t deviceid = event->sourceid;
	struct device *dev = device_find_by_id(deviceid);
	struct user *u = dev->user;
	if (!dev->keymap)
		dev->keymap = xkb_x11_keymap_new_from_device(
				xkb_context, conn,
				deviceid, XKB_KEYMAP_COMPILE_NO_FLAGS);
	if (!dev->keymap) {
		fprintf(stderr, "Could not load keymap\n");
		return;
	}

	xkb_keysym_t const *syms;
	int nsyms = xkb_keymap_key_get_syms_by_level(dev->keymap, event->detail, 0,
			/* Only care about Shift when user really presses. */
			(XCB_MOD_MASK_SHIFT & event->mods.base ? 1 : 0), &syms);

	for (int i = 0; i < nsyms; ++i)
		user_feed_key(u, syms[i], event);
}

static void
handle_input_button_press(xcb_input_button_press_event_t const *event)
{
	if (!(mod_super & event->mods.base))
		return;

	struct user *u = user_find_by_device(event->sourceid);
	if (!u)
		return;

	if (1 == event->detail) {
		if (MODE_NORMAL != u->mode)
			return;

		struct win *w = win_hash_get(event->child);
		if (!w)
			return;

		user_focus_win(u, w);
		u->pointer_x = w->user_geom.x - (event->root_x >> 16);
		u->pointer_y = w->user_geom.y - (event->root_y >> 16);
		user_set_mode(u, MODE_MOUSE);
	} else if (3 == event->detail) {
		struct win *w = u->focused_win;

		int16_t pointer_x = event->root_x >> 16;
		int16_t pointer_y = event->root_y >> 16;
		xcb_rectangle_t rect = w->user_geom;

#define xmacro(x, width) \
	if (pointer_##x < rect.x + rect.width / 2) { \
		rect.width -= pointer_##x - rect.x; \
		rect.x = pointer_##x; \
	} else { \
		rect.width = pointer_##x - rect.x; \
	}
		xmacro(x, width);
		xmacro(y, height);
#undef xmacro

		win_set_user_geom(w, &rect);
	} else if (4 == event->detail || 5 == event->detail) {
		int dir = 4 == event->detail ? 1 : -1;
		if (XCB_MOD_MASK_SHIFT & event->mods.base) {
			user_jump_sessions(u, dir);
		} else {
			user_jump_tabs(u, dir);
		}
	}
}

static void
handle_input_button_release(xcb_input_button_press_event_t const *event)
{
	if (!(mod_super & event->mods.base))
		return;

	struct user *u = user_find_by_device(event->sourceid);
	if (!u)
		return;

	if (MODE_MOUSE == u->mode)
		user_set_mode(u, MODE_NORMAL);
}

static void
handle_input_motion(xcb_input_motion_event_t const *event)
{
	struct user *u = user_find_by_device(event->sourceid);
	if (!u)
		return;
	if (MODE_MOUSE != u->mode)
		return;

	struct win *w = u->focused_win;
	if (!w->floating)
		return;

	/* For convenience. */
	if (w->tab->zoomed_win == w) {
		w->tab->zoomed_win = NULL;
		wins_changed = true;
	}

	xcb_rectangle_t rect = w->user_geom;
	rect.x = (event->root_x >> 16) + u->pointer_x;
	rect.y = (event->root_y >> 16) + u->pointer_y;
	win_set_user_geom(w, &rect);
}

static void
handle_input_enter(xcb_input_enter_event_t const *event)
{
	if (XCB_INPUT_NOTIFY_MODE_NORMAL != event->mode)
		return;
	if (event->event == event->root)
		return;

	XDO(xcb_input_xi_set_client_pointer, conn, event->event, event->deviceid);
}

static void
handle_input_focus_in(xcb_input_focus_in_event_t const *event)
{
	if (XCB_INPUT_NOTIFY_MODE_NORMAL != event->mode)
		return;

	struct user *u = user_find_by_device(event->deviceid);
	if (!u)
		return;

	if (event->event != user_get_focus_target(u) ||
	    /* Focus in event generated for root (and no focus out) but we have
	     * to forcefully focus it again to make it really focused... */
	    !u->focused_win)
		user_update_input_focus(u);
}

static void
handle_input_hierarchy_change(xcb_input_hierarchy_event_t const *event)
{
	if (event->flags & (
		XCB_INPUT_HIERARCHY_MASK_MASTER_ADDED |
		XCB_INPUT_HIERARCHY_MASK_MASTER_REMOVED |
		XCB_INPUT_HIERARCHY_MASK_SLAVE_ADDED |
		XCB_INPUT_HIERARCHY_MASK_SLAVE_REMOVED |
		XCB_INPUT_HIERARCHY_MASK_SLAVE_ATTACHED |
		XCB_INPUT_HIERARCHY_MASK_SLAVE_DETACHED
	))
		user_update_all();
}

static void
handle_input_event(xcb_ge_generic_event_t const *event)
{
	switch (event->event_type) {
	case XCB_INPUT_KEY_PRESS:
	case XCB_INPUT_KEY_RELEASE:
		handle_input_key((void const *)event);
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

	case XCB_INPUT_ENTER:
		handle_input_enter((void const *)event);
		break;

	case XCB_INPUT_FOCUS_IN:
		handle_input_focus_in((void const *)event);
		break;

	case XCB_INPUT_HIERARCHY:
		handle_input_hierarchy_change((void const *)event);
		break;
	}
}

static void
handle_generic_event(xcb_ge_generic_event_t const *event)
{
	if (xi_opcode == event->extension)
		handle_input_event(event);
}

static void
handle_keyboard_mapping(uint8_t deviceid)
{
	struct device *dev = device_find_by_id(deviceid);
	if (!dev)
		return;
	/* New mapping is loaded upon the first key event. */
	xkb_keymap_unref(dev->keymap);
	dev->keymap = NULL;
}

static void
handle_xkb_new_keyboard_notify(xcb_xkb_new_keyboard_notify_event_t const *event)
{
	if (XCB_XKB_NKN_DETAIL_DEVICE_ID & event->changed) {
		assert(!"Unimplemented; maybe not needed at all");
		/* handle_keyboard_mapping(event->oldDeviceID); */
		/* event->deviceID; */
	} else if (XCB_XKB_NKN_DETAIL_KEYCODES & event->changed)
		handle_keyboard_mapping(event->deviceID);
}

static void
handle_xkb_map_notify(xcb_xkb_map_notify_event_t const *event)
{
	handle_keyboard_mapping(event->deviceID);
}

static bool
handle_xkb_event(xcb_generic_event_t const *event)
{
	if (!xkb_first_event)
		return false;

	switch (XCB_EVENT_RESPONSE_TYPE(event) - xkb_first_event) {
	case XCB_XKB_NEW_KEYBOARD_NOTIFY:
		handle_xkb_new_keyboard_notify((void const *)event);
		break;

	case XCB_XKB_MAP_NOTIFY:
		handle_xkb_map_notify((void const *)event);
		break;

	default:
		return false;
	}

	return true;
}

static void
handle_randr_screen_change_notify(xcb_randr_screen_change_notify_event_t const *event)
{
	(void)event;
	output_update_all();
}

static bool
handle_randr_event(xcb_generic_event_t const *event)
{
	if (!randr_first_event)
		return false;

	switch (XCB_EVENT_RESPONSE_TYPE(event) - randr_first_event) {
	case XCB_RANDR_SCREEN_CHANGE_NOTIFY:
		handle_randr_screen_change_notify((void const *)event);
		break;

	default:
		return false;
	}

	return true;
}

static void
handle_shape_notify(xcb_shape_notify_event_t const *event)
{
	struct win *w = win_hash_get(event->affected_window);
	if (!w)
		return;

	if (XCB_SHAPE_SK_BOUNDING == event->shape_kind)
		w->shaped = event->shaped;
	win_update_shape(w, event->shape_kind);
}

static bool
handle_shape_event(xcb_generic_event_t const *event)
{
	if (!shape_first_event)
		return false;

	switch (XCB_EVENT_RESPONSE_TYPE(event) - shape_first_event) {
	case XCB_SHAPE_NOTIFY:
		handle_shape_notify((void const *)event);
		break;

	default:
		return false;
	}

	return true;
}

static bool
handle_extension_event(xcb_generic_event_t const *event)
{
	return
		handle_xkb_event(event) ||
		handle_randr_event(event) ||
		handle_shape_event(event);
}

static void
handle_event(xcb_generic_event_t *event)
{
#if 0
	printf("event (%d)%s\n", event->response_type, xcb_event_get_label(event->response_type));
#endif

	switch (XCB_EVENT_RESPONSE_TYPE(event)) {
#define xmacro(type, handler) \
	case type: \
		handler((void const *)event); \
		break;
	xmacro(XCB_ERROR_NOTIFY,      handle_error);
	xmacro(XCB_EXPOSE,            handle_expose);
	xmacro(XCB_UNMAP_NOTIFY,      handle_unmap_notify);
	xmacro(XCB_MAP_REQUEST,       handle_map_request);
	xmacro(XCB_CONFIGURE_NOTIFY,  handle_configure_notify);
	xmacro(XCB_CONFIGURE_REQUEST, handle_configure_request);
	xmacro(XCB_PROPERTY_NOTIFY,   handle_property_notify);
	xmacro(XCB_CLIENT_MESSAGE,    handle_client_message);
	xmacro(XCB_GE_GENERIC,        handle_generic_event);
#undef xmacro
	default:
		handle_extension_event((void const *)event);
		break;
	}
}

static void
setup_signals(void)
{
	struct sigaction sa;
	sigfillset(&sa.sa_mask);
	sa.sa_flags = 0;
	sigprocmask(SIG_SETMASK, &sa.sa_mask, &saved_set);

	sa.sa_handler = handle_child;
	sigaction(SIGCHLD, &sa, NULL);
}

static void
setup_atoms(void)
{
	xcb_intern_atom_cookie_t cookies[ARRAY_SIZE(ATOM_NAMES)];

	ARRAY_IFOREACH(i, ATOM_NAMES) {
		char const *name = ATOM_NAMES[i];
		XGET_COOKIE(cookie, xcb_intern_atom, conn, false, strlen(name), name);
		cookies[i] = cookie;
	}

	ARRAY_IFOREACH(i, ATOM_NAMES) {
		XGET_REPLY_LOCAL(reply, xcb_intern_atom, conn, cookies[i]);
		if (reply) {
			atoms[i] = reply->atom;
			free(reply);
		}
	}
}

static void
setup_properties(void)
{
	size_t i = 0;
#define xmacro(atom, type, cb) \
	properties[i++] = (struct prop const){ atom, type, cb };
	PROPERTIES
#undef xmacro
}

static void
setup_extensions(void)
{
	xcb_query_extension_reply_t const *ext;

	xcb_prefetch_extension_data(conn, &xcb_input_id);
	xcb_prefetch_extension_data(conn, &xcb_randr_id);
	xcb_prefetch_extension_data(conn, &xcb_shape_id);
	xcb_prefetch_extension_data(conn, &xcb_xkb_id);

	/* https://www.x.org/releases/X11R7.7/doc/inputproto/XI2proto.txt */
	ext = xcb_get_extension_data(conn, &xcb_input_id);
	if (!ext->present)
		fprintf(stderr, "XInput extension missing; input will not work\n");
	else
		xi_opcode = ext->major_opcode;

	/* https://www.x.org/releases/X11R7.5/doc/randrproto/randrproto.txt */
	ext = xcb_get_extension_data(conn, &xcb_randr_id);
	if (!ext->present)
		fprintf(stderr, "RandR extension missing; multihead display will not work properly\n");
	else
		randr_first_event = ext->first_event;

	/* https://www.x.org/releases/X11R7.7/doc/xextproto/shape.html */
	ext = xcb_get_extension_data(conn, &xcb_shape_id);
	if (!ext->present)
		fprintf(stderr, "Shape extension missing; labels cannot be made transparent\n");
	else
		shape_first_event = ext->first_event;

	ext = xcb_get_extension_data(conn, &xcb_xkb_id);
	if (!ext->present) {
		fprintf(stderr, "XKB extension missing; keyboard input will not work\n");
	} else {
		xkb_first_event = ext->first_event;

		XGET(reply, xcb_xkb_use_extension, conn,
				XCB_XKB_MAJOR_VERSION,
				XCB_XKB_MINOR_VERSION);
		if (reply) {
			if (!reply->supported) {
				fprintf(stderr, "Requested XKB version %d.%d not supported by server, got %d.%d; keyboard input may not work\n",
						XCB_XKB_MAJOR_VERSION, XCB_XKB_MINOR_VERSION,
						reply->serverMajor, reply->serverMinor);
			}
			free(reply);
		}

		xkb_context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
		XASSERT(xkb_context);
	}
}

static void
setup(void)
{
	setup_signals();

	int error;
	int preferred_screen;
	conn = xcb_connect(NULL, &preferred_screen);
	if ((error = xcb_connection_has_error(conn))) {
		fprintf(stderr, "Cannot open XCB connection: %s\n",
				xcb_connection_strerror(error));
		exit(EXIT_FAILURE);
	}

	setup_atoms();
	setup_properties();
	setup_extensions();

	xcb_setup_t const *setup = xcb_get_setup(conn);

	int i = 0;
	for (xcb_screen_iterator_t iter = xcb_setup_roots_iterator(setup);
	     0 < iter.rem;
	     ++i, xcb_screen_next(&iter))
	{
		if (i != preferred_screen)
			continue;

		screen = iter.data;
		visual_type = screen_get_visualtype(screen);
		break;
	}

	/* xcb_connect() does not allow invalid preferred_screen so it should
	 * be always set. */
	assert(screen);
	screen_setup();
}

static void
run(void)
{
	sigset_t none;
	sigemptyset(&none);

	struct pollfd fds[1];
	fds[0].fd = xcb_get_file_descriptor(conn);
	fds[0].events = POLLIN | POLLERR;

	int error;
	while (!(error = xcb_connection_has_error(conn))) {
		xcb_generic_event_t *event;
		for (; (event = xcb_poll_for_event(conn)); free(event))
			handle_event(event);
		/* Flush before going to sleep. */
		for (bool should_flush = true; should_flush;) {
			server_update_wins();

			/* xcb_flush() not only sends pending requests but also
			 * reads connection for new messages. Thus we have to
			 * check the newly appeared messages after a flush.
			 * RTFS. */
			xcb_flush(conn);
			should_flush = false;

			for (; (event = xcb_poll_for_queued_event(conn)); free(event)) {
				handle_event(event);
				should_flush = true;
			}
		}
		ppoll(fds, 1, NULL, &none);
	}

	fprintf(stderr, "XCB connection closed: %s\n", xcb_connection_strerror(error));
	exit(EXIT_FAILURE);
}

int
main(void)
{
	setup();
	run();
}
