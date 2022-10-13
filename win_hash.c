#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#include "win_hash.h"

enum {
	XORG_MAXCLIENTS = 512,
	WIN_HASH_SIZE = /* XORG_MAXCLIENTS < */ 1999,
};

#define WRAP(x) ((x) - (WIN_HASH_SIZE <= (x) ? WIN_HASH_SIZE : 0))

static uint32_t win_hash_keys[WIN_HASH_SIZE];
static struct win *win_hash_values[WIN_HASH_SIZE];

/* https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key */
static uint32_t
hash(uint32_t x)
{
	x = ((x >> 16) ^ x) * 0x45d9f3b;
	x = ((x >> 16) ^ x) * 0x45d9f3b;
	x = (x >> 16) ^ x;
	return x;
}

static uint32_t
get_bucket_dist(uint32_t i, uint32_t key)
{
	uint32_t k = hash(key) % WIN_HASH_SIZE;
	return WRAP(i + WIN_HASH_SIZE - k);
}

static struct win **
win_hash_pget(uint32_t key)
{
	if (!key)
		return NULL;

	uint32_t i = hash(key) % WIN_HASH_SIZE;
	uint32_t probe_len = 0;
	for (;;) {
		if (key == win_hash_keys[i])
			return &win_hash_values[i];
		else if (!win_hash_keys[i])
			return NULL;
		else if (get_bucket_dist(i, win_hash_keys[i]) < probe_len++)
			return NULL;
		i = WRAP(i + 1);
	}
}

struct win *
win_hash_get(uint32_t key)
{
	struct win **pos = win_hash_pget(key);
	return pos ? *pos : NULL;
}

void
win_hash_set(uint32_t key, struct win *value)
{
	assert(key);

	uint32_t i = hash(key) % WIN_HASH_SIZE;
	uint32_t probe_len = 0;
	for (;;) {
		if (!win_hash_keys[i] || win_hash_keys[i] == key) {
			win_hash_keys[i] = key;
			win_hash_values[i] = value;
			break;
		}

		if (get_bucket_dist(i, win_hash_keys[i]) < probe_len++) {
			uint32_t tmp_key = key;
			void *tmp_value = value;
			key = win_hash_keys[i];
			value = win_hash_values[i];
			win_hash_keys[i] = tmp_key;
			win_hash_values[i] = tmp_value;
		}

		i = WRAP(i + 1);
	}
}

void
win_hash_pdel(struct win **pos)
{
	uint32_t i = pos - win_hash_values;
	uint32_t tail = i;
	for (;;) {
		i = WRAP(i + 1);
		if (!win_hash_keys[i])
			break;
		if (hash(win_hash_keys[i]) % WIN_HASH_SIZE == i)
			break;
		win_hash_values[tail] = win_hash_values[i];
		win_hash_keys[tail] = win_hash_keys[i];
		tail = i;
	}
	win_hash_keys[tail] = 0 /* None. */;
}

void
win_hash_del(uint32_t key)
{
	struct win **pos = win_hash_pget(key);
	if (pos)
		win_hash_pdel(pos);
}
