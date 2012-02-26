/* vim: noet sw=8
 * This is a butchered version of the Mono log profiler; most features have
 * been removed, except for some features needed for heap analysis of niecza
 * which have been added.  Changes by sorear.
 */

/*
 * proflog.c: mono log profiler
 *
 * Author:
 *   Paolo Molaro (lupus@ximian.com)
 *
 * Copyright 2010 Novell, Inc (http://www.novell.com)
 * Copyright 2011 Xamarin Inc (http://www.xamarin.com)
 */

#include <mono/metadata/profiler.h>
#include <mono/metadata/threads.h>
#include <mono/metadata/mono-gc.h>
#include <mono/metadata/debug-helpers.h>

#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* For each heap shot, we do BFS to find the shortest route from the root set
 * to (all objects), then explain a sample of objects to see retention. */

/* tracks per-object data */
struct obj_info {
	MonoObject* addr; /* where is the object?  primary key. */
	size_t first_ptr; /* rference to NULL-terminated list of referees */
	size_t size;      /* byte count */
	size_t total_size;/* for fast picking */
	struct obj_info* prev; /* filled by BFS; points to previous */
};

/* we keep the roots in a hash set */
struct root_hash_link {
	struct root_hash_link* next;
	MonoObject* root;
};

/* holds per-image information for the types that are duplicated between Kernel
   and Run.Kernel */
struct per_image {
	MonoImage* image;
	MonoClass* p6any;
};

struct _MonoProfiler {
	MonoObject **ptrs;
	size_t ptrs_allocated;
	size_t ptrs_used;

	struct obj_info *objs;
	size_t objs_allocated;
	size_t objs_used;

	struct root_hash_link** root_hash;
	size_t num_roots;
	size_t root_hash_size;

	size_t total_prob;

	struct per_image *run_img, *comp_img;
};

static void buffer_add(void **bufp, size_t *allocp, size_t *usedp, void *src,
		size_t nadd, size_t quantum) {
	if (*usedp + nadd > *allocp) {
		size_t newalloc = (*usedp + nadd);
		newalloc += newalloc / 2;
		*bufp = realloc(*bufp, newalloc * quantum);
		*allocp = newalloc;
	}
	memcpy(((char*)(*bufp)) + (quantum * *usedp), src, nadd * quantum);
	*usedp += nadd;
}

static int do_heap_shot = 0;

/* For linux compile with:
 * gcc -fPIC -shared -o libmono-profiler-log.so proflog.c utils.c -Wall -g -lz `pkg-config --cflags --libs mono-2`
 * gcc -o mprof-report decode.c utils.c -Wall -g -lz -lrt -lpthread `pkg-config --cflags mono-2`
 *
 * For osx compile with:
 * gcc -m32 -Dmono_free=free shared -o libmono-profiler-log.dylib proflog.c utils.c -Wall -g -lz `pkg-config --cflags mono-2` -undefined suppress -flat_namespace
 * gcc -m32 -o mprof-report decode.c utils.c -Wall -g -lz -lrt -lpthread `pkg-config --cflags mono-2`
 *
 * Install with:
 * sudo cp mprof-report /usr/local/bin
 * sudo cp libmono-profiler-log.so /usr/local/lib
 * sudo ldconfig
 */

typedef struct _NieczaP6any NieczaP6any;
typedef struct _NieczaSTable NieczaSTable;
typedef struct _NieczaVariable NieczaVariable;

struct _NieczaP6any {
	MonoObject object;
	NieczaSTable *mo;
};

struct _NieczaSTable {
	MonoObject object;
	void *mo;
	NieczaP6any *how, *who;
	NieczaP6any *typeObject, *initObject;
	NieczaVariable *typeVar, *initVar;
	MonoString *name;
	// and a million more that I don't care about now
};

static int
explain_object_image (struct per_image *pi, MonoObject *obj)
{
	if (pi->p6any && mono_object_isinst(obj, pi->p6any)) {
		NieczaP6any *p6 = (NieczaP6any *)obj;
		if (p6->mo && p6->mo->name) {
			char *mname = mono_string_to_utf8(p6->mo->name);
			printf(":: %s\n", mname);
			mono_free(mname);
		} else {
			printf("(p6, no name available)\n");
		}
		return 1;
	}
	return 0;
}

static void
explain_object (MonoProfiler *prof, MonoObject *obj)
{
	if (prof->run_img && explain_object_image(prof->run_img, obj))
		return;
	if (prof->comp_img && explain_object_image(prof->comp_img, obj))
		return;

	printf("(clr) %s\n", mono_class_get_name(mono_object_get_class(obj)));
}

static int
gc_reference (MonoObject *obj, MonoClass *klass, uintptr_t size, uintptr_t num, MonoObject **refs, uintptr_t *offsets, void *data)
{
	MonoProfiler *prof = (MonoProfiler *) data;
	if (size == 0) {
		MonoObject *oref = prof->objs[prof->objs_used - 1].addr;
		if (obj != oref) {
			printf("Strange object continuation %p %p\n", (void*)obj, (void*)oref);
			return 0;
		}
		prof->ptrs_used--;
	} else {
		struct obj_info oi;
		oi.addr = obj;
		oi.first_ptr = prof->ptrs_used;
		oi.size = size;
		oi.total_size = 0;
		oi.prev = NULL;
		buffer_add((void**)(&prof->objs), &prof->objs_allocated,
			&prof->objs_used, (void*)(&oi), 1, sizeof(oi));
		//printf("Add info for object %p\n", (void*)obj);
	}
	MonoObject* null = NULL;
	buffer_add((void**)(&prof->ptrs), &prof->ptrs_allocated,
		&prof->ptrs_used, (void*)refs, num, sizeof(MonoObject*));
	buffer_add((void**)(&prof->ptrs), &prof->ptrs_allocated,
		&prof->ptrs_used, (void*)&null, 1, sizeof(MonoObject*));
	return 0;
}

static unsigned int hs_mode_s = 0;
static unsigned int hs_mode_gc = 0;
static unsigned int gc_count = 0;
static unsigned int last_gc_gen_started = -1;
static time_t last_hs_time = 0;

static int
obj_compare_addr(const void *a, const void *b)
{
	MonoObject *p1 = ((const struct obj_info*)a)->addr;
	MonoObject *p2 = ((const struct obj_info*)b)->addr;

	return (p1 < p2) ? -1 : (p1 == p2) ? 0 : 1;
}

static struct obj_info*
get_info(MonoProfiler* prof, MonoObject* obj)
{
	size_t left = 0;
	size_t right = prof->objs_used;

	while (right != left) {
		size_t mid = left + (right - left) / 2; /* may coincide with left */
		struct obj_info *minf = &(prof->objs[mid]);
		if (minf->addr == obj)
			return minf;
		if (minf->addr > obj) {
			right = mid;
		} else {
			left = mid + 1;
		}
	}
	//printf("Oops!  No information for object %p\n", obj);
	return NULL;
}

static void
bfs_mark(struct obj_info ***qtail, struct obj_info *item, struct obj_info *from)
{
	if (!item)
		return;
	if (!item->total_size) {
		item->total_size = 1;
		item->prev = from;
		*((*qtail)++) = item;
	}
}

// for the root set
static struct obj_info dummy;

static void
bfs(MonoProfiler *prof)
{
	/* queue holds things that have been marked, but not their children */
	struct obj_info **qbuf = (struct obj_info**)malloc(sizeof(struct obj_info*) * prof->objs_used);
	struct obj_info **qhead = qbuf, **qtail = qbuf;
	size_t ix;
	struct root_hash_link *lk;
	for (ix = 0; ix < prof->root_hash_size; ix++) {
		for (lk = prof->root_hash[ix]; lk; lk = lk->next) {
			bfs_mark(&qtail, get_info(prof, lk->root), &dummy);
		}
	}

	while (qhead != qtail) {
		struct obj_info *inf = *(qhead++);
		MonoObject** pptr = prof->ptrs + inf->first_ptr;

		while (*pptr) {
			bfs_mark(&qtail, get_info(prof, *(pptr++)), inf);
		}
	}

	printf("BFS done, visited %td objects\n", qtail - qbuf);
	free(qbuf);
}

static void
gen_probs (MonoProfiler *prof)
{
	size_t ix, totbytes = 0;

	for (ix = 0; ix < prof->objs_used; ix++) {
		prof->objs[ix].total_size = totbytes;
		totbytes += prof->objs[ix].size;
	}

	prof->total_prob = totbytes;
	printf("Probability generation done, total %zd bytes\n", totbytes);
}

static void
sample (MonoProfiler *prof)
{
	size_t byteix;
	size_t max = prof->total_prob;
	do {
		byteix = rand() / (RAND_MAX / max);
	} while (byteix >= max);

	size_t ixl = 0;
	size_t ixr = prof->objs_used;
	while (ixr - ixl > 1) {
		size_t ixm = ixl + (ixr - ixl) / 2;
		if (byteix >= prof->objs[ixm].total_size) {
			ixl = ixm;
		} else {
			ixr = ixm;
		}
	}

	struct obj_info *obj = &(prof->objs[ixl]);

	printf("Byte %zd of %zd, object %zd of %zd, size %zd\n",
		byteix, max, ixl, prof->objs_used, obj->size);
	while(1) {
		if (obj == &dummy) {
			printf("[Root object]\n\n");
			return;
		}
		if (!obj) {
			printf("[Reference set unknown]\n\n");
			return;
		}
		printf("%p  ", obj->addr);
		explain_object(prof, obj->addr);
		obj = obj->prev;
	}
}

static struct per_image*
get_image (const char *name)
{
	MonoImage* img = mono_image_loaded(name);
	if (!img) {
		printf("%s: No such image\n", name);
		return NULL;
	}

	struct per_image *pi = (struct per_image *)malloc(sizeof(struct per_image));

	pi->image = img;
	pi->p6any = mono_class_from_name(pi->image, "Niecza", "P6any");

	printf("%s: img=%p p6any=%p\n", name, pi->image, pi->p6any);
	return pi;
}

static void
heap_walk (MonoProfiler *prof)
{
	int do_walk = 0;
	time_t now = time(0);
	if (!do_heap_shot)
		return;
	if (hs_mode_s && (now - last_hs_time) >= hs_mode_s)
		do_walk = 1;
	else if (hs_mode_gc && (gc_count % hs_mode_gc) == 0)
		do_walk = 1;
	else if (!hs_mode_s && !hs_mode_gc && last_gc_gen_started == mono_gc_max_generation ())
		do_walk = 1;

	if (!do_walk)
		return;

	printf("Heap shot started... %zd roots\n", prof->num_roots);
	prof->comp_img = get_image("Kernel");
	prof->run_img = get_image("Run.Kernel");

	mono_gc_walk_heap (0, gc_reference, prof);
	last_hs_time = now;
	printf("Heap walk complete: %zd objects %zd pointers.\n", prof->objs_used, prof->ptrs_used);
	/* to allow effective access, sort the object set */
	qsort(prof->objs, prof->objs_used, sizeof(struct obj_info),
		obj_compare_addr);
	bfs(prof);
	gen_probs(prof);
	int ix;
	for (ix = 0; ix < prof->objs_used / 1000; ix++)
		sample(prof);

	free(prof->ptrs);
	free(prof->objs);
	free(prof->comp_img);
	free(prof->run_img);
	prof->ptrs_allocated = prof->ptrs_used = prof->objs_allocated =
		prof->objs_used = 0;
	prof->ptrs = NULL;
	prof->objs = NULL;
}

static void
gc_event (MonoProfiler *prof, MonoGCEvent ev, int generation) {
	/* to deal with nested gen1 after gen0 started */
	if (ev == MONO_GC_EVENT_START) {
		last_gc_gen_started = generation;
		if (generation == 0) {
			/* entering GC - clear out the old roots */
			struct root_hash_link *l, *ln;
			size_t ix;
			for (ix = 0; ix < prof->root_hash_size; ix++) {
				l = prof->root_hash[ix];
				while (l != NULL) {
					ln = l->next;
					free(l);
					l = ln;
				}
			}
			free(prof->root_hash);

			prof->root_hash = NULL;
			prof->root_hash_size = prof->num_roots = 0;
		}
		if (generation == mono_gc_max_generation ())
			gc_count++;
	}
	if (ev == MONO_GC_EVENT_PRE_START_WORLD)
		heap_walk (prof);
}

static void
gc_handle (MonoProfiler *prof, int op, int type, uintptr_t handle, MonoObject *obj) {
}

#define HASH_ROOT(prof, ptr) (((size_t)(((uintptr_t) ptr) * 0x9E3779B9UL)) & (prof->root_hash_size - 1))

static void
add_root(MonoProfiler* prof, MonoObject* root)
{
	if (prof->num_roots == prof->root_hash_size) {
		size_t ohsz = prof->root_hash_size;
		size_t ix;
		struct root_hash_link *lp, *nlp;
		struct root_hash_link **ohash = prof->root_hash;
		prof->root_hash_size = ohsz == 0 ? 512 :
			2 * prof->root_hash_size;

		prof->root_hash = (struct root_hash_link **)calloc(
			prof->root_hash_size, sizeof(struct root_hash_link *));
		prof->num_roots = 0;

		for (ix = 0; ix < ohsz; ix++) {
			lp = ohash[ix];
			while (lp) {
				nlp = lp->next;
				add_root(prof, lp->root);
				free(lp);
				lp = nlp;
			}
		}
		free(ohash);
	}

	size_t ix = HASH_ROOT(prof, root);
	struct root_hash_link *l = (struct root_hash_link *)malloc(sizeof(struct root_hash_link));
	l->root = root;
	l->next = prof->root_hash[ix];
	prof->root_hash[ix] = l;
	prof->num_roots++;
	//printf("Added root %p\n", root);
}

static void
move_root(MonoProfiler* prof, MonoObject* from, MonoObject* to)
{
	int oix = HASH_ROOT(prof, from);
	struct root_hash_link **chainp = &prof->root_hash[oix];

	while (1) {
		if (!(*chainp))
			return;
		if ((*chainp)->root == from)
			break;
		chainp = &((*chainp)->next);
	}

	int nix = HASH_ROOT(prof, to);
	struct root_hash_link *link = *chainp;
	*chainp = (*chainp)->next;
	link->next = prof->root_hash[nix];
	link->root = to;
	prof->root_hash[nix] = link;
	//printf("Moved root %p to %p\n", from, to);
}

static void
gc_roots (MonoProfiler *prof, int num, void **objects, int *root_types, uintptr_t *extra_info)
{
	int i;
	for (i = 0; i < num; ++i) {
		add_root(prof, (MonoObject*)objects[i]);
	}
}

static void
gc_moves (MonoProfiler *prof, void **objects, int num)
{
	int i;
	for (i = 0; i < num; i += 2) {
		move_root(prof, (MonoObject*)objects[i], (MonoObject*)objects[i+1]);
	}
}

/* 
 * declaration to silence the compiler: this is the entry point that
 * mono will load from the shared library and call.
 */
extern void
mono_profiler_startup (const char *desc);

static void
log_shutdown (MonoProfiler *prof)
{
	fflush(stdout);
	free(prof);
}

static void
gc_resize (MonoProfiler *profiler, int64_t new_size) {
}

void
mono_profiler_startup (const char *desc)
{
	int events = MONO_PROFILE_GC|MONO_PROFILE_GC_MOVES|
	    MONO_PROFILE_GC_ROOTS;

	do_heap_shot = 1;
	hs_mode_s = 10;

	MonoProfiler* prof = (MonoProfiler*)calloc(1, sizeof(MonoProfiler));
	mono_profiler_install (prof, log_shutdown);
	mono_profiler_install_gc (gc_event, gc_resize);
	mono_profiler_install_gc_moves (gc_moves);
	mono_profiler_install_gc_roots (gc_handle, gc_roots);

	mono_profiler_set_events (events);
}

