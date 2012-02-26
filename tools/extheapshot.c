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
#include <stdio.h>

/*#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>*/

struct _MonoProfiler {
};

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

static MonoImage *img_run, *img_comp;
static MonoClass *p6any_run, *p6any_comp;

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
gc_reference (MonoObject *obj, MonoClass *klass, uintptr_t size, uintptr_t num, MonoObject **refs, uintptr_t *offsets, void *data)
{
	if (size == 0)
		return 0;
	if ((p6any_run && mono_object_isinst(obj, p6any_run)) ||
			(p6any_comp && mono_object_isinst(obj, p6any_comp))) {
		NieczaP6any *p6 = (NieczaP6any *)obj;
		if (p6->mo && p6->mo->name) {
			char *mname = mono_string_to_utf8(p6->mo->name);
			printf(":: %s\n", mname);
			mono_free(mname);
		}
	}
	return 0;
}

static unsigned int hs_mode_s = 0;
static unsigned int hs_mode_gc = 0;
static unsigned int gc_count = 0;
static unsigned int last_gc_gen_started = -1;
static time_t last_hs_time = 0;

static void
heap_walk (MonoProfiler *profiler)
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

	printf("Heap shot started...\n");
	img_run = mono_image_loaded("Run.Kernel");
	img_comp = mono_image_loaded("Kernel");

	p6any_run = img_run == NULL ? NULL :
		mono_class_from_name(img_run, "Niecza", "P6any");
	p6any_comp = img_comp == NULL ? NULL :
		mono_class_from_name(img_comp, "Niecza", "P6any");
	printf("%p %p\n", p6any_run, p6any_comp);

	mono_gc_walk_heap (0, gc_reference, NULL);
	last_hs_time = now;
	printf("Heap shot ends.\n");
}

static void
gc_event (MonoProfiler *profiler, MonoGCEvent ev, int generation) {
	/* to deal with nested gen1 after gen0 started */
	if (ev == MONO_GC_EVENT_START) {
		last_gc_gen_started = generation;
		if (generation == mono_gc_max_generation ())
			gc_count++;
	}
	if (ev == MONO_GC_EVENT_PRE_START_WORLD)
		heap_walk (profiler);
	//printf ("gc event %d for generation %d\n", ev, generation);
}

static void
gc_handle (MonoProfiler *prof, int op, int type, uintptr_t handle, MonoObject *obj) {
}

static void
gc_roots (MonoProfiler *prof, int num, void **objects, int *root_types, uintptr_t *extra_info)
{
	int i;
	for (i = 0; i < num; ++i) {
		//printf("root: %p t=%d ei=%d\n", objects[i], root_types[i], (int)extra_info[i]);
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

	mono_profiler_install (NULL, log_shutdown);
	mono_profiler_install_gc (gc_event, gc_resize);
	mono_profiler_install_gc_roots (gc_handle, gc_roots);

	mono_profiler_set_events (events);
}

