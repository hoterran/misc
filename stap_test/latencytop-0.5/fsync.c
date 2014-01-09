/*
 * Copyright 2008, Intel Corporation
 *
 * This file is part of LatencyTOP
 *
 * This program file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program in a file named COPYING; if not, write to the
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 *
 * Authors:
 * 	Arjan van de Ven <arjan@linux.intel.com>
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ncurses.h>
#include <time.h>
#include <wchar.h>
#include <ctype.h>

#include <glib.h>

#include "latencytop.h"

struct fsync_process { 
	char name[PATH_MAX];
	int fsync_count;
	GList *files;
};

struct fsync_files {
	char name[PATH_MAX];
	int fsync_count;
};

static GList *fsync_data;


static chain_file(struct fsync_process *proc, char *filename)
{
	struct fsync_files *file;
	GList *item;

	proc->fsync_count++;
	item = proc->files;
	while (item) {
		file = item->data;
		item = g_list_next(item);
		if (strcmp(file->name, filename)==0) {
			file->fsync_count++;
			return;
		}
	}
	file = malloc(sizeof(struct fsync_files));
	if (!file)
		return;
	memset(file, 0, sizeof(struct fsync_files));
	strncpy(file->name, filename, PATH_MAX-1);
	file->fsync_count = 1;
	proc->files = g_list_append(proc->files, file);
}

static report_file(char *process, char *file)
{
	struct fsync_process *proc;
	GList *item;

	item = fsync_data;
	while (item) {
		proc = item->data;
		item = g_list_next(item);
		if (strcmp(proc->name, process) == 0) {
			chain_file(proc, file);
			return;
		}
	}

	proc = malloc(sizeof(struct fsync_process));
	if (!proc)
		return;
	memset(proc, 0, sizeof(struct fsync_process));
	strncpy(proc->name, process, PATH_MAX-1);
	chain_file(proc, file);
	fsync_data = g_list_append(fsync_data, proc);
}

static gint sort_files(gconstpointer A, gconstpointer B)
{
	struct fsync_files *a = (struct fsync_files *)A;
	struct fsync_files *b = (struct fsync_files *)B;
	return a->fsync_count < b->fsync_count;
}

static gint sort_process(gconstpointer A, gconstpointer B)
{
	struct fsync_process *a = (struct fsync_process *)A;
	struct fsync_process *b = (struct fsync_process *)B;
	return a->fsync_count < b->fsync_count;
}

static void sort_the_lot(void)
{
	GList *item;
	struct fsync_process *proc;

	item = fsync_data = g_list_sort(fsync_data, sort_process);
	while (item) {
		proc = item->data;
		item = g_list_next(item);
		proc->files = g_list_sort(proc->files, sort_files);
	}
}



static void write_to_file(char *filename, char *value)
{
	FILE *file;
	file = fopen(filename, "w");
	if (!file)
		return;
	fprintf(file,"%s\n", value);
	fclose(file);
}


int enable_fsync_tracer(void)
{
	int ret;
/*
 * Steps to do:
 *
 * mount -t debugfs none /sys/kernel/debug/
 * cd /sys/kernel/debug/tracing
 * echo fsync > current_tracer
 * echo ftrace_printk > iter_ctrl 
 * echo 1 > tracing_enabled
 */
	ret = system("/bin/mount -t debugfs none /sys/kernel/debug/");
	if (!ret) 
		return -1;
	write_to_file("/sys/kernel/debug/tracing/current_tracer", "fsync");	
	write_to_file("/sys/kernel/debug/tracing/iter_ctrl", "ftrace_printk");	
	write_to_file("/sys/kernel/debug/tracing/tracing_enabled", "1");
}

int disable_fsync_tracer(void)
{
	write_to_file("/sys/kernel/debug/tracing/tracing_enabled", "0");
}


static WINDOW *title_bar_window;
static WINDOW *global_window;


static void fsync_cleanup_curses(void) 
{
	endwin();
}


static void zap_windows(void)
{
	if (title_bar_window) {
		delwin(title_bar_window);
		title_bar_window = NULL;
	}
	if (global_window) {
		delwin(global_window);
		global_window = NULL;
	}
}


static int maxx, maxy;

static void fsync_setup_windows(void) 
{
	int midy;
	getmaxyx(stdscr, maxy, maxx);

	zap_windows();	

	title_bar_window = subwin(stdscr, 1, maxx, 0, 0);
	global_window = subwin(stdscr, maxy-3 , maxx, 2, 0);

	werase(stdscr);
	refresh();
}

#if 0 /* Dead code */
static void fsync_initialize_curses(void) 
{
	if (noui)
		return;
	initscr();
	start_color();
	keypad(stdscr, TRUE);	/* enable keyboard mapping */
	nonl();			/* tell curses not to do NL->CR/NL on output */
	cbreak();		/* take input chars one at a time, no wait for \n */
	noecho();		/* dont echo input */
	curs_set(0);		/* turn off cursor */
	use_default_colors();

	init_pair(PT_COLOR_DEFAULT, COLOR_WHITE, COLOR_BLACK);
	init_pair(PT_COLOR_HEADER_BAR, COLOR_BLACK, COLOR_WHITE);
	init_pair(PT_COLOR_ERROR, COLOR_BLACK, COLOR_RED);
	init_pair(PT_COLOR_RED, COLOR_WHITE, COLOR_RED);
	init_pair(PT_COLOR_YELLOW, COLOR_WHITE, COLOR_YELLOW);
	init_pair(PT_COLOR_GREEN, COLOR_WHITE, COLOR_GREEN);
	init_pair(PT_COLOR_BRIGHT, COLOR_WHITE, COLOR_BLACK);
	
	atexit(cleanup_curses);
}
#endif

static void show_title_bar(void) 
{
	wattrset(title_bar_window, COLOR_PAIR(PT_COLOR_HEADER_BAR));
	wbkgd(title_bar_window, COLOR_PAIR(PT_COLOR_HEADER_BAR));   
	werase(title_bar_window);

	mvwprintw(title_bar_window, 0, 0,  "   LatencyTOP -- fsync() view... type 'F' to exit");

	wrefresh(title_bar_window);
}



static void print_global_list(void)
{
	GList *item, *item2;
	struct fsync_process *proc;
	struct fsync_files *file;
	int i = 1, i2 = 0;
	int y = 1;

	werase(global_window);


	mvwprintw(global_window, 0, 0, "Process        File");
	item = g_list_first(fsync_data);
	while (item && i < maxy-6) {
		proc = item->data;
		item = g_list_next(item);
		
		mvwprintw(global_window, y, 0, "%s (%i)", proc->name, proc->fsync_count);
		y++;
		item2 = proc->files;
		while (item2 && i2 < 5) {
			file = item2->data;
			item2 = g_list_next(item2);
			mvwprintw(global_window, y, 10, "%s (%i)", file->name, file->fsync_count);
			y++;
			i2++;
		}
		i++;
		y++;
	}
	wrefresh(global_window);

}

static void parse_ftrace(void)
{
	FILE *file;
	char line[PATH_MAX];
	file = fopen("/sys/kernel/debug/tracing/trace", "r");
	if (!file)
		return;
	while (!feof(file)) {
		char *c, *c2;
		memset(line, 0, PATH_MAX);
		fgets(line, PATH_MAX-1, file);
		c = strchr(line, '\n');
		if (c) *c = 0;
		c = strstr(line, "probe_do_fsync: Process ");
		if (!c)
			continue;
		c += 24;
		c2 = strchr(c, ' ');
		if (!c2)
			continue;
		*c2 = 0;
		c2++;
		c2 = strstr(c2, "fsync on ");
		if (!c2)
			continue;
		c2 += 9;
		report_file(c, c2);
	}
	fclose(file);
	sort_the_lot();
}


int fsync_display(int duration)
{
	struct timeval start,end,now;
	int key;
	fd_set rfds;
	int curduration;

	fsync_setup_windows();
	show_title_bar();
	curduration = 3;
	if (curduration > duration)
		curduration = duration;
	parse_ftrace();
	print_global_list();
	while (1) {
		FD_ZERO(&rfds);
		FD_SET(0, &rfds);
		gettimeofday(&start, NULL);
		gettimeofday(&now, NULL);
		end.tv_sec = start.tv_sec + curduration - now.tv_sec;
		end.tv_usec = start.tv_usec - now.tv_usec;
		while (end.tv_usec < 0) {
			end.tv_sec --;
			end.tv_usec += 1000000;
		};
		curduration = duration;
		if (curduration > 5)
			curduration = 5;
		/* clear the ftrace buffer */
		write_to_file("/sys/kernel/debug/tracing/tracing_enabled", "0");
		write_to_file("/sys/kernel/debug/tracing/tracing_enabled", "1");
		key = select(1, &rfds, NULL, NULL, &end);
		parse_ftrace();
		print_global_list();

		if (key) {
			char keychar;
			keychar = fgetc(stdin);		
			if (keychar == 27) {
				keychar = fgetc(stdin);	
				if (keychar==79)
					keychar = fgetc(stdin);	
			}
			keychar = toupper(keychar);
			if (keychar == 'F') {
				fsync_cleanup_curses();
				return 1;
			}
			if (keychar == 'Q') {
				fsync_cleanup_curses();
				return 0;
			}
		}
	}

	return 1;
}
