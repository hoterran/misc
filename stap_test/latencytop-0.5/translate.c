/*
 * Copyright 2007, Intel Corporation
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
#include <time.h>

#include <glib.h>

#include "latencytop.h"


/* translate kernel output to human readable */
struct translate_line {
	int priority;
	char function[200];
	char display[200];	
};


GList *translations;

char *translate(char *line)
{
	char buffer[4096], *c, *c2;
	int prio = 0;
	char *selected = NULL;
	GList *item;
	struct translate_line *trans;

	memset(buffer, 0, 4096);	
	strncpy(buffer, line, 4096);

	c2 = buffer;
	while (c2[0] == ' ') c2++;

	while (c2 && strlen(c2) > 0) {

		c = strchr(c2, ' ');
		if (c) *c = 0;

		item = g_list_first(translations);
		while (item) {
			trans = item->data;
			item = g_list_next(item);
			if (trans->priority >= prio && strcmp(trans->function, c2)==0) {
				selected = trans->display;
				prio = trans->priority;
			}
		}

		if (c)
			c2 = c+1;
		else
			c2 = NULL;
	}
	if (dump_unknown && prio < 2) {
		FILE *file;
		file = fopen("latencytop.log", "w+");
		if (file) {
			fprintf(file, "%s\n", line);
			fclose(file);
		}
		printf("Unknown: %s\n", line);
	}
	return selected;
}

void init_translations(char *filename)
{
	char *c, *c2;
	FILE *file;
	
	file = fopen(filename, "r");
	if (!file)
		return;
	while (!feof(file)) {
		struct translate_line *trans;
		size_t dummy;
		char *line = NULL;
		if (getline(&line, &dummy, file) <= 0) {
			free(line);
			break;
		}
		if (line[0]=='#') {
			free(line);
			continue;
		}

		trans = malloc(sizeof(struct translate_line));
		memset(trans, 0, sizeof(trans));

		c2 = line;
		c = strchr(c2, '\t');
		if (c) {
			*c = 0;
			trans->priority = strtoull(c2, NULL, 10);
			c2 = c+1;
		}

		c = strchr(c2, '\t');
		if (c) {
			*c = 0;
			strcpy(trans->function, c2);
			c2 = c+1;
		}
		while (*c2=='\t') c2++;
		strcpy(trans->display, c2);
		translations = g_list_append(translations, trans);
		free(line);
	}
	fclose(file);
}
