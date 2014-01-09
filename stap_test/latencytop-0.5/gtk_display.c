/*
 * Copyright 2008, Benjamin Herrenschmidt
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <dirent.h>
#include <time.h>
#include <wchar.h>
#include <ctype.h>
#include <assert.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "latencytop.h"

static GtkWidget *main_window;
static GtkListStore *targets_model;
static GtkWidget *targets_view;
static GtkListStore *results_model;
static GtkWidget *results_view;
static GtkListStore *backtrace_model;
static GtkWidget *backtrace_view;
static GtkWidget *countdown_label;
static GtkWidget *freeze_button;

static GdkPixbuf *global_icon;
static GdkPixbuf *kthread_icon;
static GdkPixbuf *process_icon;

static unsigned int cur_target_pid;

static int countdown;
static int countdown_max = 30;
static int countdown_frozen;

static int ignore_sel;
static int force_results;

/* Column indices in the various trees */
enum {
	COL_R_CAUSE = 0,
	COL_R_MAX,
	COL_R_PERCENT,
	COL_R_LINE
};

enum {
	COL_T_ICON = 0,
	COL_T_NAME,
	COL_T_MAX,
	COL_T_PROC,
};

enum {
	COL_B_FUNC = 0,
};

static void remove_all_targets(void)
{
	gtk_list_store_clear(targets_model);
}

static void remove_all_results(void)
{
	gtk_list_store_clear(results_model);
	gtk_list_store_clear(backtrace_model);
}

static gint compare_process(gconstpointer a, gconstpointer b)
{
	const struct process *pa = a;
	const struct process *pb = b;

	return pb->max - pa->max;
}

static void insert_all_targets(void)
{
	GList *entry;
	struct process *proc;
	GtkTreeIter iter;
	gchar *esctext;
	GdkPixbuf *icon;
	GtkTreePath *path = NULL;
	int preferred;

	/* Force the selection change to update the results */
	force_results = 1;

	/* Add the "system" target */
        gtk_list_store_append (GTK_LIST_STORE(targets_model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(targets_model), 
	                    &iter,
			    COL_T_ICON, global_icon,
                            COL_T_NAME, "<b>Global</b>",
			    COL_T_MAX, "",
			    COL_T_PROC, NULL,
	                    -1);

	/* Select it if it's our current target */
	if (cur_target_pid == 0)
		path = gtk_tree_model_get_path(GTK_TREE_MODEL(targets_model), &iter);

	/* The core doesn't sort the list, we do here */
	entry = g_list_sort(procs, compare_process);
	while(entry) {
		char targetstr[64];
		char maxstr[32];

		proc = entry->data;
		entry = g_list_next(entry);

		preferred = proc->name && prefered_process &&
			strcmp(proc->name, prefered_process) == 0;

		/* Skip too small values */
		if (!preferred && (proc->max * 0.001 < 0.1))
			continue;

		if (preferred)
			gtk_list_store_insert(GTK_LIST_STORE(targets_model), &iter, 1);
		else
			gtk_list_store_append(GTK_LIST_STORE(targets_model), &iter);
		esctext = g_markup_escape_text(proc->name, -1);
		icon = proc->kernelthread ? kthread_icon : process_icon;
		sprintf(maxstr, "%.1f", proc->max * 0.001);
		targetstr[63] = 0;
		if (preferred)
			snprintf(targetstr, 63, "<u>%s</u>", esctext);
		else
			strncpy(targetstr, esctext, 63);
		gtk_list_store_set(GTK_LIST_STORE(targets_model), 
				   &iter,
				   COL_T_ICON, icon,
				   COL_T_NAME, targetstr,
				   COL_T_MAX, maxstr,
				   COL_T_PROC, proc,
				   -1);
		if (cur_target_pid == proc->pid)
			path = gtk_tree_model_get_path(GTK_TREE_MODEL(targets_model), &iter);
		g_free(esctext);
	}

	if (path)
		gtk_tree_view_set_cursor(GTK_TREE_VIEW(targets_view),
					 path, NULL, FALSE);
}

static void insert_results(GList *list)
{
	GList *entry;
	struct latency_line *line;
	GtkTreeIter iter;
	gchar *reasonstr;
	int l;

	entry = g_list_first(list);
	while(entry) {
		char maxstr[32];
		char percentstr[32];

		line = entry->data;
		entry = g_list_next(entry);

		/* Skip too small values */
		if (line->max * 0.001 < 0.1) 
			continue;

		/* Remove crap \n at end of line */
		reasonstr = g_strdup(line->reason);
		l = strlen(reasonstr);
		while(l) {
			if (reasonstr[--l] != '\n')
				break;
			reasonstr[l] = 0;
		}

		/* Store it all into the model */
		gtk_list_store_append(GTK_LIST_STORE(results_model), &iter);
		sprintf(maxstr, "%.1f ms", line->max * 0.001);
		sprintf(percentstr, "%.1f %%", (line->time * 100 + 0.0001) / total_time);
		gtk_list_store_set(GTK_LIST_STORE(results_model),
				   &iter,
				   COL_R_CAUSE, reasonstr,
				   COL_R_MAX, maxstr,
				   COL_R_PERCENT, percentstr,
				   COL_R_LINE, line,
				   -1);
		g_free(reasonstr);
	}
}

static void insert_backtrace(struct latency_line *line)
{
	GtkTreeIter iter;
	char *c = line->backtrace;
	char *c2;
	char buffer[1024];

	while (c && *c) {
		while (*c == ' ')
			c++;
		if (!*c)
			break;
		c2 = strchr(c, ' ');
		if (c2) {
			int l = MIN(c2-c, 1023);
			strncpy(buffer, c, l);
			buffer[l] = 0;
		} else {
			strncpy(buffer, c, 1023);
			buffer[1023] = 0;
		}
		c = c2;
		gtk_list_store_append(GTK_LIST_STORE(backtrace_model), &iter);
		gtk_list_store_set(GTK_LIST_STORE(backtrace_model),
				   &iter,
				   COL_B_FUNC, buffer,
				   -1);
	}
	gtk_tree_view_columns_autosize(GTK_TREE_VIEW(backtrace_view));
}

static gint target_select(GtkTreeSelection *sel, void *data)
{
	GtkTreeModel		*model;
	GtkTreeIter		iter;
	struct process		*proc = NULL;

	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(targets_view));
	if (gtk_tree_selection_get_selected(sel, &model, &iter))
		gtk_tree_model_get(model, &iter, COL_T_PROC, &proc, -1);

	if (ignore_sel)
		return TRUE;

	if (proc && (force_results || proc->pid != cur_target_pid)) {
		cur_target_pid = proc->pid;
		remove_all_results();
		insert_results(proc->latencies);
	} else if (!proc && (force_results || cur_target_pid != 0)) {
		cur_target_pid = 0;
		remove_all_results();
		insert_results(lines);
	}

	/* We don't need to force updates anymore */
	force_results = 0;

	return FALSE;
}

static gint backtrace_select(GtkTreeSelection *sel, void *data)
{
	GtkTreeModel		*model;
	GtkTreeIter		iter;
	struct latency_line	*line = NULL;

	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(results_view));
	if (gtk_tree_selection_get_selected(sel, &model, &iter))
		gtk_tree_model_get(model, &iter, COL_R_LINE, &line, -1);
	if (ignore_sel)
		return TRUE;

	gtk_list_store_clear(backtrace_model);
	if (line)
		insert_backtrace(line);

	return TRUE;
}

static void do_refresh(void)
{
	/* Ignore selection messages triggerd by the list during those updates */
	ignore_sel = TRUE;

	/* Empty list views */
	remove_all_targets();
	remove_all_results();

	/* Call core to update the glists */
	update_list();

	/* Stop ignoring selection changes */
	ignore_sel = FALSE;

	/* Insert all targets again */
	insert_all_targets();

	/* Make them look good */
	gtk_tree_view_columns_autosize(GTK_TREE_VIEW(targets_view));
	gtk_tree_view_columns_autosize(GTK_TREE_VIEW(results_view));
}

static void update_countdown(void)
{
	char buffer[128];

	sprintf(buffer, "Refresh in %d s", countdown);
	gtk_label_set_text(GTK_LABEL(countdown_label), buffer);
}

static gint timer_tick(void *data)
{
	/* Maybe we should stop the timer instead .. oh well, Arjan will kill me :-) */
	if (countdown_frozen)
		return TRUE;

	countdown--;
	if (countdown <= 0) {
		countdown = countdown_max;
		do_refresh();
	}
	update_countdown();

	return TRUE;
}

static void refresh_clicked(GtkButton *btn, void *data)
{
	do_refresh();
	countdown = countdown_max;
	update_countdown();
}

static void freeze_toggled(GtkToggleButton *btn, void *data)
{
	gboolean state = gtk_toggle_button_get_active(btn);

	countdown_frozen = state;	
}

static GtkWidget *create_targets_list(void)
{
	GtkWidget *scrollw;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *sel;

	/* We Create a scrolled window with a treeview to hold the target
	 * list. Note that a lot of this is straight from the GTK tutorial :-)
	 */
	scrollw = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollw),
				       GTK_POLICY_AUTOMATIC, 
				       GTK_POLICY_AUTOMATIC);
   
	/* Create a tree view object */
	targets_view = gtk_tree_view_new();
	gtk_container_add(GTK_CONTAINER(scrollw), targets_view);

	/* We use a simple list store for 4 colums, one is not displayed */
	targets_model = gtk_list_store_new(4,
					   GDK_TYPE_PIXBUF,
					   G_TYPE_STRING,
					   G_TYPE_STRING,
					   G_TYPE_POINTER);
	gtk_tree_view_set_model(GTK_TREE_VIEW(targets_view),
				GTK_TREE_MODEL(targets_model));

	/* Create the column with icon & text */
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Targets");
	renderer = gtk_cell_renderer_pixbuf_new();
	gtk_tree_view_column_pack_start(column, renderer, FALSE);
	gtk_tree_view_column_set_attributes(column, renderer,
					    "pixbuf", COL_T_ICON,
					    NULL);
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer,
					    "markup", COL_T_NAME,
					    NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(targets_view),
				    GTK_TREE_VIEW_COLUMN(column));

	/* Add a column for the max latency */
	renderer = gtk_cell_renderer_text_new();
	g_object_set(G_OBJECT(renderer),
		     "xalign", 1.0, 
		     NULL);
	column = gtk_tree_view_column_new_with_attributes("Max",
							  renderer,
							  "text", COL_T_MAX,
							  NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(targets_view),
				    GTK_TREE_VIEW_COLUMN(column));

	/* Set selection mode to single */
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(targets_view));
	gtk_tree_selection_set_mode(sel, GTK_SELECTION_SINGLE);

	/* Hookup selection changed message */
	g_signal_connect(G_OBJECT(sel), "changed",
			 G_CALLBACK(target_select), NULL);
	return scrollw;
}

static GtkWidget *create_results_list(void)
{
	GtkWidget *scrollw;
	GtkCellRenderer *cell;
	GtkTreeViewColumn *column;
	GtkTreeSelection *sel;

	/* We Create a scrolled window with a treeview to hold the target
	 * list. Note that a lot of this is straight from the GTK tutorial :-)
	 */
	scrollw = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrollw),
				       GTK_POLICY_AUTOMATIC, 
				       GTK_POLICY_AUTOMATIC);
   
	/* Create a tree view object */
	results_view = gtk_tree_view_new();
	gtk_container_add(GTK_CONTAINER(scrollw), results_view);

	/* We use a simple list store for 4 columns, one not visible */
	results_model = gtk_list_store_new(4,
					   G_TYPE_STRING,
					   G_TYPE_STRING,
					   G_TYPE_STRING,
					   G_TYPE_POINTER);
	gtk_tree_view_set_model(GTK_TREE_VIEW(results_view),
				GTK_TREE_MODEL(results_model));



	/* Add columns */
	cell = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("Cause",
							   cell,
							   "text", COL_R_CAUSE,
							   NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(results_view),
				    GTK_TREE_VIEW_COLUMN(column));

	cell = gtk_cell_renderer_text_new ();
	g_object_set(G_OBJECT(cell),
		     "xalign", 1.0, 
		     NULL);
	column = gtk_tree_view_column_new_with_attributes ("Maximum",
							   cell,
							   "text", COL_R_MAX,
							   NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(results_view),
				    GTK_TREE_VIEW_COLUMN(column));

	cell = gtk_cell_renderer_text_new ();
	g_object_set(G_OBJECT(cell),
		     "xalign", 1.0, 
		     NULL);
	column = gtk_tree_view_column_new_with_attributes ("Percentage",
							   cell,
							   "text", COL_R_PERCENT,
							   NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(results_view),
				    GTK_TREE_VIEW_COLUMN(column));


	/* Set selection mode to single */
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(results_view));
	gtk_tree_selection_set_mode(sel, GTK_SELECTION_SINGLE);

	/* Hookup selection changed message */
	g_signal_connect(G_OBJECT(sel), "changed",
			 G_CALLBACK(backtrace_select), NULL);
	return scrollw;
}

static GtkWidget *create_backtrace_list(void)
{
	GtkWidget *scrollw;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *sel;

	/* We Create a scrolled window with a treeview to hold the target
	 * list. Note that a lot of this is straight from the GTK tutorial :-)
	 */
	scrollw = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrollw),
				       GTK_POLICY_AUTOMATIC, 
				       GTK_POLICY_AUTOMATIC);
   
	/* Create a tree view object */
	backtrace_view = gtk_tree_view_new();
	gtk_container_add(GTK_CONTAINER(scrollw), backtrace_view);

	/* We use a simple list store */
	backtrace_model = gtk_list_store_new(1, G_TYPE_STRING);
	gtk_tree_view_set_model(GTK_TREE_VIEW(backtrace_view),
				GTK_TREE_MODEL(backtrace_model));



	/* Add columns */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("Backtrace",
							   renderer,
							   "text", COL_B_FUNC,
							   NULL);
	gtk_tree_view_append_column(GTK_TREE_VIEW(backtrace_view),
				    GTK_TREE_VIEW_COLUMN(column));

	/* Set selection mode to single */
	sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(backtrace_view));
	gtk_tree_selection_set_mode(sel, GTK_SELECTION_SINGLE);
	return scrollw;
}

static void create_targets_window(void)
{
	GtkWidget *w, *paned, *paned2, *vbox, *hbox, *btn;

	/* We need a window first for the process list */
	main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(main_window), "LatencyTOP "VERSION);
	g_signal_connect(G_OBJECT(main_window), "destroy",
			 G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(main_window), 5);
	gtk_widget_set_size_request(main_window, 600, 400);

	/* Split top container vs. buttons etc... at the bottom */
	vbox = gtk_vbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(main_window), vbox);

	/* Split it in two with an hpaned */
	paned = gtk_hpaned_new();
	gtk_box_pack_start(GTK_BOX(vbox), paned, TRUE, TRUE, 0);

	/* Create the target list (left pane) */
	w = create_targets_list();
	gtk_paned_add1(GTK_PANED(paned), w);

	/* Split the right pane between results and backtrace */
	paned2 = gtk_vpaned_new();
	gtk_paned_add2(GTK_PANED(paned), paned2);
	
	/* Create the results list (right/top pane) */
	w = create_results_list();
	gtk_paned_add1(GTK_PANED(paned2), w);

	/* Create the backtrace list (right/bottom pane) */
	w = create_backtrace_list();
	gtk_paned_add2(GTK_PANED(paned2), w);

	/* The panner seems to default to a stupid position, I'm not sure
	 * yet what's the right way of getting it to automagically adjust
	 * to the size of the list items, so let's whack it for now to
	 * some arbitrary value that looks better
	 */
	gtk_paned_set_position(GTK_PANED(paned), 200);
	gtk_paned_set_position(GTK_PANED(paned2), 250);

	/* Now create an hbox at the botton for various controls, etc... */
	hbox = gtk_hbox_new(FALSE, 5);
	gtk_box_pack_end(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	/* Add the countdown */
	countdown_label = gtk_label_new("");
	gtk_box_pack_end(GTK_BOX(hbox), countdown_label, FALSE, FALSE, 0);
	btn = gtk_button_new_with_label("Refresh");
	gtk_box_pack_end(GTK_BOX(hbox), btn, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(btn), "clicked",
			 G_CALLBACK(refresh_clicked), NULL);
	freeze_button = gtk_toggle_button_new_with_label("Freeze");
	gtk_box_pack_end(GTK_BOX(hbox), freeze_button, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(freeze_button), "toggled",
			 G_CALLBACK(freeze_toggled), NULL);
}

static load_icons(void)
{
	GError *err;

	/* Hackish ... needs fixing obviously :-)
	 *
	 * We scale the icons to 18x18 because the ones I randomly grabbed
	 * don't look good with my default font :-) Ideally, we should
	 * get the font metrics yadada yadada...
	 */
#define ICON_SIZE	18
#define PATH_PREFIX	"/usr/share/latencytop/"

	err = NULL;
	global_icon = gdk_pixbuf_new_from_file_at_size(PATH_PREFIX "global.png",
						       ICON_SIZE, ICON_SIZE,
						       &err);
	if (!global_icon) {
		err = NULL;
		global_icon = gdk_pixbuf_new_from_file_at_size("global.png",
							       ICON_SIZE, ICON_SIZE,
							       &err);
	}
	err = NULL;
	kthread_icon = gdk_pixbuf_new_from_file_at_size(PATH_PREFIX "kernel_thread.png",
							ICON_SIZE, ICON_SIZE,
							&err);
	if (!kthread_icon) {
		err = NULL;
		kthread_icon = gdk_pixbuf_new_from_file_at_size("kernel_thread.png",
								ICON_SIZE, ICON_SIZE,
								&err);
	}
	err = NULL;
	process_icon = gdk_pixbuf_new_from_file_at_size(PATH_PREFIX "user_process.png",
							ICON_SIZE, ICON_SIZE, &err);
	if (!process_icon) {
		err = NULL;
		process_icon = gdk_pixbuf_new_from_file_at_size("user_process.png",
								ICON_SIZE, ICON_SIZE,
								&err);
	}
}

void start_gtk_ui(void) 
{
	load_icons();
	create_targets_window();

	/* Initialize the lists */
	update_list();
	insert_all_targets();

	/* Make them look good */
	gtk_tree_view_columns_autosize(GTK_TREE_VIEW(targets_view));
	gtk_tree_view_columns_autosize(GTK_TREE_VIEW(results_view));

	/* Show everything */
	gtk_widget_show_all(main_window);

	/* Start the timer */
	countdown = countdown_max;
	update_countdown();
	g_timeout_add(1000, timer_tick, NULL);

	/* Run the main event loop */
	gtk_main();
}

int preinitialize_gtk_ui(int *argc, char ***argv)
{
	int rc, i;

	rc = gtk_init_check(argc, argv);
	if (!rc)
		return FALSE;

	/* Hack... will do for now */
	for (i = 1; i < *argc; i++)		
		if (strcmp((*argv)[i], "--nogui") == 0)
			return FALSE;
	return TRUE;
}

