#define VERSION "0.5"

struct latency_line;

struct latency_line {
	char reason[1024];
	char backtrace[4096];
	int count;
	double time;
	double max;
};

struct process {
	unsigned int pid;
	int kernelthread;
	char name[64];
	unsigned int max;
	GList *latencies;
	int used;
	int exists;
	/* scheduler stats */
        double maxdelay;
        double totaldelay;
        int delaycount;
};

extern GList *lines;
extern GList *procs;
extern GList *translations;
extern int total_time;
extern int total_count;

extern unsigned int pid_with_max;
extern unsigned int pidmax;
extern int noui;
extern int dump_unknown;
extern char *prefered_process;

#define PT_COLOR_DEFAULT    1
#define PT_COLOR_HEADER_BAR 2
#define PT_COLOR_ERROR      3
#define PT_COLOR_RED        4
#define PT_COLOR_YELLOW     5
#define PT_COLOR_GREEN      6
#define PT_COLOR_BRIGHT     7

extern int preinitialize_gtk_ui(int *argc, char ***argv);
extern void start_gtk_ui(void);

extern void preinitialize_text_ui(int *argc, char ***argv);
extern void start_text_ui(void);

extern char *translate(char *line);
extern void init_translations(char *filename);
extern int fsync_display(int duration);
extern int enable_fsync_tracer(void);
extern int disable_fsync_tracer(void);
extern void update_list(void);
