#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <plugin.h>
#include <mysql_version.h>
#include <my_global.h>
#include <my_sys.h>
#include <pthread.h>

#define MONITORING_BUFFER 1024

//sql/mysqld.cc
extern ulong	thread_id;
extern uint	thread_count;
extern ulong	max_connections;

static pthread_t monitoring_thread;
static int monitoring_file;		//fd

//func

pthread_handler_t monitoring(void *p)
{
	char buffer[MONITORING_BUFFER];
	char time_str[20];
	while(1) {
		sleep(5);
		get_date(time_str, GETDATE_DATE_TIME, 0);
		snprintf(buffer, sizeof(buffer), "%s: %u of %lu  clients connected, "
				"%lu connections made\n",
				time_str, thread_count,
				max_connections, thread_id);
		write(monitoring_file, buffer, strlen(buffer));
	}
}

static int monitor_plugin_init(void *p) 
{
	pthread_attr_t attr;
	char monitoring_filename[FN_REFLEN];
	char buffer[MONITORING_BUFFER];
	char time_str[20];

	fn_format(monitoring_filename, "monitor", "", ".log",
		MY_REPLACE_EXT | MY_UNPACK_FILENAME);

	unlink(monitoring_filename);
	monitoring_file = open(monitoring_filename, O_CREAT | O_RDWR, 0644);

	if (monitoring_file < 0) {
		fprintf(stderr, "Plugin 'monitor cant create %s", monitoring_filename);	
		return 1;	
	}

	get_date(time_str, GETDATE_DATE_TIME, 0);
	snprintf(buffer, sizeof(buffer), "Monitor started at %s\n", time_str);
	write(monitoring_file, buffer, strlen(buffer));

	pthread_attr_init(&attr);

	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	if (pthread_create(&monitoring_thread, &attr, monitoring, NULL) != 0) {
		fprintf(stderr, "create thread failure");
		return 1;
	}
	
	return 0;
}

static int monitor_plugin_deinit(void *p)
{
	char time_str[20];
	char buffer[MONITORING_BUFFER];
	pthread_cancel(monitoring_thread);
	pthread_join(monitoring_thread, NULL);
	get_date(time_str, GETDATE_DATE_TIME, 0);
	snprintf(buffer, sizeof(buffer), "monitor %s\n", time_str);
	write(monitoring_file, buffer, strlen(buffer));
	close(monitoring_file);
}

struct st_mysql_daemon monitor_plugin = 
{
	MYSQL_DAEMON_INTERFACE_VERSION
};

mysql_declare_plugin(monitoring)
{
	MYSQL_DAEMON_PLUGIN,
	&monitor_plugin,
	"monitoring",
	"hoterran",
	"saaaa",
	PLUGIN_LICENSE_GPL,
	monitor_plugin_init,
	monitor_plugin_deinit,
	0x0100,
	NULL,
	NULL,
	NULL,
}
mysql_declare_plugin_end;

