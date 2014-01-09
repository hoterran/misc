#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <plugin.h>
#include <mysql_version.h>
#include <my_global.h>
#include <my_sys.h>
#include <pthread.h>


static int get_value(MYSQL_THD thd, struct st_mysql_show_var* var, char *buff) {
	struct st_mysql_show_var *status = (struct st_mysql_show_var*) buff;

	var->type = SHOW_ARRAY;
	var->value= (char* )status;
	
	status->value = "aaa";
	status->type = SHOW_CHAR;
	status->name = "test2";
	status++;

	status->name = 0;

	return 0;
}

struct st_mysql_daemon sys_status = 
{
	MYSQL_DAEMON_INTERFACE_VERSION
};

static struct st_mysql_show_var sys_status_var[] = 
{
	{"plugin_test", (char *)&get_value, SHOW_FUNC},
	{0, 0, 0} 
};

mysql_declare_plugin(sys_status)
{
	MYSQL_DAEMON_PLUGIN,
	&sys_status,
	"sys_status",
	"hoterran",
	"test",
	PLUGIN_LICENSE_GPL,
	NULL,	
	NULL,	
	0x0100,
	sys_status_var,
	NULL,
	NULL,
}
mysql_declare_plugin_end;

