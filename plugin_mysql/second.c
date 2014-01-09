#include <unistd.h>
#include <stdio.h>
#include <plugin.h>
#include <mysql_version.h>

long long system_var = 0;
long long status_var = 0;

//plugin.h
//SHOW_VAR com_status_vars[]
struct st_mysql_show_var vars_status_var[] =
{
	{"vars_status_var", (char *) &status_var, SHOW_LONGLONG},
	{0, 0, 0}
};

//sql/sql_plugin.h
// (name, varname, opt, comment, check, update, def, min, max, blk)
//struct system_variables

int sysvar_check(MYSQL_THD thd, struct st_mysql_sys_var *var,
	void* save, struct st_mysql_value *value)
{
	long long buf;
	value->val_int(value, &buf);
	*(long long*) save = buf;

	return 0;
}

void sysvar_update(MYSQL_THD thd, struct st_mysql_sys_var *var,
	void* var_ptr, const void* save)
{
	system_var = *(long long *)save;
	status_var = system_var;
}
// mysql_sysvar_vars_system
MYSQL_SYSVAR_LONGLONG(vars_system, system_var, 0, "a demo", 
	sysvar_check, sysvar_update, 0, 0,
	123, 0);


struct st_mysql_sys_var* vars_system_var[] = 
{
	MYSQL_SYSVAR(vars_system), NULL 
};

//plugin descriptor

struct st_mysql_daemon var_plugin_info = 
{
	MYSQL_DAEMON_INTERFACE_VERSION
};

// plugin declaration

static int plugin_hoterran2_init(void* p)
{
	fprintf(stderr, "plugin_hoterran2_init\n");
	return 0;
}

static int plugin_hoterran2_deinit(void* p)
{
	fprintf(stderr, "plugin_hoterran2_deinit\n");
	return 0;
}

mysql_declare_plugin(hoterran)
{	
	MYSQL_DAEMON_PLUGIN,
	&var_plugin_info,
	"hoterran2",
	"hoterran2",
	"test",
	PLUGIN_LICENSE_BSD,
	plugin_hoterran2_init,
	plugin_hoterran2_deinit,
	0x0001,
	vars_status_var,
	vars_system_var,
	NULL
}

mysql_declare_plugin_end;
