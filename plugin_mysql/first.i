# 1 "first.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "first.c"
# 1 "/usr/local/mysql/include/mysql/plugin.h" 1
# 39 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_lex_string
{
  char *str;
  unsigned int length;
};

typedef struct st_mysql_lex_string MYSQL_LEX_STRING;
# 56 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_xid {
  long formatID;
  long gtrid_length;
  long bqual_length;
  char data[128];
};
typedef struct st_mysql_xid MYSQL_XID;
# 119 "/usr/local/mysql/include/mysql/plugin.h"
enum enum_mysql_show_type
{
  SHOW_UNDEF, SHOW_BOOL, SHOW_INT, SHOW_LONG,
  SHOW_LONGLONG, SHOW_CHAR, SHOW_CHAR_PTR,
  SHOW_ARRAY, SHOW_FUNC, SHOW_DOUBLE
};

struct st_mysql_show_var {
  const char *name;
  char *value;
  enum enum_mysql_show_type type;
};


typedef int (*mysql_show_var_func)(void*, struct st_mysql_show_var*, char *);
# 158 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_sys_var;
struct st_mysql_value;
# 180 "/usr/local/mysql/include/mysql/plugin.h"
typedef int (*mysql_var_check_func)(void* thd,
                                    struct st_mysql_sys_var *var,
                                    void *save, struct st_mysql_value *value);
# 198 "/usr/local/mysql/include/mysql/plugin.h"
typedef void (*mysql_var_update_func)(void* thd,
                                      struct st_mysql_sys_var *var,
                                      void *var_ptr, const void *save);
# 394 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_plugin
{
  int type;
  void *info;
  const char *name;
  const char *author;
  const char *descr;
  int license;
  int (*init)(void *);
  int (*deinit)(void *);
  unsigned int version;
  struct st_mysql_show_var *status_vars;
  struct st_mysql_sys_var **system_vars;
  void * __reserved1;
};
# 417 "/usr/local/mysql/include/mysql/plugin.h"
enum enum_ftparser_mode
{
# 427 "/usr/local/mysql/include/mysql/plugin.h"
  MYSQL_FTPARSER_SIMPLE_MODE= 0,
# 438 "/usr/local/mysql/include/mysql/plugin.h"
  MYSQL_FTPARSER_WITH_STOPWORDS= 1,
# 451 "/usr/local/mysql/include/mysql/plugin.h"
  MYSQL_FTPARSER_FULL_BOOLEAN_INFO= 2
};
# 465 "/usr/local/mysql/include/mysql/plugin.h"
enum enum_ft_token_type
{
  FT_TOKEN_EOF= 0,
  FT_TOKEN_WORD= 1,
  FT_TOKEN_LEFT_PAREN= 2,
  FT_TOKEN_RIGHT_PAREN= 3,
  FT_TOKEN_STOPWORD= 4
};
# 505 "/usr/local/mysql/include/mysql/plugin.h"
typedef struct st_mysql_ftparser_boolean_info
{
  enum enum_ft_token_type type;
  int yesno;
  int weight_adjust;
  char wasign;
  char trunc;

  char prev;
  char *quot;
} MYSQL_FTPARSER_BOOLEAN_INFO;
# 567 "/usr/local/mysql/include/mysql/plugin.h"
typedef struct st_mysql_ftparser_param
{
  int (*mysql_parse)(struct st_mysql_ftparser_param *,
                     char *doc, int doc_len);
  int (*mysql_add_word)(struct st_mysql_ftparser_param *,
                        char *word, int word_len,
                        MYSQL_FTPARSER_BOOLEAN_INFO *boolean_info);
  void *ftparser_state;
  void *mysql_ftparam;
  struct charset_info_st *cs;
  char *doc;
  int length;
  int flags;
  enum enum_ftparser_mode mode;
} MYSQL_FTPARSER_PARAM;
# 591 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_ftparser
{
  int interface_version;
  int (*parse)(MYSQL_FTPARSER_PARAM *param);
  int (*init)(MYSQL_FTPARSER_PARAM *param);
  int (*deinit)(MYSQL_FTPARSER_PARAM *param);
};
# 626 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_storage_engine
{
  int interface_version;
};

struct handlerton;






struct st_mysql_daemon
{
  int interface_version;
};






struct st_mysql_information_schema
{
  int interface_version;
};
# 668 "/usr/local/mysql/include/mysql/plugin.h"
struct st_mysql_value
{
  int (*value_type)(struct st_mysql_value *);
  const char *(*val_str)(struct st_mysql_value *, char *buffer, int *length);
  int (*val_real)(struct st_mysql_value *, double *realbuf);
  int (*val_int)(struct st_mysql_value *, long long *intbuf);
};
# 685 "/usr/local/mysql/include/mysql/plugin.h"
int thd_in_lock_tables(const void* thd);
int thd_tablespace_op(const void* thd);
long long thd_test_options(const void* thd, long long test_options);
int thd_sql_command(const void* thd);
const char *thd_proc_info(void* thd, const char *info);
void **thd_ha_data(const void* thd, const struct handlerton *hton);
int thd_tx_isolation(const void* thd);
char *thd_security_context(void* thd, char *buffer, unsigned int length,
                           unsigned int max_query_len);

void thd_inc_row_count(void* thd);
# 709 "/usr/local/mysql/include/mysql/plugin.h"
int mysql_tmpfile(const char *prefix);
# 725 "/usr/local/mysql/include/mysql/plugin.h"
int thd_killed(const void* thd);
# 734 "/usr/local/mysql/include/mysql/plugin.h"
unsigned long thd_get_thread_id(const void* thd);
# 749 "/usr/local/mysql/include/mysql/plugin.h"
void *thd_alloc(void* thd, unsigned int size);



void *thd_calloc(void* thd, unsigned int size);



char *thd_strdup(void* thd, const char *str);



char *thd_strmake(void* thd, const char *str, unsigned int size);



void *thd_memdup(void* thd, const void* str, unsigned int size);
# 780 "/usr/local/mysql/include/mysql/plugin.h"
MYSQL_LEX_STRING *thd_make_lex_string(void* thd, MYSQL_LEX_STRING *lex_str,
                                      const char *str, unsigned int size,
                                      int allocate_lex_string);







void thd_get_xid(const void* thd, MYSQL_XID *xid);
# 800 "/usr/local/mysql/include/mysql/plugin.h"
void mysql_query_cache_invalidate4(void* thd,
                                   const char *key, unsigned int key_length,
                                   int using_trx);
# 2 "first.c" 2

typedef struct
{
 const char *name;
 const char *author;
 const char *desc;
} plugin_info;

plugin_info plugin_hoterran = {
"plugin_test",
"hoterran",
"just a plugin test"
};

long number_of_calls = 0;

struct st_mysql_show_var hoterran_status[]=
{
  {"static", (char *)"just a static text", SHOW_CHAR},
  {"called", (char *)&number_of_calls, SHOW_LONG},
  {0,0,0}
};



int hoterran_desc = 0;



mysql_declare_plugin
{
 0,
 &hoterran_desc,
 plugin_hoterran.name,
 plugin_hoterran.author,
 plugin_hoterran.desc,
 2,
 plugin_hoterran_init,
 plugin_hoterran_deinit,
 0x0001,
 hoterran_status,
 NULL,
 NULL
}
,{0,0,0,0,0,0,0,0,0,0,0,0}};

static int plugin_hoterran_init(void)
{
 return 0;
}

static int plugin_hoterran_deinit(void)
{
 return 0;
}

int main()
{



}
