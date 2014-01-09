#include <string.h>
#include <errno.h>

#include <zookeeper/zookeeper.h>

static zhandle_t *zh;

/**
 * In this example this method gets the cert for your
 *   environment -- you must provide
 */
char *foo_get_cert_once(char* id) { return 0; }

/** Watcher function -- empty for this example, not something you should
 * do in real code */
void watcher(zhandle_t *zzh, int type, int state, const char *path,
             void *watcherCtx) {}

int main(int argc, char argv) {
  char buffer[512];
  char p[2048];
  char *cert=0;
  char appId[64];

  strcpy(appId, "example.foo_test");
  cert = foo_get_cert_once(appId);
  if(cert!=0) {
    fprintf(stderr,
            "Certificate for appid [%s] is [%s]\n",appId,cert);
    strncpy(p,cert, sizeof(p)-1);
    free(cert);
  } else {
    fprintf(stderr, "Certificate for appid [%s] not found\n",appId);
    strcpy(p, "dummy");
  }

  zoo_set_debug_level(ZOO_LOG_LEVEL_DEBUG);

  zh = zookeeper_init("127.0.0.1:2181", watcher, 10000, 0, 0, 0);
  if (!zh) {
    return errno;
  }
  if(zoo_add_auth(zh,"foo",p,strlen(p),0,0)!=ZOK)
    return 2;

  struct ACL CREATE_ONLY_ACL[] = {{ZOO_PERM_CREATE, ZOO_AUTH_IDS}};
  struct ACL_vector CREATE_ONLY = {1, CREATE_ONLY_ACL};
  int rc = zoo_create(zh,"/xyz","value", 5, &CREATE_ONLY, ZOO_EPHEMERAL,
                      buffer, sizeof(buffer)-1);

  /** this operation will fail with a ZNOAUTH error */
  int buflen= sizeof(buffer);
  struct Stat stat;
  rc = zoo_get(zh, "/xyz", 0, buffer, &buflen, &stat);
  if (rc) {
    fprintf(stderr, "Error %d for %d\n", rc, __LINE__);
  }

  zookeeper_close(zh);
  return 0;
}
