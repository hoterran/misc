#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <zookeeper/zookeeper.h>
#include <zookeeper/zookeeper_log.h>

void
watcher_global(zhandle_t * zh, int type, int state,
                            const char *path, void *watcherCtx)
{
    printf("%s-%d-%d\n", path, type, state);

}

static void
dump_stat(const struct Stat *stat)
{
    char tctimes[40];
    char tmtimes[40];
    time_t tctime;
    time_t tmtime;

    if (!stat) {
        fprintf(stderr, "null\n");
        return;
    }
    tctime = stat->ctime / 1000;
    tmtime = stat->mtime / 1000;

    ctime_r(&tmtime, tmtimes);
    ctime_r(&tctime, tctimes);

    fprintf(stderr, "\tctime = %s\tczxid=%llx\n"
            "\tmtime=%s\tmzxid=%llx\n"
            "\tversion=%x\taversion=%x\n"
            "\tephemeralOwner = %llx\n",
            tctimes, stat->czxid,
            tmtimes, stat->mzxid,
            (unsigned int) stat->version, (unsigned int) stat->aversion,
            stat->ephemeralOwner);
}

void
completion(int rc, const struct Stat *stat,
                             const void *data)
{
    printf("completion\n");
}

struct Stat s;

void
watcher_wexists(zhandle_t *zh, int type, int state,
                              const char *path, void *watcherCtx)
{
    if (state == ZOO_CONNECTED_STATE) {
        if (type == ZOO_DELETED_EVENT) {
            printf(" delete \n");
        } else if (type == ZOO_CREATED_EVENT) {
            printf(" create \n");
        }
    }

    int ret = zoo_wexists(zh, "/server", watcher_wexists, 
        "watch_wexists", &s);
}
void
get_watcher(zhandle_t *zh, int type, int state,
                              const char *path, void *watcherCtx)
{
    if (type == ZOO_CHANGED_EVENT) {
        if (state == ZOO_CONNECTED_STATE) {
            zhandle_t *zk =  (zhandle_t*) watcherCtx;

            printf("%s %d %d\n", path, state, type);

            char buff[100];
            int len = sizeof(buff);
            struct Stat s;
            int ret;

            ret = zoo_wget(zk, "/data1", get_watcher, zk, buff, &len, &s);

            char *str = strndup(buff, len);
            if (!ret) { 
                printf("zoo_wget %s\n", str); 
            }
        }
    }
}

int
main(int argc, const char *argv[])
{
    const char *host = "127.0.0.1:2181";
    int timeout = 30000;

    zoo_set_debug_level(ZOO_LOG_LEVEL_WARN);
    zhandle_t *zk = zookeeper_init(host,
         watcher_global, timeout, 0, "watcher_global", 0);

    if (zk == NULL) {
        fprintf(stderr, "Error when connecting to zookeeper servers...\n");
        exit(EXIT_FAILURE);
    }

    char buff[100];
    int len = sizeof(buff);
    struct Stat s;

    int ret;
    char *data = "test";
    int dataLen = strlen(data);

    ret = zoo_exists(zk, "/data1", 0, &s);
    
    if (!ret) {
        /* yes */
        printf("exists %d %d\n", s.version, s.dataLength); 
        ret = zoo_delete(zk, "/data1", s.version);

        if (ret) { 
            printf("zoo_delete failure %d\n", ret);
        }
    }

    ret = zoo_create(zk, "/data1", data, dataLen, &ZOO_OPEN_ACL_UNSAFE, 0, buff, len - 1);

    if (!ret) {
        printf("zoo_create {%s} \n", buff); 
    } else {
        printf("zoo_create failure {%d} \n", ret); 
    }

    // call watch_global
    ret = zoo_get(zk, "/data1", 1, buff, &len, &s);

    getchar();

    char *str = strndup(buff, len);
    if (!ret) { 
        printf("zoo_get %s\n", str); 
    }

    ret = zoo_wget(zk, "/data1", get_watcher, zk, buff, &len, &s);

    str = strndup(buff, len);
    if (!ret) { 
        printf("zoo_wget %s\n", str); 
    }

    getchar();

    zookeeper_close(zk);

    return 0;
}
