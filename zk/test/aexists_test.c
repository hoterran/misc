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
    if (type == ZOO_SESSION_EVENT) {
        if (state == ZOO_CONNECTED_STATE) {
            printf("Connected to zookeeper service successfully!\n");
        } else if (state == ZOO_EXPIRED_SESSION_STATE) { 
            printf("Zookeeper session expired!\n");
        }
    }
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
    printf("completion %d \n", rc);
}

int
main(int argc, const char *argv[])
{
    const char *host = "127.0.0.1:2181";
    int timeout = 30000;

    zoo_set_debug_level(ZOO_LOG_LEVEL_WARN);
    zhandle_t *zkhandle = zookeeper_init(host,
         watcher_global, timeout, 0, "watcher_global", 0);

    if (zkhandle == NULL) {
        fprintf(stderr, "Error when connecting to zookeeper servers...\n");
        exit(EXIT_FAILURE);
    }

    int ret = zoo_aexists(zkhandle, "/server", 1, completion, "aaa");
    
    if (ret) { fprintf(stderr, "Error %d for %s\n", ret, "aexists");
    }

    getchar(); //hold 

    zookeeper_close(zkhandle);

    return 0;
}
