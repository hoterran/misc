#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <zookeeper/zookeeper.h>
#include <zookeeper/zookeeper_log.h>

void QueryServerd_watcher_global(zhandle_t * zh, int type, int state,
                            const char *path, void *watcherCtx);
void QueryServerd_stat_completion(int rc, const struct Stat *stat,
                             const void *data);
void QueryServerd_watcher_awexists(zhandle_t *zh, int type, int state,
                              const char *path, void *watcherCtx);
static void QueryServerd_awexists(zhandle_t *zh);

void
QueryServerd_watcher_global(zhandle_t * zh, int type, int state,
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
QueryServerd_dump_stat(const struct Stat *stat)
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
QueryServerd_stat_completion(int rc, const struct Stat *stat,
                             const void *data)
{
}

void
QueryServerd_watcher_awexists(zhandle_t *zh, int type, int state,
                              const char *path, void *watcherCtx)
{
    if (state == ZOO_CONNECTED_STATE) {
        if (type == ZOO_DELETED_EVENT) {
            printf("QueryServer gone away, restart now...\n");
            // re-exists and set watch on /QueryServer again.
            QueryServerd_awexists(zh);
            pid_t pid = fork();
            if (pid < 0) {
                fprintf(stderr, "Error when doing fork.\n");
                exit(EXIT_FAILURE);
            }
            if (pid == 0) { 
                // start 
                execl("./server", "server", NULL);
                exit(EXIT_SUCCESS);
            }
            sleep(1); /* sleep 1 second for purpose. */
        } else if (type == ZOO_CREATED_EVENT) {
            printf("QueryServer started...\n");
        }
    }

    // re-exists and set watch on /QueryServer again.
    QueryServerd_awexists(zh);
}

static void
QueryServerd_awexists(zhandle_t *zh)
{
    int ret =
        zoo_awexists(zh, "/QueryServer",
                     QueryServerd_watcher_awexists,
                     "QueryServerd_awexists.",
                     QueryServerd_stat_completion,
                     "zoo_awexists");
    if (ret) {
        fprintf(stderr, "Error %d for %s\n", ret, "aexists");
        exit(EXIT_FAILURE);
    }
}

int
main(int argc, const char *argv[])
{
    const char *host = "127.0.0.1:2181";
    int timeout = 30000;

    zoo_set_debug_level(ZOO_LOG_LEVEL_WARN);
    zhandle_t *zkhandle = zookeeper_init(host,
                                         QueryServerd_watcher_global,
                                         timeout,
                                         0, "QueryServerd", 0);
    if (zkhandle == NULL) {
        fprintf(stderr, "Error when connecting to zookeeper servers...\n");
        exit(EXIT_FAILURE);
    }

    QueryServerd_awexists(zkhandle);
    // Wait for asynchronous zookeeper call done.
    getchar();

    zookeeper_close(zkhandle);

    return 0;
}
