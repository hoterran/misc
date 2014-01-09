#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zookeeper/zookeeper.h>
#include <zookeeper/zookeeper_log.h>

void watcher_g(zhandle_t* zh, int type, int state,
        const char* path, void* watcherCtx)
{
    if (type == ZOO_SESSION_EVENT) {
        if (state == ZOO_CONNECTED_STATE) {
            printf(" Connected to zookeeper service successfully!\n");
        } else if (state == ZOO_EXPIRED_SESSION_STATE) { 
            printf("Zookeeper session expired!\n");
        }
    }  
}

void completion(int rc, const char *name, const void *data)
{
    fprintf(stderr, "[%s]: rc = %d\n", (char*)(data==0?"null":data), rc);

    if (!rc) {
        //successfully 
        fprintf(stderr, "\tname = %s\n", name);
    }
}

void accept_query()
{
    printf("server is running...\n");
}

int main(int argc, const char *argv[])
{

    const char* host = "127.0.0.1:2181";
    char *data = "alive";

    //const char* host = "127.0.0.1:2181,127.0.0.1:2182,127.0.0.1:2183,127.0.0.1:2184,127.0.0.1:2185";
    int timeout = 30000;
    
    zoo_set_debug_level(ZOO_LOG_LEVEL_WARN);
    zhandle_t* zkhandle = zookeeper_init(host,
            watcher_g, timeout, 0, "hello zookeeper.", 0);
    if (zkhandle == NULL) {
        fprintf(stderr, "Error when connecting to zookeeper servers...\n");
        exit(EXIT_FAILURE);
    }

    int ret = zoo_acreate(zkhandle, "/server", data, strlen(data),
           &ZOO_OPEN_ACL_UNSAFE, ZOO_EPHEMERAL,
           completion, "zoo_acreate");
    if (ret) {
        fprintf(stderr, "Error %d for %s\n", ret, "acreate");
        exit(EXIT_FAILURE);
    }

    do {
        //sleep 5, then down
        accept_query();
        sleep(5);
    } while(false);

    zookeeper_close(zkhandle);
}
