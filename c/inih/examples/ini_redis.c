/* Example: parse a simple configuration file */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../ini.h"

typedef struct
{
    const char* redis_server;
    const char* redis_password;
    unsigned int redis_port;
} configuration;

static int handler(void* user, const char* section, const char* name,
    const char* value) {
    configuration* pconfig = (configuration*)user;

    #define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
    if (MATCH("redis", "redis_server")) {
        pconfig->redis_server = strdup(value);
    } else if (MATCH("redis", "redis_port")) {
        pconfig->redis_port = atoi(value);
    } else if (MATCH("redis", "redis_password")) {
        pconfig->redis_password = strdup(value);
    } else {
        return 0;  /* unknown section/name, error */
    }
    return 1;
}

int main(int argc, char* argv[])
{
    configuration config;

    if (ini_parse("redis.ini", handler, &config) < 0) {
        printf("Can't load 'test.ini'\n");
        return 1;
    }
    printf("Config loaded from 'redis.ini': s=%s, p=%s, po=%u\n",
        config.redis_server, config.redis_password, config.redis_port);
    return 0;
}
