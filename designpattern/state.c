#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

struct file_stat_t;

struct file_state_t {
    void (*file_state_func)(struct file_state_t *this, const char *filename, void *arg);
};

struct file_state_t *_fs = NULL;

void dump_untracked_file_state(struct file_state_t *this, const char *filename, void *arg) {
    printf("1 %s\n", filename);
}

void dump_staged_file_state(struct file_state_t *this, const char *filename, void *arg) {
    printf("2 %s\n", filename);
}

void dump_modified_file_state(struct file_state_t *this, const char *filename, void *arg) {
    printf("3 %s\n", filename);
}

void dump_committed_file_state(struct file_state_t *this, const char *filename, void *arg) {
    printf("4 %s\n", filename);
}

struct file_state_t* file_state_instance() {
    if (!_fs) {
        _fs = malloc(sizeof(*_fs));
        memset(_fs, 0, sizeof(*_fs));
        _fs->file_state_func = dump_untracked_file_state;
    }
}

void untracked_file_state_destroy() {
    if (_fs)
        free(_fs);
    _fs =  NULL;
}

struct file_t {
    char filename[100];
    struct file_state_t *state;
};

static struct file_t* file_new(const char *filename) {
    struct file_t *f = (struct file_t*)malloc(sizeof(*f));
    if (!f) return NULL;

    memset(f, 0, sizeof(*f));
    strcpy(f->filename, filename);

    f->state = file_state_instance();
    if (!f->state) {
        free(f);
        return NULL;
    }

    return f;
}

static void file_status(struct file_t *f) {
    f->state->file_state_func(f->state, f->filename, NULL);
}

static void file_add(struct file_t *f) {
    f->state->file_state_func  = dump_staged_file_state;
}

static void file_commit(struct file_t *f) {
    f->state->file_state_func = dump_committed_file_state;
}

static void file_modified(struct file_t *f) {
    f->state->file_state_func = dump_modified_file_state;
}

int main(int argc, const char *argv[])
{
    struct file_t *f = file_new("foo.txt");
    file_status(f);

    file_add(f);
    file_status(f);

    file_commit(f);
    file_status(f);

    file_modified(f);
    file_status(f);

    file_commit(f);
    file_status(f);

    return 0;
}

