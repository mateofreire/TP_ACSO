#include "pathname.h"
#include "directory.h"
#include "inode.h"
#include "diskimg.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

int pathname_lookup(struct unixfilesystem *fs, const char *pathname) {
    if (pathname == NULL || pathname[0] != '/') {
        return -1; // Ruta no absoluta o inválida
    }

    if (strcmp(pathname, "/") == 0) {
        return 1; // Inodo raíz
    }

    char pathcopy[1024];
    strncpy(pathcopy, pathname, sizeof(pathcopy));
    pathcopy[sizeof(pathcopy) - 1] = '\0'; // Seguridad: null-terminación

    int inumber = 1; // Comenzamos en la raíz

    char *token = strtok(pathcopy, "/");
    while (token != NULL) {
        struct direntv6 dir_entry;

        if (directory_findname(fs, token, inumber, &dir_entry) < 0)
            return -1;

        inumber = dir_entry.d_inumber;
        token = strtok(NULL, "/");
    }

    return inumber;
}
