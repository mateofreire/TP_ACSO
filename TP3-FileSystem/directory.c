#include "directory.h"
#include "inode.h"
#include "diskimg.h"
#include "file.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#define DIR_NAME_SIZE 14

int directory_findname(struct unixfilesystem *fs, const char *name, int dirinumber, struct direntv6 *dirEnt) {
    struct inode dir_inode;

    // 1. Obtener el inodo del directorio
    if (inode_iget(fs, dirinumber, &dir_inode) < 0) return -1;

    // 2. Validar que sea un directorio asignado
    if (!(dir_inode.i_mode & IALLOC) || (dir_inode.i_mode & IFMT) != IFDIR) return -1;

    // 3. Calcular la cantidad de bloques del directorio
    int dir_size = inode_getsize(&dir_inode);
    int num_blocks = (dir_size + DISKIMG_SECTOR_SIZE - 1) / DISKIMG_SECTOR_SIZE;

    // 4. Leer bloque por bloque y buscar la entrada
    unsigned char buffer[DISKIMG_SECTOR_SIZE];
    for (int b = 0; b < num_blocks; b++) {
        int bytes = file_getblock(fs, dirinumber, b, buffer);
        if (bytes < 0) return -1;

        int num_entries = bytes / sizeof(struct direntv6);
        struct direntv6 *entries = (struct direntv6 *)buffer;

        for (int i = 0; i < num_entries; i++) {
            if (entries[i].d_inumber != 0 &&
                strncmp(entries[i].d_name, name, DIR_NAME_SIZE) == 0) {
                *dirEnt = entries[i];
                return 0;
            }
        }
    }

    return -1; // No se encontró el nombre
}
