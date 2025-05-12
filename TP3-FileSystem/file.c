#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "file.h"
#include "inode.h"
#include "diskimg.h"

int file_getblock(struct unixfilesystem *fs, int inumber, int blockNum, void *buf) {
    struct inode in;

    // 1. Obtener el inodo
    if (inode_iget(fs, inumber, &in) < 0) return -1;
    if (!(in.i_mode & IALLOC)) return -1;

    // 2. Obtener tamaño del archivo
    int file_size = inode_getsize(&in);
    int total_blocks = (file_size + DISKIMG_SECTOR_SIZE - 1) / DISKIMG_SECTOR_SIZE;

    // 3. Validar bloque pedido
    if (blockNum < 0 || blockNum >= total_blocks) return -1;

    // 4. Obtener número de sector físico
    int disk_block = inode_indexlookup(fs, &in, blockNum);
    if (disk_block < 0) return -1;

    // 5. Leer el bloque desde el disco
    if (diskimg_readsector(fs->dfd, disk_block, buf) != DISKIMG_SECTOR_SIZE) return -1;

    // 6. Calcular bytes válidos
    int offset = blockNum * DISKIMG_SECTOR_SIZE;
    int remaining = file_size - offset;

    return (remaining >= DISKIMG_SECTOR_SIZE) ? DISKIMG_SECTOR_SIZE : remaining;
}

