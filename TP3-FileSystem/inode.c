#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "inode.h"
#include "diskimg.h"

int inode_iget(struct unixfilesystem *fs, int inumber, struct inode *inp) {
    if (fs == NULL || inp == NULL || inumber < 1) return -1;

    int inodes_per_sector = DISKIMG_SECTOR_SIZE / sizeof(struct inode);
    int max_inodes = fs->superblock.s_isize * inodes_per_sector;
    if (inumber > max_inodes) return -1;

    int sector_number = INODE_START_SECTOR + (inumber - 1) / inodes_per_sector;
    int offset = (inumber - 1) % inodes_per_sector;

    unsigned char buffer[DISKIMG_SECTOR_SIZE];
    if (diskimg_readsector(fs->dfd, sector_number, buffer) != DISKIMG_SECTOR_SIZE)
        return -1;

    struct inode *inode_table = (struct inode *) buffer;
    *inp = inode_table[offset];

    return 0;
}

int inode_indexlookup(struct unixfilesystem *fs, struct inode *inp, int blockNum) {
    if (blockNum < 0 || !(inp->i_mode & IALLOC)) return -1;

    // Directo
    if (!(inp->i_mode & ILARG)) {
        if (blockNum >= 8) return -1;
        return inp->i_addr[blockNum];
    }

    // Indirecto
    int indirect_limit = 7 * 256;

    if (blockNum < indirect_limit) {
        int indirect_block_index = blockNum / 256;
        int offset = blockNum % 256;

        if (inp->i_addr[indirect_block_index] == 0) return -1;

        uint16_t buffer[256];
        if (diskimg_readsector(fs->dfd, inp->i_addr[indirect_block_index], buffer) != DISKIMG_SECTOR_SIZE)
            return -1;

        return buffer[offset];
    }

    // Doblemente indirecto
    int double_block_index = blockNum - indirect_limit;
    if (double_block_index >= 256 * 256) return -1;

    int first_index = double_block_index / 256;
    int second_index = double_block_index % 256;

    if (inp->i_addr[7] == 0) return -1;

    uint16_t level1[256];
    if (diskimg_readsector(fs->dfd, inp->i_addr[7], level1) != DISKIMG_SECTOR_SIZE)
        return -1;

    if (level1[first_index] == 0) return -1;

    uint16_t level2[256];
    if (diskimg_readsector(fs->dfd, level1[first_index], level2) != DISKIMG_SECTOR_SIZE)
        return -1;

    return level2[second_index];
}

int inode_getsize(struct inode *inp) {
  return ((inp->i_size0 << 16) | inp->i_size1); 
}
