/* uring_compat.c - Compatibility layer for io_uring operations */

#include <liburing.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Socket operation preparation wrappers */

void hs_uring_prep_recv(struct io_uring_sqe *sqe, int fd, 
                        void *buf, unsigned len, int flags) {
    io_uring_prep_recv(sqe, fd, buf, len, flags);
}

void hs_uring_prep_send(struct io_uring_sqe *sqe, int fd,
                        const void *buf, unsigned len, int flags) {
    io_uring_prep_send(sqe, fd, buf, len, flags);
}

void hs_uring_prep_send_zc(struct io_uring_sqe *sqe, int fd,
                           const void *buf, unsigned len, int flags,
                           unsigned zc_flags) {
    // Check if io_uring_prep_send_zc is available
    // For now we assume liburing 2.3+ is available as per request for "modern" features
    // If not, this will fail to link.
    io_uring_prep_send_zc(sqe, fd, buf, len, flags, zc_flags);
}

void hs_uring_prep_accept(struct io_uring_sqe *sqe, int fd,
                          struct sockaddr *addr, socklen_t *addrlen, int flags) {
    io_uring_prep_accept(sqe, fd, addr, addrlen, flags);
}

void hs_uring_prep_connect(struct io_uring_sqe *sqe, int fd,
                           const struct sockaddr *addr, socklen_t addrlen) {
    io_uring_prep_connect(sqe, fd, addr, addrlen);
}

void hs_uring_prep_cancel(struct io_uring_sqe *sqe, void *user_data, int flags) {
    io_uring_prep_cancel64(sqe, (unsigned long long)user_data, flags);
}

void hs_uring_prep_nop(struct io_uring_sqe *sqe) {
    io_uring_prep_nop(sqe);
}

void hs_uring_prep_readv(struct io_uring_sqe *sqe, int fd, 
                         const struct iovec *iovecs, unsigned nr_vecs, 
                         unsigned long long offset) {
    io_uring_prep_readv(sqe, fd, iovecs, nr_vecs, offset);
}

void hs_uring_prep_writev(struct io_uring_sqe *sqe, int fd, 
                          const struct iovec *iovecs, unsigned nr_vecs, 
                          unsigned long long offset) {
    io_uring_prep_writev(sqe, fd, iovecs, nr_vecs, offset);
}

void hs_uring_prep_read(struct io_uring_sqe *sqe, int fd, 
                        void *buf, unsigned nbytes, 
                        unsigned long long offset) {
    io_uring_prep_read(sqe, fd, buf, nbytes, offset);
}

void hs_uring_prep_write(struct io_uring_sqe *sqe, int fd, 
                         const void *buf, unsigned nbytes, 
                         unsigned long long offset) {
    io_uring_prep_write(sqe, fd, buf, nbytes, offset);
}

void hs_uring_sqe_set_data(struct io_uring_sqe *sqe, unsigned long long data) {
    io_uring_sqe_set_data64(sqe, data);
}

/* New Parity Wrappers */

void hs_uring_prep_poll_add(struct io_uring_sqe *sqe, int fd, unsigned poll_mask) {
    io_uring_prep_poll_add(sqe, fd, poll_mask);
}

void hs_uring_prep_poll_remove(struct io_uring_sqe *sqe, unsigned long long user_data) {
    io_uring_prep_poll_remove(sqe, user_data);
}

void hs_uring_prep_fsync(struct io_uring_sqe *sqe, int fd, unsigned fsync_flags) {
    io_uring_prep_fsync(sqe, fd, fsync_flags);
}

void hs_uring_prep_timeout(struct io_uring_sqe *sqe, struct __kernel_timespec *ts, 
                           unsigned count, unsigned flags) {
    io_uring_prep_timeout(sqe, ts, count, flags);
}

void hs_uring_prep_timeout_remove(struct io_uring_sqe *sqe, unsigned long long user_data, unsigned flags) {
    io_uring_prep_timeout_remove(sqe, user_data, flags);
}

void hs_uring_prep_openat(struct io_uring_sqe *sqe, int dfd, const char *path, int flags, mode_t mode) {
    io_uring_prep_openat(sqe, dfd, path, flags, mode);
}

void hs_uring_prep_close(struct io_uring_sqe *sqe, int fd) {
    io_uring_prep_close(sqe, fd);
}

void hs_uring_prep_fallocate(struct io_uring_sqe *sqe, int fd, int mode, off_t offset, off_t len) {
    io_uring_prep_fallocate(sqe, fd, mode, offset, len);
}

void hs_uring_prep_splice(struct io_uring_sqe *sqe, int fd_in, int64_t off_in, 
                          int fd_out, int64_t off_out, unsigned int nbytes, 
                          unsigned int splice_flags) {
    io_uring_prep_splice(sqe, fd_in, off_in, fd_out, off_out, nbytes, splice_flags);
}

void hs_uring_prep_tee(struct io_uring_sqe *sqe, int fd_in, int fd_out, 
                       unsigned int nbytes, unsigned int splice_flags) {
    io_uring_prep_tee(sqe, fd_in, fd_out, nbytes, splice_flags);
}

void hs_uring_prep_shutdown(struct io_uring_sqe *sqe, int fd, int how) {
    io_uring_prep_shutdown(sqe, fd, how);
}

void hs_uring_prep_renameat(struct io_uring_sqe *sqe, int olddfd, const char *oldpath, 
                            int newdfd, const char *newpath, unsigned int flags) {
    io_uring_prep_renameat(sqe, olddfd, oldpath, newdfd, newpath, flags);
}

void hs_uring_prep_unlinkat(struct io_uring_sqe *sqe, int dfd, const char *path, int flags) {
    io_uring_prep_unlinkat(sqe, dfd, path, flags);
}

void hs_uring_prep_mkdirat(struct io_uring_sqe *sqe, int dfd, const char *path, mode_t mode) {
    io_uring_prep_mkdirat(sqe, dfd, path, mode);
}

void hs_uring_prep_symlinkat(struct io_uring_sqe *sqe, const char *target, int newdfd, const char *linkpath) {
    io_uring_prep_symlinkat(sqe, target, newdfd, linkpath);
}

void hs_uring_prep_linkat(struct io_uring_sqe *sqe, int olddfd, const char *oldpath, 
                          int newdfd, const char *newpath, int flags) {
    io_uring_prep_linkat(sqe, olddfd, oldpath, newdfd, newpath, flags);
}

void hs_uring_prep_madvise(struct io_uring_sqe *sqe, void *addr, off_t length, int advice) {
    io_uring_prep_madvise(sqe, addr, length, advice);
}

void hs_uring_prep_fadvise(struct io_uring_sqe *sqe, int fd, off_t offset, off_t len, int advice) {
    io_uring_prep_fadvise(sqe, fd, offset, len, advice);
}

int hs_uring_register_files(struct io_uring *ring, const int *files, unsigned nr_files) {
    return io_uring_register_files(ring, files, nr_files);
}

int hs_uring_unregister_files(struct io_uring *ring) {
    return io_uring_unregister_files(ring);
}

int hs_uring_register_files_update(struct io_uring *ring, unsigned off, const int *files, unsigned nr_files) {
    return io_uring_register_files_update(ring, off, files, nr_files);
}

/* Completion handling */

int hs_uring_peek_cqe(struct io_uring *ring, struct io_uring_cqe **cqe) {
    return io_uring_peek_cqe(ring, cqe);
}

int hs_uring_wait_cqe(struct io_uring *ring, struct io_uring_cqe **cqe) {
    return io_uring_wait_cqe(ring, cqe);
}

void hs_uring_cqe_seen(struct io_uring *ring, struct io_uring_cqe *cqe) {
    io_uring_cqe_seen(ring, cqe);
}

int hs_uring_register_buffers(struct io_uring *ring, const struct iovec *iovecs, unsigned nr_iovecs) {
    return io_uring_register_buffers(ring, iovecs, nr_iovecs);
}

int hs_uring_unregister_buffers(struct io_uring *ring) {
    return io_uring_unregister_buffers(ring);
}

/* Feature detection */

int hs_uring_probe_op(struct io_uring *ring, int op) {
    struct io_uring_probe *probe = io_uring_get_probe_ring(ring);
    if (!probe) return 0;
    
    int res = io_uring_opcode_supported(probe, op);
    io_uring_free_probe(probe);
    return res;
}

#ifdef __cplusplus
}
#endif
