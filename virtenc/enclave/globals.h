// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

#ifndef _VE_ENCLAVE_GLOBALS_H
#define _VE_ENCLAVE_GLOBALS_H

#include <openenclave/bits/types.h>
#include "thread.h"

typedef struct _globals
{
    thread_t* threads;
    ve_lock_t lock;

    /* Shared memory between host and enclave. */
    void* shmaddr;
} globals_t;

extern globals_t globals;

/* Socket used to communicate with the host process. */
extern int g_sock;

/* TLS information from the host. */
extern size_t g_tdata_rva;
extern size_t g_tdata_size;
extern size_t g_tdata_align;
extern size_t g_tbss_rva;
extern size_t g_tbss_size;
extern size_t g_tbss_align;

/* Holds relative virtual address of this variable itself (from the host). */
extern uint64_t __ve_self;

#endif /* _VE_ENCLAVE_GLOBALS_H */