#include <openenclave/host.h>
#include <openenclave/bits/tests.h>
#include <stdlib.h>
#include <stdio.h>
#include "dupenv.h"

uint32_t OE_GetCreateFlags(void)
{
    uint32_t result = OE_FLAG_DEBUG;
    char* env = NULL;
    
    if (!(env = OE_Dupenv("OE_SIMULATION")))
        goto done;

    if (strcmp(env, "1") == 0)
        result |= OE_FLAG_SIMULATE;

done:

    if (env)
        free(env);

    return result;
}
