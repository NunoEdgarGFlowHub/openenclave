// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

// TODO:
// 1. add verify_full_evidence support
// 2. add support for get_tcb info, get_crl info, get qe identity info
//    Serialize and deserialize, and verufy time
// 3. Add oe_verify_report_ex(), which takes tcbinfo, qe_ide, and crl as inputs
// 4. Add a plugin smaple for verify_full_evidence
// 5. add a other report type for token

#include <stdio.h>

// Include the trusted attestation_plugin header that is generated
// during the build. This file is generated by calling the
// sdk tool oeedger8r against the attestation_plugin.edl file.
#include "attestation_plugin_t.h"

extern oe_attestation_plugin_context_t my_plugin_context1;
extern oe_attestation_plugin_context_t my_plugin_context2;
extern oe_attestation_plugin_context_t my_plugin_context3;

// This is the function that the host calls. It prints
// a message in the enclave before calling back out to
// the host to print a message from there too.
void enclave_attestation_plugin()
{
    oe_result_t result = OE_FAILURE;
    uint8_t* evidence_buffer1 = NULL;
    size_t evidence_buffer1_size = 0;
    uint8_t* evidence_buffer2 = NULL;
    size_t evidence_buffer2_size = 0;
    uint8_t* evidence_buffer3 = NULL;
    size_t evidence_buffer3_size = 0;
    uint8_t* custom_data = NULL;
    size_t custom_evidence_size = 0;
    oe_report_t parsed_report = {0};
    void* context = NULL;

    fprintf(stdout, "Hello from encalve::enclave_attestation_plugin\n");
    // unregister all attestation plugins
    result = oe_register_attestation_plugin(&my_plugin_context1);
    if (result != OE_OK)
    {
        fprintf(
            stdout,
            "oe_register_attestation_plugin failed for "
            "evidence_format_uuid (1)\n");
        goto done;
    }
    fprintf(
        stdout,
        "oe_register_attestation_plugin succeeded for "
        "evidence_format_uuid (1)\n");

    result = oe_register_attestation_plugin(&my_plugin_context2);
    if (result != OE_OK)
    {
        fprintf(
            stdout,
            "oe_register_attestation_plugin failed for "
            "evidence_format_uuid (2)\n");
        goto done;
    }

    fprintf(
        stdout,
        "oe_register_attestation_plugin succeeded for "
        "evidence_format_uuid (2)\n");

    result = oe_register_attestation_plugin(&my_plugin_context3);
    if (result != OE_OK)
    {
        fprintf(
            stdout,
            "oe_register_attestation_plugin failed for "
            "evidence_format_uuid (3)\n");
        goto done;
    }

    fprintf(
        stdout,
        "oe_register_attestation_plugin succeeded for "
        "evidence_format_uuid (3)\n");

    result = oe_get_attestation_evidence(
        &my_plugin_context1.evidence_format_uuid,
        &evidence_buffer1,
        &evidence_buffer1_size);
    if (result != OE_OK)
        goto done;

    fprintf(
        stdout,
        "oe_get_attestation_evidence succeeded with "
        "evidence_buffer1_size=%zu\n",
        evidence_buffer1_size);

    result = oe_verify_attestation_evidence(
        context, evidence_buffer1, evidence_buffer1_size, &parsed_report);
    if (result != OE_OK)
        goto done;

    fprintf(stdout, "oe_verify_attestation_evidence succeeded 1\n");

    result = oe_get_attestation_evidence(
        &my_plugin_context2.evidence_format_uuid,
        &evidence_buffer2,
        &evidence_buffer2_size);
    if (result != OE_OK)
        goto done;

    fprintf(
        stdout,
        "oe_get_attestation_evidence succeeded with evidence_buffer_size 2"
        "=%zu\n",
        evidence_buffer2_size);

    result = oe_verify_attestation_evidence(
        context, evidence_buffer2, evidence_buffer2_size, &parsed_report);
    if (result != OE_OK)
        goto done;

    fprintf(stdout, "oe_verify_attestation_evidence succeeded 2\n");

    result = oe_get_attestation_evidence(
        &my_plugin_context3.evidence_format_uuid,
        &evidence_buffer3,
        &evidence_buffer3_size);
    if (result != OE_OK)
        goto done;

    fprintf(
        stdout,
        "oe_get_attestation_evidence succeeded with evidence_buffer_size 3"
        "=%zu\n",
        evidence_buffer3_size);

    result = oe_verify_attestation_evidence(
        context, evidence_buffer3, evidence_buffer3_size, &parsed_report);
    if (result != OE_OK)
        goto done;

    fprintf(stdout, "oe_verify_attestation_evidence succeeded 3\n");

    // Call back into the host
    result = host_attestation_plugin();
    if (result != OE_OK)
    {
        fprintf(
            stderr,
            "Call to host_helloworld failed: result=%u (%s)\n",
            result,
            oe_result_str(result));
    }

    // unregister all attestation plugins
    result = oe_unregister_attestation_plugin(&my_plugin_context1);
    if (result != OE_OK)
        goto done;
    fprintf(
        stdout,
        "oe_unregister_attestation_plugin succeeded for "
        "evidence_format_uuid (3)\n");

    result = oe_unregister_attestation_plugin(&my_plugin_context2);
    if (result != OE_OK)
        goto done;
    fprintf(
        stdout,
        "oe_unregister_attestation_plugin succeeded for "
        "evidence_format_uuid(2)\n");

    result = oe_unregister_attestation_plugin(&my_plugin_context3);
    if (result != OE_OK)
        goto done;
    fprintf(
        stdout,
        "oe_unregister_attestation_plugin succeeded for "
        "evidence_format_uuid (3)\n");

done:
    oe_free_attestation_evidence(evidence_buffer1);
    oe_free_attestation_evidence(evidence_buffer2);
    oe_free_attestation_evidence(evidence_buffer3);
    return;
}
