# Copyright (c) Open Enclave SDK contributors.
# Licensed under the MIT License.


oeedl_file(../debug_mode.edl host gen)

add_executable(debug_host host.c ${gen})

target_include_directories(debug_host PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(debug_host oehostapp)
