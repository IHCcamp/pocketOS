function(compile_erlang module_name)
    add_custom_command(
        OUTPUT ${module_name}.beam
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        COMMENT "Compiling ${module_name}.erl"
    )
endfunction()

function(compile_elixir ex_file beam_file)
    add_custom_command(
        OUTPUT ${beam_file}
        COMMAND elixirc ${CMAKE_CURRENT_SOURCE_DIR}/${ex_file}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${ex_file}
        COMMENT "Compiling ${ex_file}"
    )
endfunction()

function(pack_beam output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND PackBEAM ${output} ${ARGN}
        DEPENDS ${ARGN}
        COMMENT "Packing into ${output}"
    )
endfunction()

function(pack_deps output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND PackBEAM ${output} ${ARGN}
        COMMENT "Packing dependencies into ${output}"
    )
endfunction()
