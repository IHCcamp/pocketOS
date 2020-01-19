cmake_minimum_required (VERSION 2.6)
project (icons)

include(../../cmake/beam.cmake)

function(convert_image input output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND convert ${input} -define h:format=rgba -depth 8 ${input} ${output}
        DEPENDS ${input}
        COMMENT "Converting ${input}"
    )
endfunction()

function(rgba_to_function input name width height output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND python3 rgba_to_erl.py ${input} ${name} ${width} ${height} > ${output}
        DEPENDS ${input}
        COMMENT "Writing ${input} to erl file"
    )
endfunction()

function(image_to_function input name width height output)
    convert_image(${input} ${input}.rgba)
    rgba_to_function(${input}.rgba ${name} ${width} ${height} ${output})
endfunction()

function(make_module output)
    add_custom_command(
        OUTPUT ${output}
        COMMAND bash write_module.sh ${output} ${ARGN}
        DEPENDS ${ARGN}
        COMMENT "Writing ${output} module"
    )
endfunction()

image_to_function(64/status/critical.png critical_icon 64 64 icons64_critical_icon.erl.func)
image_to_function(64/status/idea.png idea_icon 64 64 icons64_idea_icon.erl.func)
image_to_function(64/status/info.png info_icon 64 64 icons64_info_icon.erl.func)
image_to_function(64/status/warning.png warning_icon 64 64 icons64_warning_icon.erl.func)

make_module(icons64.erl
    icons64_idea_icon.erl.func
    icons64_info_icon.erl.func
    icons64_critical_icon.erl.func
    icons64_warning_icon.erl.func
)

compile_erlang(icons64)

add_custom_target(icons DEPENDS
    icons64.beam
)
