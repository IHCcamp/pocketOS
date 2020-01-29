cmake_minimum_required (VERSION 2.6)
project (core)

include(../cmake/beam.cmake)

add_subdirectory(icons)

compile_erlang(display)
compile_erlang(input)
compile_erlang(scene)

pack_deps(deps.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/estdlib.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/eavmlib.avm
)

pack_beam(pocketos.avm
    scene.beam
    display.beam
    input.beam
    icons/icons64.beam
    deps.avm
)

add_custom_target(core DEPENDS pocketos.avm)
add_dependencies(core icons)
