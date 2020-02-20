cmake_minimum_required (VERSION 2.6)
project (core)

include(../cmake/beam.cmake)

add_subdirectory(icons)

compile_erlang(display)
compile_elixir(keyboard_parser.ex Elixir.KeyboardParser.beam)
compile_erlang(scene)
compile_elixir(main.ex Elixir.Main.beam)

pack_deps(deps.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/estdlib.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/eavmlib.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/exavmlib.avm
)

pack_beam(pocketos.avm
    Elixir.Main.beam
    scene.beam
    display.beam
    Elixir.KeyboardParser.beam
    icons/icons64.beam
    deps.avm
)

add_custom_target(core DEPENDS pocketos.avm)
add_dependencies(core icons)
