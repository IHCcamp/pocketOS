#!/bin/bash

MODULE_NAME=$(echo "${1}" | sed 's/\.erl//g')
echo "-module(${MODULE_NAME})." > "${1}"

for export_fun in "${@:2}"
do
    FUNCTION_NAME=$(echo ${export_fun} | sed 's/\.erl\.func//g' | sed "s/${MODULE_NAME}_//g")
    echo "-export([${FUNCTION_NAME}/0])." >> "${1}"
done

cat "${@:2}" >> "${1}"
