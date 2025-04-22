#!/bin/bash

# https://github.com/willkg/dotfiles/blob/main/dotfiles/bin/uv-python-symlink

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at http://mozilla.org/MPL/2.0/.

# Creates symlinks in $HOME/.local/bin for python versions installed with uv.

LOCALBIN="${HOME}/.local/bin"
UVDIR=$(uv python dir)

# Create symlinks for pythonX.Y to uv-managed Pythons
for ITEM in "${UVDIR}"/*
do
    BASEITEM=$(basename "${ITEM}")

    FULLVERSION=$(echo "${BASEITEM}" | cut -d'-' -f 2)
    MINORVERSION=$(echo "${FULLVERSION}" | rev | cut -f 2- -d '.' | rev)
    DEST="${LOCALBIN}/python${MINORVERSION}"

    if [[ -L "${DEST}" ]]
    then
        if [[ -e "${DEST}" ]]
        then
            echo "${DEST} already exists and is valid. Nothing to do."
            continue
        else
            echo "${DEST} already exists but is broken. Removing."
            rm "${DEST}"
        fi
    fi

    rm -rf "${DEST}"
    ln -s "${UVDIR}/${BASEITEM}/bin/python${MINORVERSION}" "${DEST}"
    echo "${DEST} created."
done

# Create symlink for python to latest uv-managed Python
LATESTPYTHON=$(uv python find)
DEST="${LOCALBIN}/python"

if [[ -L "${DEST}" ]]
then
    if [[ -e "${DEST}" ]]
    then
        echo "${DEST} already exists and is valid. Nothing to do."
        exit
    else
        echo "${DEST} already exists but is broken. Removing."
        rm "${DEST}"
    fi
fi

rm -rf "${DEST}"
ln -s "${LATESTPYTHON}" "${DEST}"
echo "${DEST} created."
