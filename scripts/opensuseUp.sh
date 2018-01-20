#!/usr/bin/env bash

echo "Start upgrading the Tumbleweed system..."

sudo zypper dup --repo "openSUSE:Tumbleweed"

echo "Finished!"
