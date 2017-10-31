#!/bin/sh

lsof -n -i:6941 | grep LISTEN
