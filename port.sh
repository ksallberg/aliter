#!/bin/sh

lsof -n -i:6941 | grep LISTEN
lsof -n -i:5121 | grep LISTEN
lsof -n -i:6121 | grep LISTEN

ps aux | grep google
