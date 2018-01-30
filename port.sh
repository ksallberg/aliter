#!/bin/sh

echo "look at ports:"

lsof -n -i:6900 | grep LISTEN
lsof -n -i:5121 | grep LISTEN
lsof -n -i:6121 | grep LISTEN

ps aux | grep google
