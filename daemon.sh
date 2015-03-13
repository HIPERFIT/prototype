#!/bin/sh

# Quick start-stop-daemon example, derived from Debian /etc/init.d/ssh
set -e

# Must be a valid filename
NAME=prototype-web
PIDFILE=$NAME.pid
# Probably it's better to use absolute path.
# Won't work if running not from the project root dir.
PROJ_DIR=.

DAEMON=./dist/build/web/web
DAEMON_OPTS="8000"

export PATH="${PATH:+$PATH:}/usr/sbin:/sbin"

case "$1" in
    start)
        echo -n "Starting daemon: "$NAME
        start-stop-daemon --start --quiet --background --chdir $PROJ_DIR -m --pidfile $PIDFILE --exec $DAEMON -- $DAEMON_OPTS
        echo "."
        ;;
    stop)
        echo -n "Stopping daemon: "$NAME
        start-stop-daemon --stop --quiet --oknodo --pidfile $PIDFILE
        rm $PIDFILE
        echo "."
        ;;
    restart)
        echo -n "Restarting daemon: "$NAME
        start-stop-daemon --stop --quiet --oknodo --retry 30 --pidfile $PIDFILE
        start-stop-daemon --start --quiet --pidfile $PIDFILE --exec $DAEMON -- $DAEMON_OPTS
        echo "."
        ;;
    
    *)
        echo "Usage: "$1" {start|stop|restart}"
        exit 1
esac

exit 0
