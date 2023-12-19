#!/bin/bash
user_count=$(who | grep -v "$(whoami)" | wc -l)
if [ "$user_count" -eq 0 ]; then
    /usr/local/etc/shutdown.sh -y $(hostname)
else
    echo "Other users are currently logged in."
fi