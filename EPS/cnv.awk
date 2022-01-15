#!/usr/bin/gawk -f

($5!=last){ print s; } { print; last=$5; }

