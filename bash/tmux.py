#!/usr/bin/env python3

import os
import sys
import subprocess


SHELL = os.getenv("SHELL", "/bin/bash")


def get_sessions():
    sessions = []

    output = subprocess.Popen(["tmux", "list-sessions"], stdout=subprocess.PIPE).communicate()[0]
    if sys.stdout.encoding is None:
        output = output.decode("UTF-8")
    else:
        output = output.decode(sys.stdout.encoding)
    if output:
        for s in output.splitlines():
            # Ignore hidden sessions (named sessions that start with a "_")
            if s and not s.startswith("_"):
                sessions.append(s.strip())
    return sessions


sessions = get_sessions()
if sessions:
    session_name = sessions[-1].split(":")[0]
    os.execvp("tmux", ["tmux", "attach", "-t", session_name])
else:
    os.execvp("tmux", ["tmux", "new"])
