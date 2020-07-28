#!/usr/bin/env python3

import os
import copy
import sys
import subprocess


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


env = copy.deepcopy(os.environ)
env.update({"TERM": "xterm-256color"})


if sessions:
    session_name = sessions[-1].split(":")[0]
    os.execvpe("tmux", ["tmux", "attach", "-t", session_name], env)
else:
    os.execvpe("tmux", ["tmux", "new"], env)
