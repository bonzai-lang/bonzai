from os import system
from shutil import which
import os.path
from glob import glob
import platform
from sys import argv

is_root = argv[1] == '--root' if len(argv) > 1 else False

# Check for Cabal and XMake 
if not which('cabal') or not which('xmake'):
  print('Please install cabal and xmake')
  exit(1)

# Build the compiler project
system('cabal build')

ext = '.exe' if platform.system() == 'Windows' else ''

executable_name = f"bonzai{ext}"

found_executables = glob(f"dist-newstyle/**/{executable_name}", recursive=True)
executable_files = [file for file in found_executables if os.path.isfile(file)]

if len(executable_files) == 0:
  print('No executable found')
  exit(1)

executable = executable_files[0]
executable_out = f"bonzaic{ext}"

if not os.path.isdir('bin'): os.mkdir('bin')

system(f"cp {executable} bin/{executable_out}")

# Build the runtime project

runtime_executable = f"bonzai-runtime{ext}"
runtime_executable_out = f"bonzai{ext}"

xmake_root = '--root' if is_root else ''
system(f'xmake b {xmake_root} -P runtime')
system(f"cp runtime/bin/{runtime_executable} bin/{runtime_executable_out}")

system(f'xmake config {xmake_root} -P bindings --ccache=n -y')
system(f'xmake b {xmake_root} -P bindings')

# Write simple shell script
shell_script = f"""#!/bin/sh
bonzai $BONZAI_PATH/packages/bpm/main.bzi.bin -l $BONZAI_PATH/bindings/bin/bindings.dylib "$@"
"""

with open('bin/bpm', 'w') as f:
  f.write(shell_script)

system('chmod +x bin/bpm')

print('Build ran successfully')