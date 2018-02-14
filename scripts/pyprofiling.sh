# script to profile my Python code
# Requirements:
# apt-get install kcachegrind
# pip install pyprof2calltree
# Author: Wanderson Ferreira


python -m cProfile -o script.profile $1
pyprof2calltree -i script.profile -o script.calltree
kcachegrind script.calltree &
