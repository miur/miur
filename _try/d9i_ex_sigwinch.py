# python -c 'exec(open("test.py").read())'
import os
import curses

def run():
 stdscr=curses.initscr()
 os.unsetenv("LINES")
 os.unsetenv("COLUMNS")
 key=0
 while(key!=ord('q')):
  key=stdscr.getch()
  stdscr.addstr(0,0,str(stdscr.getmaxyx())+' '+str(key))
  stdscr.refresh()
 curses.endwin()

run()
