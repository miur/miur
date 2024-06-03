# REF: The Python Profilers — Python 3.12.3 documentation ⌇⡦⠿⡻⣪
#   https://docs.python.org/3/library/profile.html#what-is-deterministic-profiling

from cProfile import Profile

g_profile = Profile()  # BAD: only ms resolution

# FAIL: overwrites inof appending
# retval = pf.runcall(func, *args, **kwargs)
# pf.dump_stats(func.__name__ + ".pfl")


def profileit(func):
    ##%USAGE:(decorator): @__import__('prof').profileit

    @__import__("functools").wraps(func)
    def wrapper(*args, **kwargs):
        g_profile.enable()
        try:
            return func(*args, **kwargs)
        finally:
            g_profile.disable()

    return wrapper


##%ALT:(analysis): $ python -m pstats foo.pfl  % help  % sort time  % stats 10
@__import__("atexit").register
def print_stats():
    __import__("pstats").Stats(g_profile).sort_stats("tottime").print_stats()
