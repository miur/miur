# NEED: pip install python-ripgrep


# g1a [!] TODO: integ with #miur
#   [_] IDEA! use it to replace in files from #miur itself (inof qf-in-vim)
def grep():
    from python_ripgrep import search

    results = search(
        patterns=["your_regex_pattern"],
        paths=["/path/to/search"],
        globs=["*.py"],
    )
    for result in results:
        print(result)
