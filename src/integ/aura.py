import os

from ..app import KeyTable


# TODO: lazy_load
def keytable_insert_aura_pathes(keytable: KeyTable) -> KeyTable:
    ## Generate key bindings for fast directory jumping
    fpathes = "/d/airy/airy/pathes"
    lst = []
    try:
        with open(fpathes, "r", encoding="utf-8") as f:
            lst = f.readlines()
    except IOError as exc:
        from ..util.exchook import log_exc

        log_exc(exc)
        return keytable

    import os.path as fs

    from ..entity.fsentry import FSAuto

    # FIXME: allow ".#"
    entries = [l.partition("#")[0].strip().split(None, 1) for l in lst]
    it = sorted(filter(lambda e: len(e) > 1, entries), key=lambda l: l[0])
    for e in it:
        assert len(e) in [2, 3]
        keys, flags, path = e[0], (e[1] if len(e) == 3 else None), e[-1]
        t = keytable
        for k in keys[:-1]:
            t.setdefault(k, {})
            t = t[k]
            if not isinstance(t, dict):
                raise ValueError("Overlapping keybind vs keytable")
        if not flags:
            pass
        elif flags == "-l":
            lpath = os.readlink(path)
            # NOTE: resolve only basename (relative to its dir)
            if lpath.startswith("/"):
                anchor = fs.realpath(fs.dirname(path))
                relpath = fs.relpath(fs.realpath(path), anchor)
                path = fs.join(fs.dirname(path), relpath)  # MAYBE:USE fs.abspath()
            else:
                path = fs.join(fs.dirname(path), lpath)
        elif flags == "-L":
            path = fs.realpath(path)
        elif flags == "-m":
            from stat import S_ISREG as isfile

            files = __import__("glob").glob(path + "/**", recursive=True)
            path = max(
                (st.st_mtime, x) for x in files if isfile((st := os.stat(x)).st_mode)
            )[1]

        # log.info(keytable["."])
        if keys[-1] in t:
            raise ValueError("Conflicting keybind")
        t[keys[-1]] = lambda g, v=path: g.root_wdg._navi.view_jump_to(FSAuto(v, None))
    return keytable
