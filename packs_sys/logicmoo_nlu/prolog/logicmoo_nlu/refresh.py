def refresh(filepath = __file__, _globals = None, _locals = None):
    print("Reading {}...".format(filepath))
    if _globals is None:
        _globals = globals()
    _globals.update({
        "__file__": filepath,
        "__name__": "__main__",
    })
    with open(filepath, 'rb') as file:
        exec(compile(file.read(), filepath, 'exec'), _globals, _locals)

def refresh___BASE__():
    refresh("__FILE__")


