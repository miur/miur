import pickle


def serialize(obj, ofmt=None):
    if ofmt is None:
        ofmt = pickle

    if ofmt is str:
        data = str(obj).encode()
    elif ofmt is pickle:
        data = pickle.dumps(obj, protocol=pickle.HIGHEST_PROTOCOL)
    return data


def deserialize(data, ifmt=None):
    try:
        # CHECK: 'data_received' guarantee complete msg OR only partial ones
        obj = pickle.loads(data)
        ifmt = pickle
    # BAD: unreliable and slow method to combine data + text_msg by 'nc'
    except (pickle.UnpicklingError, KeyError, ValueError, EOFError):
        obj = data.decode()
        ifmt = str
    return (obj, ifmt)
