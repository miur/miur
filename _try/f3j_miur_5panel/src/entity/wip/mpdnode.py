# Using the client library — python-mpd2 3.0.3 documentation ⌇⡧⢪⡿⢰
#   https://python-mpd2.readthedocs.io/en/latest/topics/getting-started.html

# import os
# import os.path as fs
from typing import override

from ..base.golden import Entities, Entity, Golden
from ..core.fsentry import FSAuto
from ..core.text import TextEntry


class MPDProto(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

    @override
    def explore(self) -> Entities:
        from mpd import MPDClient

        client = MPDClient()
        client.timeout = 10  # network timeout in seconds
        client.idletimeout = None
        client.connect("localhost", 6600)
        yield TextEntry(str(client.mpd_version), self)
        # yield TextEntry(str(client.find("any", "house")), self)
        client.iterate = True
        for song in client.playlistinfo():
            yield FSAuto(str(song["file"]), self)
        client.close()
        client.disconnect()
