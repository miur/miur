from typing import override

from .base.golden import Entities, Entity, Golden
from .text import TextEntry


# MAYBE:SPLIT: use UrlEntity.explore() -> HTMLEntity -> Blocks/Links
#   ALT:TRY: !elinks
# ALSO: make entities for REST/API/JSON(jira/confl)
class WebPageEntity(Golden[str]):
    def __init__(self, url: str, parent: Entity) -> None:
        super().__init__(url, parent)

    def html(self) -> Entities:
        # from urllib.parse import urlsplit
        import requests

        session = requests.Session()
        site = session.get(self._x)

        # CHG: yield by block inof line
        for l in site.text.splitlines():
            if l:
                yield TextEntry(l, self)

    @override
    def explore(self) -> Entities:
        # from urllib.parse import urlsplit
        import requests
        from bs4 import BeautifulSoup

        session = requests.Session()
        site = session.get(self._x)
        # CFG: https://www.crummy.com/software/BeautifulSoup/bs4/doc/#installing-a-parser
        soup = BeautifulSoup(site.text, features="lxml")

        for l in soup.get_text().splitlines():
            if l:
                # CHG: if line has URL -- yield one more WebPage
                yield TextEntry(l, self)

        # DEBUG: Path("/t/pretty").write_text(soup.prettify())
        # for chap in soup.find_all("div", class_="chapter"):
        #     yield parse_one(chap)


## REF: https://requests.readthedocs.io/en/latest/
# r = requests.get('https://api.github.com/user', auth=('user', 'pass'))
# r.status_code
# 200
# r.headers['content-type']
# 'application/json; charset=utf8'
# r.encoding
# 'utf-8'
# r.text
# '{"type":"User"...'
# r.json()
# {'private_gists': 419, 'total_private_repos': 77, ...}
