from miurcore import Entity
from miurtui import render
from miurview import Cursor


def main() -> None:
    print(render(Cursor(Entity(system="demo", handle="bootstrap"))))


if __name__ == "__main__":
    main()
