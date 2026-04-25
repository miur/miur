from miurapp import AppSession, run
from miurcache import hello
from miurcore import Entity


def main() -> None:
    print(hello())
    print(run(AppSession(focus=Entity(system="demo", handle="bootstrap"))))


if __name__ == "__main__":
    main()
