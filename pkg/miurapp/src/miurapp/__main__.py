from miurapp import AppSession
from miurapp import run
from miurcore import Entity


def main() -> None:
    print(run(AppSession(focus=Entity(system="demo", handle="bootstrap"))))


if __name__ == "__main__":
    main()
