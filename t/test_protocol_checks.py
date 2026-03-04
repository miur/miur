# USAGE: $ (source /d/miur/.venv/bin/activate && pip install -e . && pytest)


def test_golden_satisfies_protocols() -> None:
    from typing import Any, Iterable

    from _check import verify_protocol_impl
    from miur.entity.base.golden import Golden
    from miur.entity.base.traits import GoldenProtocol

    Entity = Golden[Any]  # runtime only, not imported anywhere else
    Entities = Iterable[Entity]  # runtime only

    # if __debug__ or __name__ == "__main__":
    verify_protocol_impl(
        Golden,
        GoldenProtocol,
        extra_ns={
            "Entity": Golden[Any],
            "Entities": Iterable[Golden[Any]],
            "Golden": Golden,
        },
    )
