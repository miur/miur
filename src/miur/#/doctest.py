
def add(a: int, b: int) -> int:
    """
    Returns the sum of two integers.

    >>> add(2, 3)
    5
    >>> add(-1, 1)
    0
    """
    return a + b


def divide(a: int, b: int) -> float:
    """
    >>> divide(10, 0)
    Traceback (most recent call last):
        ...
    ZeroDivisionError: division by zero
    """
    return a / b


if __name__ == "__main__":
    # OR: python -m doctest enum_hotreload.py -v
    # OR: pytest --doctest-modules
    __import__("doctest").testmod(verbose=True)
