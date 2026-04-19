from miurdemo import run_demo


def test_run_demo_uses_app_entry_flow() -> None:
    assert run_demo() == "cursor=demo:root"
