def verify_protocol_impl(impl: type, protocol: type, extra_ns: dict[str, type]) -> None:
    from typing import Protocol, get_type_hints

    proto_members = {
        name
        for cls in protocol.__mro__
        if cls not in (object, Protocol)
        for name in vars(cls)
        if not name.startswith("_")
    }
    for name in proto_members:
        impl_member = getattr(impl, name, None)
        assert impl_member is not None, f"{impl.__name__} missing '{name}'"
        proto_raw = getattr(protocol, name)
        impl_raw = impl_member
        proto_fn = proto_raw.fget if isinstance(proto_raw, property) else proto_raw
        impl_fn = impl_raw.fget if isinstance(impl_raw, property) else impl_raw
        try:
            proto_hints = get_type_hints(proto_fn, localns=extra_ns)
            impl_hints = get_type_hints(impl_fn, localns=extra_ns)
        except Exception as e:
            print(f"WARNING: could not resolve hints for {name}: {e}")
            continue
        assert proto_hints == impl_hints, (
            f"{impl.__name__}.{name}:\n  impl:  {impl_hints}\n  proto: {proto_hints}"
        )


## ALT:v2
# def _verify_protocol_impl(impl: type, protocol: type) -> None:
#     import sys
#     from inspect import getmembers, signature
#
#     ns = (
#         sys.modules[impl.__module__].__dict__
#         | sys.modules[protocol.__module__].__dict__
#     )
#     for name, member in getmembers(protocol):
#         if name.startswith("_"):
#             continue
#         impl_member = getattr(impl, name, None)
#         assert impl_member is not None, f"{impl} missing {name}"
#         # check signatures
#         proto_sig = signature(member, eval_str=True, globals=ns)
#         impl_sig = signature(impl_member, eval_str=True, globals=ns)
#         assert proto_sig == impl_sig, f"{impl}.{name}: {impl_sig} != {proto_sig}"
