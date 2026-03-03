import threading


class LazyTransform:
    _lock = threading.Lock()

    def __init__(self, target_cls, *args, **kwargs):
        # Using __dict__ directly avoids triggering any logic
        self.__dict__["_target_cls"] = target_cls
        self.__dict__["_init_args"] = (args, kwargs)

    def _materialize(self):
        with self._lock:
            # Re-check in case another thread finished while we waited
            if not isinstance(self, self._target_cls):
                cls = self._target_cls
                args, kwargs = self._init_args

                # Create real instance and "coerce" data
                real_instance = cls(*args, **kwargs)

                # ZERO OVERHEAD: Swap the class of 'self'
                # Future lookups use target_cls methods directly
                self.__class__ = cls
                self.__dict__.update(real_instance.__dict__)

                # Cleanup lazy metadata
                del self.__dict__["_target_cls"]
                del self.__dict__["_init_args"]

    def __getattr__(self, name):
        """Triggered only for attributes not in __dict__."""
        self._materialize()
        return getattr(self, name)

    def __dir__(self):
        """Makes dir() match the target class immediately."""
        return dir(self._target_cls)


# 1. Create proxy: No HeavyObject init yet
lazy_obj = LazyTransform(HeavyObject, 100)

# 2. dir() shows HeavyObject methods (like 'action')
print("action" in dir(lazy_obj))  # True

# 3. Accessing 'action' transforms the object
print(lazy_obj.action())  # Prints "HeavyObject fully..." then "Action with 100"

# 4. Zero overhead: it is now a real HeavyObject
print(type(lazy_obj))  # <class '__main__.HeavyObject'>
