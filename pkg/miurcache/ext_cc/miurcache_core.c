/* pkg/miurcache/ext/miurcache_core.c */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

// The actual hello() function implementation
static PyObject*
cache_hello(PyObject* self, PyObject* args)
{
    return PyUnicode_FromString("Hello from the C Extension!");
}

// Define the methods for this module
static PyMethodDef CacheMethods[] = {
    {"hello", cache_hello, METH_NOARGS, "Returns a greeting from the C implementation."},
    {NULL, NULL, 0, NULL} /* Sentinel */
};

// Define the module
static struct PyModuleDef coremodule = {
    PyModuleDef_HEAD_INIT,
    "_ext_cc",                             // Module name
    "Core C implementation for miurcache", // Module docstring
    -1,                                    // Size of per-interpreter state of the module
    CacheMethods                           // Methods array
};

// Module initialization function (Must match the OUTPUT_NAME in CMake)
PyMODINIT_FUNC
PyInit__ext_cc(void)
{
    return PyModule_Create(&coremodule);
}
