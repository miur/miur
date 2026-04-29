// #include "graphcore.h"

// SRC:OLD:(tmpl):
//   https://discuss.python.org/t/adding-extension-module-examples-to-the-packaging-user-guide/105111/10
//
// Important ABI3 Constraints
//   Struct Access: You cannot access PyObject fields directly (e.g.,
//   obj->ob_type). You must use getter functions like Py_TYPE(obj).

//// REM? only needed for older python
// #define PY_SSIZE_T_CLEAN
#define Py_LIMITED_API 0x030E0000  // Stable ABI3 for python>=3.14
#include <Python.h>
// #include "libIwanttolinkto.h"

// struct {
//     ....
// } the_struct_C_lib_exposes_t;

// typedef struct {
//     PyObject_HEAD Py_buffer view;
//     gc_graph g;
// } PyGraph;
//
// /* ---------- lifecycle ---------- */
//
// static int
// PyGraph_init(PyGraph* self, PyObject* args, PyObject* kw)
// {
//     PyObject* obj;
//
//     if (!PyArg_ParseTuple(args, "O", &obj)) {
//         return -1;
//     }
//
//     if (PyObject_GetBuffer(obj, &self->view, PyBUF_SIMPLE) < 0) {
//         return -1;
//     }
//
//     if (gc_graph_init(&self->g, self->view.buf, self->view.len) || gc_graph_validate(&self->g)) {
//         PyBuffer_Release(&self->view);
//         PyErr_SetString(PyExc_ValueError, "invalid graph buffer");
//         return -1;
//     }
//
//     return 0;
// }
//
// static void
// PyGraph_dealloc(PyGraph* self)
// {
//     PyBuffer_Release(&self->view);
//     Py_TYPE(self)->tp_free((PyObject*)self);
// }
//
// /* ---------- methods ---------- */
//
// static PyObject*
// PyGraph_node_count(PyGraph* self, PyObject* Py_UNUSED(ignored))
// {
//     return PyLong_FromSize_t(gc_node_count(&self->g));
// }
//
// static PyObject*
// PyGraph_degree(PyGraph* self, PyObject* args)
// {
//     unsigned long node;
//     if (!PyArg_ParseTuple(args, "k", &node)) {
//         return NULL;
//     }
//
//     size_t deg;
//     if (gc_node_degree(&self->g, (gc_node_id)node, &deg)) {
//         PyErr_SetString(PyExc_IndexError, "invalid node");
//         return NULL;
//     }
//
//     return PyLong_FromSize_t(deg);
// }
//
// static PyObject*
// PyGraph_edges(PyGraph* self, PyObject* args)
// {
//     unsigned long node;
//     if (!PyArg_ParseTuple(args, "k", &node)) {
//         return NULL;
//     }
//
//     size_t deg;
//     if (gc_node_degree(&self->g, (gc_node_id)node, &deg)) {
//         PyErr_SetString(PyExc_IndexError, "invalid node");
//         return NULL;
//     }
//
//     gc_edge_iter it;
//     if (gc_edge_iter_begin(&self->g, (gc_node_id)node, &it)) {
//         PyErr_SetString(PyExc_IndexError, "invalid node");
//         return NULL;
//     }
//
//     /* preallocate exact size */
//     PyObject* list = PyList_New((Py_ssize_t)deg);
//     if (!list) {
//         return NULL;
//     }
//
//     gc_edge e;
//     Py_ssize_t i = 0;
//
//     while (gc_edge_iter_next(&it, &e)) {
//         PyObject* t = Py_BuildValue("(kk)", e.dst, e.weight);
//         if (!t) {
//             Py_DECREF(list);
//             return NULL;
//         }
//         /* steals reference */
//         PyList_SET_ITEM(list, i++, t);
//     }
//
//     return list;
// }
//
// /* ---------- method table ---------- */
//
// static PyMethodDef methods[] = {{"node_count", (PyCFunction)PyGraph_node_count, METH_NOARGS, ""},
//                                 {"degree", (PyCFunction)PyGraph_degree, METH_VARARGS, ""},
//                                 {"edges", (PyCFunction)PyGraph_edges, METH_VARARGS, ""},
//                                 {NULL, NULL, 0, NULL}};
//
// /* ---------- ABI3 heap type ---------- */
//
// static PyType_Slot slots[] = {{Py_tp_init, PyGraph_init},
//                               {Py_tp_dealloc, PyGraph_dealloc},
//                               {Py_tp_methods, methods},
//                               {Py_tp_new, PyType_GenericNew},
//                               {0, NULL}};
//
static PyType_Spec const graph_spec = {
    .name = "graphcore.Graph",
    // .basicsize = sizeof(PyGraph),
    .flags = Py_TPFLAGS_DEFAULT,
    // .slots = slots,
};

// The actual hello() function implementation
static PyObject*
cache_hello(PyObject* self [[maybe_unused]], PyObject* args [[maybe_unused]])
{
    return PyUnicode_FromString("Hello from the C Extension!");
}

// double func_I_want_to_use(struct....., int const a, double x, double y);

static PyObject*
myfunc(PyObject* Py_UNUSED(dummy), PyObject* args [[maybe_unused]])
{
    return PyUnicode_FromString("myfunc from the C Extension!");
    // PyObject* input_dict = nullptr;
    // double result = 0.0;
    // ....
    // if (!PyArg_ParseTuple(args, "O!...",
    //     &PyDict_Type, (PyObject **)&input_dict,  // O!
    //     ....
    // ) {
    //     return nullptr;
    // }
    // ...
    // result = func_I_want_to_use(......)
    // ...
    // return Py_BuildValue(....)
}

static char const doc_myfunc[] = ("This is my wrapped function...");

/* ---------- module ---------- */

// Define the methods for this module
static struct PyMethodDef const mylib_module_methods[] = {
    //     {"get", (PyCFunction)miur_get, METH_VARARGS, "Get a value from cache"},
    {"hello", cache_hello, METH_NOARGS, "Returns a greeting from the C implementation."},
    {"myfunc", myfunc, METH_VARARGS, doc_myfunc},
    {nullptr, nullptr, 0, nullptr},  // Sentinel
};

static int
graphcore_exec(PyObject* mod)
{
    PyObject* type = PyType_FromSpec((PyType_Spec*)&graph_spec);

    if (!type) {
        goto error;
    }
    if (PyModule_AddObjectRef(mod, "Graph", type) < 0) {
        goto error;
    }

    Py_DECREF(type);
    return 0;

error:
    Py_XDECREF(type);
    return -1;
}

// CHECK: Your module supports per-interpreter GIL if it has no process-wide shared mutable state.
//   * no static/global C variables
//   * No module-level caches or singletons
//   * No static PyObject* storing types/exceptions between calls
//   * Types created via PyType_FromSpec (heap types) ✓ — static PyTypeObject ❌
//   * Library is stateless or context-based (no own global state e.g. OpenSSL, libxml2)
static PyModuleDef_Slot const mylib_module_slots[] = {
    {Py_mod_exec, graphcore_exec},
    // {Py_mod_multiple_interpreters, Py_MOD_PER_INTERPRETER_GIL_SUPPORTED},
    // {Py_mod_gil, Py_MOD_GIL_NOT_USED},
    {0, nullptr},
};

//// ALT: set .m_size = sizeof(module_state) and access it via PyModule_GetState
// typedef struct {
//     PyObject *Graph_type;   // cached type object
//     PyObject *some_exception;
// } module_state;
// // accessed via:
// module_state *state = PyModule_GetState(m);
// state->Graph_type = ...;
static PyModuleDef const moduledef = {
    .m_base = PyModuleDef_HEAD_INIT,
    .m_name = "_ext_cc",                              // Module name
    .m_doc = "Core C23 ABI3 impl for miurcache",      // Module docstring
    .m_size = 0,                                      // Size of per-interpreter state of the module
    .m_methods = (PyMethodDef*)mylib_module_methods,  // Methods array
    .m_slots = (PyModuleDef_Slot*)mylib_module_slots,
};

// Module initialization function (Must match the OUTPUT_NAME in CMake)
PyMODINIT_FUNC
PyInit__ext_cc(void)
{
    // import_array();
    return PyModuleDef_Init((PyModuleDef*)&moduledef);
}
