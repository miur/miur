// SRC:
// https://discuss.python.org/t/adding-extension-module-examples-to-the-packaging-user-guide/105111/10
#define PY_SSIZE_T_CLEAN
#include "Python.h"
#include "libIwanttolinkto.h"

struct {
  ....
} the_struct_C_lib_exposes_t;

double func_I_want_to_use(struct....., const int a, double x, double y);

static PyObject *myfunc(PyObject *Py_UNUSED(dummy), PyObject *args) {
  PyObject *input_dict = NULL;
  double result = 0.0;
    ....
    if (!PyArg_ParseTuple(args, "O!...",
        &PyDict_Type, (PyObject **)&input_dict,  // O!
        ....
    ) {
    return NULL;
    }
    ...
    result = func_I_want_to_use(......)
    ...
    return Py_BuildValue(....)
}

static char doc_myfunc[] = ("This is my wrapped function...");

static struct PyMethodDef mylib_module_methods[] = {
    {"myfunc", myfunc, METH_VARARGS, doc_myfunc}, {NULL, NULL, 0, NULL}};

static struct PyModuleDef_Slot mylib_module_slots[] = {
    {Py_mod_multiple_interpreters, Py_MOD_PER_INTERPRETER_GIL_SUPPORTED},
    {Py_mod_gil, Py_MOD_GIL_NOT_USED},
    {0, NULL},
};

static struct PyModuleDef moduledef = {
    .m_base = PyModuleDef_HEAD_INIT,
    .m_name = "mylib",
    .m_size = 0,
    .m_methods = mylib_module_methods,
    .m_slots = mylib_module_slots,
};

PyMODINIT_FUNC PyInit_mylib(void) {
  import_array();
  return PyModuleDef_Init(&moduledef);
}
