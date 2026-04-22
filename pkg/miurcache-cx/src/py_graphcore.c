// Important ABI3 Constraints
// Struct Access: You cannot access PyObject fields directly (e.g.,
// obj->ob_type). You must use getter functions like Py_TYPE(obj).
#define Py_LIMITED_API 0x030E0000 // Stable ABI3 for python>=3.14
#include "graphcore.h"
#include <Python.h>

typedef struct {
  PyObject_HEAD Py_buffer view;
  gc_graph g;
} PyGraph;

/* ---------- lifecycle ---------- */

static int PyGraph_init(PyGraph *self, PyObject *args, PyObject *kw) {
  PyObject *obj;

  if (!PyArg_ParseTuple(args, "O", &obj))
    return -1;

  if (PyObject_GetBuffer(obj, &self->view, PyBUF_SIMPLE) < 0)
    return -1;

  if (gc_graph_init(&self->g, self->view.buf, self->view.len) ||
      gc_graph_validate(&self->g)) {
    PyBuffer_Release(&self->view);
    PyErr_SetString(PyExc_ValueError, "invalid graph buffer");
    return -1;
  }

  return 0;
}

static void PyGraph_dealloc(PyGraph *self) {
  PyBuffer_Release(&self->view);
  Py_TYPE(self)->tp_free((PyObject *)self);
}

/* ---------- methods ---------- */

static PyObject *PyGraph_node_count(PyGraph *self,
                                    PyObject *Py_UNUSED(ignored)) {
  return PyLong_FromSize_t(gc_node_count(&self->g));
}

static PyObject *PyGraph_degree(PyGraph *self, PyObject *args) {
  unsigned long node;
  if (!PyArg_ParseTuple(args, "k", &node))
    return NULL;

  size_t deg;
  if (gc_node_degree(&self->g, (gc_node_id)node, &deg)) {
    PyErr_SetString(PyExc_IndexError, "invalid node");
    return NULL;
  }

  return PyLong_FromSize_t(deg);
}

static PyObject *PyGraph_edges(PyGraph *self, PyObject *args) {
  unsigned long node;
  if (!PyArg_ParseTuple(args, "k", &node))
    return NULL;

  size_t deg;
  if (gc_node_degree(&self->g, (gc_node_id)node, &deg)) {
    PyErr_SetString(PyExc_IndexError, "invalid node");
    return NULL;
  }

  gc_edge_iter it;
  if (gc_edge_iter_begin(&self->g, (gc_node_id)node, &it)) {
    PyErr_SetString(PyExc_IndexError, "invalid node");
    return NULL;
  }

  /* preallocate exact size */
  PyObject *list = PyList_New((Py_ssize_t)deg);
  if (!list)
    return NULL;

  gc_edge e;
  Py_ssize_t i = 0;

  while (gc_edge_iter_next(&it, &e)) {
    PyObject *t = Py_BuildValue("(kk)", e.dst, e.weight);
    if (!t) {
      Py_DECREF(list);
      return NULL;
    }
    /* steals reference */
    PyList_SET_ITEM(list, i++, t);
  }

  return list;
}

/* ---------- method table ---------- */

static PyMethodDef methods[] = {
    {"node_count", (PyCFunction)PyGraph_node_count, METH_NOARGS, ""},
    {"degree", (PyCFunction)PyGraph_degree, METH_VARARGS, ""},
    {"edges", (PyCFunction)PyGraph_edges, METH_VARARGS, ""},
    {NULL, NULL, 0, NULL}};

/* ---------- ABI3 heap type ---------- */

static PyType_Slot slots[] = {{Py_tp_init, PyGraph_init},
                              {Py_tp_dealloc, PyGraph_dealloc},
                              {Py_tp_methods, methods},
                              {Py_tp_new, PyType_GenericNew},
                              {0, NULL}};

static PyType_Spec spec = {
    .name = "graphcore.Graph",
    .basicsize = sizeof(PyGraph),
    .flags = Py_TPFLAGS_DEFAULT,
    .slots = slots,
};

/* ---------- module ---------- */

// static PyMethodDef MiurMethods[] = {
//     {"get", (PyCFunction)miur_get, METH_VARARGS, "Get a value from cache"},
//     {NULL, NULL, 0, NULL}
// };

static struct PyModuleDef module = {
    PyModuleDef_HEAD_INIT,

    // BET? conventional -- to keep it inside same module as shim
    // "_miurcache",
    "miurcache_cx",
    "C23 ABI3 High-speed implementation",
    NULL,
    -1,
    NULL // MiurMethods
};

PyMODINIT_FUNC PyInit_graphcore(void) {
  PyObject *m = PyModule_Create(&module);
  if (!m)
    return NULL;

  PyObject *type = PyType_FromSpec(&spec);
  if (!type) {
    Py_DECREF(m);
    return NULL;
  }

  if (PyModule_AddObject(m, "Graph", type) < 0) {
    Py_DECREF(type);
    Py_DECREF(m);
    return NULL;
  }

  return m;
}
