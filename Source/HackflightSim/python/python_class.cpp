/*
Generic C++ class for wrapping Python classes

Copyright 2018 Simon D. Levy

MIT License
*/

#include "python_class.h"

#ifdef _PYTHON

PythonClass::PythonClass(const char * moduleName, const char * className)
{
    // Initialize Python access
    Py_Initialize();

    // Make sure we can access Python modules in the current directory
    PyRun_SimpleString("import sys");
    PyRun_SimpleString("sys.path.append(\".\")");

    // Build the name object
    PyObject * pName = PyUnicode_FromString(moduleName);

    // Load the module object
    PyObject * pModule = PyImport_Import(pName);
    if (pModule == NULL) {
        fprintf(stderr, "Error loading module %s\n", moduleName);
		while (true);
    }

    // pDict is a borrowed reference 
    PyObject * pDict = PyModule_GetDict(pModule);

    // Build the name of a callable class 
    _pClass = PyDict_GetItemString(pDict, className);

    // Ensure class is callable
    if (!PyCallable_Check(_pClass)) {
        fprintf(stderr, "%s is not a callable class\n", className);
		while (true);
    }
}

#endif
