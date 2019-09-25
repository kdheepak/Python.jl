# MIT License
# Copyright (c) 2019 Dheepak Krishnamurthy

module Python

# utils

macro embed(typename)
    t = getfield(@__MODULE__, typename)
    e = Expr(:block, [])
    for (x, y) in zip(fieldnames(t), fieldtypes(t))
        push!(e.args, :($x::$y))
    end
    return esc(e)
end

# Python.jl interface

const Py_ssize_t = Clong

# PyObject

mutable struct PyObject
    ob_refcnt::Py_ssize_t
    ob_type::Ptr{Cvoid}
end

# PyVarObject

mutable struct PyVarObject
    @embed PyObject
    ob_size::Py_ssize_t
end

# PyMethodDef

"""Base.@ccallable PyCFunction(::Ptr{PyObject}, ::Ptr{PyObject})::Ptr{PyObject} end"""
const PyCFunction = Ptr{Cvoid}

mutable struct PyMethodDef
    ml_name::Ptr{Cchar}
    ml_meth::PyCFunction
    ml_flags::Cint
    ml_doc::Ptr{Cchar}
end

# PyMemberDef

mutable struct PyMemberDef
    name::Ptr{Cchar}
    type::Cint
    offset::Py_ssize_t
    flags::Cint
    doc::Ptr{Cchar}
end

# PyGetSetDef

"""Base.@ccallable getter(::Ptr{PyObject}, ::Ptr{PyObject})::Ptr{PyObject}"""
const getter = Ptr{Cvoid}

"""Base.@ccallable setter(::Ptr{PyObject}, ::Ptr{PyObject}, Ptr{Cvoid})::Cint"""
const setter = Ptr{Cvoid}

mutable struct PyGetSetDef
    name::Ptr{Cchar}
    get::getter
    set::setter
    doc::Ptr{Cchar}
    closure::Ptr{Cvoid}
end

# PyModuleDef

"""Base.@ccallable _m_init()::Ptr{PyObject}"""
const _m_init = Ptr{Cvoid}

mutable struct PyModuleDef_Base
    @embed PyObject
    m_init::_m_init
    m_index::Py_ssize_t
    m_copy::Ptr{PyObject}
end

mutable struct PyModuleDef_Slot
    slot::Cint
    value::Ptr{Cvoid}
end

"""Base.@ccallable inquiry(::Ptr{PyObject})::Cint"""
const inquiry = Ptr{Cvoid}

"""Base.@ccallable traverseproc(::Ptr{PyObject}, ::visitproc, ::Ptr{Cvoid})::Cint"""
const traverseproc = Ptr{Cvoid}

"""Base.@ccallable freefunc(Ptr{Cvoid})::Ptr{Cvoid}"""
const freefunc = Ptr{Cvoid}

mutable struct PyModuleDef
    @embed PyModuleDef_Base
    m_doc::Cstring
    m_size::Py_ssize_t
    m_methods::Ptr{PyMethodDef}
    m_slots::Ptr{PyModuleDef_Slot}
    m_traverse::traverseproc
    m_clear::inquiry
    m_free::freefunc
end

# PyStructSequence_Field

mutable struct PyStructSequence_Field
    name::Ptr{Cchar}
    doc::Ptr{Cchar}
end

# PyStructSequence_Desc

mutable struct PyStructSequence_Desc
    name::Ptr{Cchar}
    doc::Ptr{Cchar}
    fields::Ptr{PyStructSequence_Field}
    n_in_sequence::Cint
end

# PyType_Slot

mutable struct PyType_Slot
    slot::Cint
    pfunc::Ptr{Cvoid}
end

# PyType_Spec

mutable struct PyType_Spec
    name::Ptr{Cchar}
    basicsize::Cint
    itemsize::Cint
    flags::Cuint
    slots::Ptr{PyType_Slot}
end

# Py_REFCNT

Py_REFCNT(ob::PyObject) = ob.ob_refcnt
Py_TYPE(ob::PyObject) = ob.ob_type
Py_SIZE(ob::PyVarObject) = ob.ob_size

const PyThreadState = Cvoid
const PyInterpreterState = Cvoid
const _frame = Cvoid
const symtable = Cvoid
const _node = Cvoid
const PyWeakReference = Cvoid
const PyLongObject = Cvoid
const PyTypeObject = Cvoid


PyAST_Check()                                         = error("Not implemented.")
PyAST_Compile()                                       = error("Not implemented.")
PyAST_CompileEx()                                     = error("Not implemented.")
PyAST_CompileObject()                                 = error("Not implemented.")
PyAST_FromNode()                                      = error("Not implemented.")
PyAST_FromNodeObject()                                = error("Not implemented.")
PyAST_Validate()                                      = error("Not implemented.")
PyAST_mod2obj()                                       = error("Not implemented.")
PyAST_obj2mod()                                       = error("Not implemented.")
PyArena_AddPyObject()                                 = error("Not implemented.")
PyArena_Free()                                        = error("Not implemented.")
PyArena_Malloc()                                      = error("Not implemented.")
PyArena_New()                                         = error("Not implemented.")
PyArg_Parse()                                         = error("Not implemented.")
PyArg_ParseTuple()                                    = error("Not implemented.")
PyArg_ParseTupleAndKeywords()                         = error("Not implemented.")
PyArg_UnpackTuple()                                   = error("Not implemented.")
PyArg_VaParse()                                       = error("Not implemented.")
PyArg_VaParseTupleAndKeywords()                       = error("Not implemented.")
PyArg_ValidateKeywordArguments()                      = error("Not implemented.")
PyAsyncGen_ClearFreeLists()                           = error("Not implemented.")
PyAsyncGen_Fini()                                     = error("Not implemented.")
PyAsyncGen_New()                                      = error("Not implemented.")
PyBool_FromLong()                                     = error("Not implemented.")
PyBuffer_FillContiguousStrides()                      = error("Not implemented.")
PyBuffer_FillInfo()                                   = error("Not implemented.")
PyBuffer_FromContiguous()                             = error("Not implemented.")
PyBuffer_GetPointer()                                 = error("Not implemented.")
PyBuffer_IsContiguous()                               = error("Not implemented.")
PyBuffer_Release()                                    = error("Not implemented.")
PyBuffer_ToContiguous()                               = error("Not implemented.")
PyByteArray_AsString()                                = error("Not implemented.")
PyByteArray_Concat()                                  = error("Not implemented.")
PyByteArray_Fini()                                    = error("Not implemented.")
PyByteArray_FromObject()                              = error("Not implemented.")
PyByteArray_FromStringAndSize()                       = error("Not implemented.")
PyByteArray_Init()                                    = error("Not implemented.")
PyByteArray_Resize()                                  = error("Not implemented.")
PyByteArray_Size()                                    = error("Not implemented.")
PyBytes_AsString()                                    = error("Not implemented.")
PyBytes_AsStringAndSize()                             = error("Not implemented.")
PyBytes_Concat()                                      = error("Not implemented.")
PyBytes_ConcatAndDel()                                = error("Not implemented.")
PyBytes_DecodeEscape()                                = error("Not implemented.")
PyBytes_Fini()                                        = error("Not implemented.")
PyBytes_FromFormat()                                  = error("Not implemented.")
PyBytes_FromFormatV()                                 = error("Not implemented.")
PyBytes_FromObject()                                  = error("Not implemented.")
PyBytes_FromString()                                  = error("Not implemented.")
PyBytes_FromStringAndSize()                           = error("Not implemented.")
PyBytes_Repr()                                        = error("Not implemented.")
PyBytes_Size()                                        = error("Not implemented.")
PyCFunction_Call()                                    = error("Not implemented.")
PyCFunction_ClearFreeList()                           = error("Not implemented.")
PyCFunction_Fini()                                    = error("Not implemented.")
PyCFunction_GetFlags()                                = error("Not implemented.")
PyCFunction_GetFunction()                             = error("Not implemented.")
PyCFunction_GetSelf()                                 = error("Not implemented.")
PyCFunction_New()                                     = error("Not implemented.")
PyCFunction_NewEx()                                   = error("Not implemented.")
PyCallIter_New()                                      = error("Not implemented.")
PyCallable_Check()                                    = error("Not implemented.")
PyCapsule_GetContext()                                = error("Not implemented.")
PyCapsule_GetDestructor()                             = error("Not implemented.")
PyCapsule_GetName()                                   = error("Not implemented.")
PyCapsule_GetPointer()                                = error("Not implemented.")
PyCapsule_Import()                                    = error("Not implemented.")
PyCapsule_IsValid()                                   = error("Not implemented.")
PyCapsule_New()                                       = error("Not implemented.")
PyCapsule_SetContext()                                = error("Not implemented.")
PyCapsule_SetDestructor()                             = error("Not implemented.")
PyCapsule_SetName()                                   = error("Not implemented.")
PyCapsule_SetPointer()                                = error("Not implemented.")
PyCell_Get()                                          = error("Not implemented.")
PyCell_New()                                          = error("Not implemented.")
PyCell_Set()                                          = error("Not implemented.")
PyClassMethod_New()                                   = error("Not implemented.")
PyCode_Addr2Line()                                    = error("Not implemented.")
PyCode_New()                                          = error("Not implemented.")
PyCode_NewEmpty()                                     = error("Not implemented.")
PyCode_Optimize()                                     = error("Not implemented.")
PyCodec_BackslashReplaceErrors()                      = error("Not implemented.")
PyCodec_Decode()                                      = error("Not implemented.")
PyCodec_Decoder()                                     = error("Not implemented.")
PyCodec_Encode()                                      = error("Not implemented.")
PyCodec_Encoder()                                     = error("Not implemented.")
PyCodec_IgnoreErrors()                                = error("Not implemented.")
PyCodec_IncrementalDecoder()                          = error("Not implemented.")
PyCodec_IncrementalEncoder()                          = error("Not implemented.")
PyCodec_KnownEncoding()                               = error("Not implemented.")
PyCodec_LookupError()                                 = error("Not implemented.")
PyCodec_NameReplaceErrors()                           = error("Not implemented.")
PyCodec_Register()                                    = error("Not implemented.")
PyCodec_RegisterError()                               = error("Not implemented.")
PyCodec_ReplaceErrors()                               = error("Not implemented.")
PyCodec_StreamReader()                                = error("Not implemented.")
PyCodec_StreamWriter()                                = error("Not implemented.")
PyCodec_StrictErrors()                                = error("Not implemented.")
PyCodec_XMLCharRefReplaceErrors()                     = error("Not implemented.")
PyCompileString()                                     = error("Not implemented.")
PyCompile_OpcodeStackEffect()                         = error("Not implemented.")
PyComplex_AsCComplex()                                = error("Not implemented.")
PyComplex_FromCComplex()                              = error("Not implemented.")
PyComplex_FromDoubles()                               = error("Not implemented.")
PyComplex_ImagAsDouble()                              = error("Not implemented.")
PyComplex_RealAsDouble()                              = error("Not implemented.")
PyContextVar_Get()                                    = error("Not implemented.")
PyContextVar_New()                                    = error("Not implemented.")
PyContextVar_Reset()                                  = error("Not implemented.")
PyContextVar_Set()                                    = error("Not implemented.")
PyContext_ClearFreeList()                             = error("Not implemented.")
PyContext_Copy()                                      = error("Not implemented.")
PyContext_CopyCurrent()                               = error("Not implemented.")
PyContext_Enter()                                     = error("Not implemented.")
PyContext_Exit()                                      = error("Not implemented.")
PyContext_New()                                       = error("Not implemented.")
PyCoro_New()                                          = error("Not implemented.")
PyDescr_NewClassMethod()                              = error("Not implemented.")
PyDescr_NewGetSet()                                   = error("Not implemented.")
PyDescr_NewMember()                                   = error("Not implemented.")
PyDescr_NewMethod()                                   = error("Not implemented.")
PyDescr_NewWrapper()                                  = error("Not implemented.")
PyDictProxy_New()                                     = error("Not implemented.")
PyDict_Clear()                                        = error("Not implemented.")
PyDict_ClearFreeList()                                = error("Not implemented.")
PyDict_Contains()                                     = error("Not implemented.")
PyDict_Copy()                                         = error("Not implemented.")
PyDict_DelItem()                                      = error("Not implemented.")
PyDict_DelItemString()                                = error("Not implemented.")
PyDict_Fini()                                         = error("Not implemented.")
PyDict_GetItem()                                      = error("Not implemented.")
PyDict_GetItemString()                                = error("Not implemented.")
PyDict_GetItemWithError()                             = error("Not implemented.")
PyDict_Items()                                        = error("Not implemented.")
PyDict_Keys()                                         = error("Not implemented.")
PyDict_Merge()                                        = error("Not implemented.")
PyDict_MergeFromSeq2()                                = error("Not implemented.")
PyDict_New()                                          = error("Not implemented.")
PyDict_Next()                                         = error("Not implemented.")
PyDict_SetDefault()                                   = error("Not implemented.")
PyDict_SetItem()                                      = error("Not implemented.")
PyDict_SetItemString()                                = error("Not implemented.")
PyDict_Size()                                         = error("Not implemented.")
PyDict_Update()                                       = error("Not implemented.")
PyDict_Values()                                       = error("Not implemented.")
PyErr_BadArgument()                                   = error("Not implemented.")
PyErr_BadInternalCall()                               = error("Not implemented.")
PyErr_CheckSignals()                                  = error("Not implemented.")
PyErr_Clear()                                         = error("Not implemented.")
PyErr_Display()                                       = error("Not implemented.")
PyErr_ExceptionMatches()                              = error("Not implemented.")
PyErr_Fetch()                                         = error("Not implemented.")
PyErr_Format()                                        = error("Not implemented.")
PyErr_FormatV()                                       = error("Not implemented.")
PyErr_GetExcInfo()                                    = error("Not implemented.")
PyErr_GivenExceptionMatches()                         = error("Not implemented.")
PyErr_NewException()                                  = error("Not implemented.")
PyErr_NewExceptionWithDoc()                           = error("Not implemented.")
PyErr_NoMemory()                                      = error("Not implemented.")
PyErr_NormalizeException()                            = error("Not implemented.")
PyErr_Occurred()                                      = error("Not implemented.")
PyErr_Print()                                         = error("Not implemented.")
PyErr_PrintEx()                                       = error("Not implemented.")
PyErr_ProgramText()                                   = error("Not implemented.")
PyErr_ProgramTextObject()                             = error("Not implemented.")
PyErr_ResourceWarning()                               = error("Not implemented.")
PyErr_Restore()                                       = error("Not implemented.")
PyErr_SetExcInfo()                                    = error("Not implemented.")
PyErr_SetFromErrno()                                  = error("Not implemented.")
PyErr_SetFromErrnoWithFilename()                      = error("Not implemented.")
PyErr_SetFromErrnoWithFilenameObject()                = error("Not implemented.")
PyErr_SetFromErrnoWithFilenameObjects()               = error("Not implemented.")
PyErr_SetImportError()                                = error("Not implemented.")
PyErr_SetImportErrorSubclass()                        = error("Not implemented.")
PyErr_SetInterrupt()                                  = error("Not implemented.")
PyErr_SetNone()                                       = error("Not implemented.")
PyErr_SetObject()                                     = error("Not implemented.")
PyErr_SetString()                                     = error("Not implemented.")
PyErr_SyntaxLocation()                                = error("Not implemented.")
PyErr_SyntaxLocationEx()                              = error("Not implemented.")
PyErr_SyntaxLocationObject()                          = error("Not implemented.")
PyErr_Warn()                                          = error("Not implemented.")
PyErr_WarnEx()                                        = error("Not implemented.")
PyErr_WarnExplicit()                                  = error("Not implemented.")
PyErr_WarnExplicitFormat()                            = error("Not implemented.")
PyErr_WarnExplicitObject()                            = error("Not implemented.")
PyErr_WarnFormat()                                    = error("Not implemented.")
PyErr_WriteUnraisable()                               = error("Not implemented.")
PyEval_AcquireLock()                                  = error("Not implemented.")
PyEval_AcquireThread()                                = error("Not implemented.")
PyEval_CallFunction()                                 = error("Not implemented.")
PyEval_CallMethod()                                   = error("Not implemented.")
PyEval_CallObjectWithKeywords()                       = error("Not implemented.")
PyEval_EvalCode()                                     = error("Not implemented.")
PyEval_EvalCodeEx()                                   = error("Not implemented.")
PyEval_EvalFrame()                                    = error("Not implemented.")
PyEval_EvalFrameEx()                                  = error("Not implemented.")
PyEval_GetBuiltins()                                  = error("Not implemented.")
PyEval_GetFrame()                                     = error("Not implemented.")
PyEval_GetFuncDesc()                                  = error("Not implemented.")
PyEval_GetFuncName()                                  = error("Not implemented.")
PyEval_GetGlobals()                                   = error("Not implemented.")
PyEval_GetLocals()                                    = error("Not implemented.")
PyEval_InitThreads()                                  = error("Not implemented.")
PyEval_MergeCompilerFlags()                           = error("Not implemented.")
PyEval_ReInitThreads()                                = error("Not implemented.")
PyEval_ReleaseLock()                                  = error("Not implemented.")
PyEval_ReleaseThread()                                = error("Not implemented.")
PyEval_RestoreThread()                                = error("Not implemented.")
PyEval_SaveThread()                                   = error("Not implemented.")
PyEval_SetProfile()                                   = error("Not implemented.")
PyEval_SetTrace()                                     = error("Not implemented.")
PyEval_ThreadsInitialized()                           = error("Not implemented.")
PyException_GetCause()                                = error("Not implemented.")
PyException_GetContext()                              = error("Not implemented.")
PyException_GetTraceback()                            = error("Not implemented.")
PyException_SetCause()                                = error("Not implemented.")
PyException_SetContext()                              = error("Not implemented.")
PyException_SetTraceback()                            = error("Not implemented.")
PyFPE_dummy()                                         = error("Not implemented.")
PyFile_FromFd()                                       = error("Not implemented.")
PyFile_GetLine()                                      = error("Not implemented.")
PyFile_NewStdPrinter()                                = error("Not implemented.")
PyFile_WriteObject()                                  = error("Not implemented.")
PyFile_WriteString()                                  = error("Not implemented.")
PyFloat_AsDouble()                                    = error("Not implemented.")
PyFloat_ClearFreeList()                               = error("Not implemented.")
PyFloat_Fini()                                        = error("Not implemented.")
PyFloat_FromDouble()                                  = error("Not implemented.")
PyFloat_FromString()                                  = error("Not implemented.")
PyFloat_GetInfo()                                     = error("Not implemented.")
PyFloat_GetMax()                                      = error("Not implemented.")
PyFloat_GetMin()                                      = error("Not implemented.")
PyFrame_BlockPop()                                    = error("Not implemented.")
PyFrame_BlockSetup()                                  = error("Not implemented.")
PyFrame_ClearFreeList()                               = error("Not implemented.")
PyFrame_FastToLocals()                                = error("Not implemented.")
PyFrame_FastToLocalsWithError()                       = error("Not implemented.")
PyFrame_Fini()                                        = error("Not implemented.")
PyFrame_GetLineNumber()                               = error("Not implemented.")
PyFrame_LocalsToFast()                                = error("Not implemented.")
PyFrame_New()                                         = error("Not implemented.")
PyFrozenSet_New()                                     = error("Not implemented.")
PyFunction_GetAnnotations()                           = error("Not implemented.")
PyFunction_GetClosure()                               = error("Not implemented.")
PyFunction_GetCode()                                  = error("Not implemented.")
PyFunction_GetDefaults()                              = error("Not implemented.")
PyFunction_GetGlobals()                               = error("Not implemented.")
PyFunction_GetKwDefaults()                            = error("Not implemented.")
PyFunction_GetModule()                                = error("Not implemented.")
PyFunction_New()                                      = error("Not implemented.")
PyFunction_NewWithQualName()                          = error("Not implemented.")
PyFunction_SetAnnotations()                           = error("Not implemented.")
PyFunction_SetClosure()                               = error("Not implemented.")
PyFunction_SetDefaults()                              = error("Not implemented.")
PyFunction_SetKwDefaults()                            = error("Not implemented.")
PyFuture_FromAST()                                    = error("Not implemented.")
PyFuture_FromASTObject()                              = error("Not implemented.")
PyGC_Collect()                                        = error("Not implemented.")
PyGILState_Check()                                    = error("Not implemented.")
PyGILState_Ensure()                                   = error("Not implemented.")
PyGILState_GetThisThreadState()                       = error("Not implemented.")
PyGILState_Release()                                  = error("Not implemented.")
PyGen_NeedsFinalizing()                               = error("Not implemented.")
PyGen_New()                                           = error("Not implemented.")
PyGen_NewWithQualName()                               = error("Not implemented.")
PyGrammar_AddAccelerators()                           = error("Not implemented.")
PyGrammar_FindDFA()                                   = error("Not implemented.")
PyGrammar_LabelRepr()                                 = error("Not implemented.")
PyGrammar_RemoveAccelerators()                        = error("Not implemented.")
PyHash_GetFuncDef()                                   = error("Not implemented.")
PyImport_AddModule()                                  = error("Not implemented.")
PyImport_AddModuleObject()                            = error("Not implemented.")
PyImport_AppendInittab()                              = error("Not implemented.")
PyImport_Cleanup()                                    = error("Not implemented.")
PyImport_ExecCodeModule()                             = error("Not implemented.")
PyImport_ExecCodeModuleEx()                           = error("Not implemented.")
PyImport_ExecCodeModuleObject()                       = error("Not implemented.")
PyImport_ExecCodeModuleWithPathnames()                = error("Not implemented.")
PyImport_ExtendInittab()                              = error("Not implemented.")
PyImport_GetImporter()                                = error("Not implemented.")
PyImport_GetMagicNumber()                             = error("Not implemented.")
PyImport_GetMagicTag()                                = error("Not implemented.")
PyImport_GetModule()                                  = error("Not implemented.")
PyImport_GetModuleDict()                              = error("Not implemented.")
PyImport_Import()                                     = error("Not implemented.")
PyImport_ImportFrozenModule()                         = error("Not implemented.")
PyImport_ImportFrozenModuleObject()                   = error("Not implemented.")
PyImport_ImportModule()                               = error("Not implemented.")
PyImport_ImportModuleLevel()                          = error("Not implemented.")
PyImport_ImportModuleLevelObject()                    = error("Not implemented.")
PyImport_ImportModuleNoBlock()                        = error("Not implemented.")
PyImport_ReloadModule()                               = error("Not implemented.")
PyInit__abc()                                         = error("Not implemented.")
PyInit__ast()                                         = error("Not implemented.")
PyInit__codecs()                                      = error("Not implemented.")
PyInit__collections()                                 = error("Not implemented.")
PyInit__functools()                                   = error("Not implemented.")
PyInit__imp()                                         = error("Not implemented.")
PyInit__io()                                          = error("Not implemented.")
PyInit__locale()                                      = error("Not implemented.")
PyInit__operator()                                    = error("Not implemented.")
PyInit__signal()                                      = error("Not implemented.")
PyInit__sre()                                         = error("Not implemented.")
PyInit__stat()                                        = error("Not implemented.")
PyInit__string()                                      = error("Not implemented.")
PyInit__symtable()                                    = error("Not implemented.")
PyInit__thread()                                      = error("Not implemented.")
PyInit__tracemalloc()                                 = error("Not implemented.")
PyInit__weakref()                                     = error("Not implemented.")
PyInit_atexit()                                       = error("Not implemented.")
PyInit_errno()                                        = error("Not implemented.")
PyInit_faulthandler()                                 = error("Not implemented.")
PyInit_gc()                                           = error("Not implemented.")
PyInit_itertools()                                    = error("Not implemented.")
PyInit_posix()                                        = error("Not implemented.")
PyInit_pwd()                                          = error("Not implemented.")
PyInit_time()                                         = error("Not implemented.")
PyInit_xxsubtype()                                    = error("Not implemented.")
PyInit_zipimport()                                    = error("Not implemented.")
PyInstanceMethod_Function()                           = error("Not implemented.")
PyInstanceMethod_New()                                = error("Not implemented.")
PyInterpreterState_Clear()                            = error("Not implemented.")
PyInterpreterState_Delete()                           = error("Not implemented.")
PyInterpreterState_GetID()                            = error("Not implemented.")
PyInterpreterState_Head()                             = error("Not implemented.")
PyInterpreterState_Main()                             = error("Not implemented.")
PyInterpreterState_New()                              = error("Not implemented.")
PyInterpreterState_Next()                             = error("Not implemented.")
PyInterpreterState_ThreadHead()                       = error("Not implemented.")
PyIter_Next()                                         = error("Not implemented.")
PyList_Append()                                       = error("Not implemented.")
PyList_AsTuple()                                      = error("Not implemented.")
PyList_ClearFreeList()                                = error("Not implemented.")
PyList_Fini()                                         = error("Not implemented.")
PyList_GetItem()                                      = error("Not implemented.")
PyList_GetSlice()                                     = error("Not implemented.")
PyList_Insert()                                       = error("Not implemented.")
PyList_New()                                          = error("Not implemented.")
PyList_Reverse()                                      = error("Not implemented.")
PyList_SetItem()                                      = error("Not implemented.")
PyList_SetSlice()                                     = error("Not implemented.")
PyList_Size()                                         = error("Not implemented.")
PyList_Sort()                                         = error("Not implemented.")
PyLong_AsDouble()                                     = error("Not implemented.")
PyLong_AsLong()                                       = error("Not implemented.")
PyLong_AsLongAndOverflow()                            = error("Not implemented.")
PyLong_AsLongLong()                                   = error("Not implemented.")
PyLong_AsLongLongAndOverflow()                        = error("Not implemented.")
PyLong_AsSize_t()                                     = error("Not implemented.")
PyLong_AsSsize_t()                                    = error("Not implemented.")
PyLong_AsUnsignedLong()                               = error("Not implemented.")
PyLong_AsUnsignedLongLong()                           = error("Not implemented.")
PyLong_AsUnsignedLongLongMask()                       = error("Not implemented.")
PyLong_AsUnsignedLongMask()                           = error("Not implemented.")
PyLong_AsVoidPtr()                                    = error("Not implemented.")
PyLong_Fini()                                         = error("Not implemented.")
PyLong_FromDouble()                                   = error("Not implemented.")
PyLong_FromLong()                                     = error("Not implemented.")
PyLong_FromLongLong()                                 = error("Not implemented.")
PyLong_FromSize_t()                                   = error("Not implemented.")
PyLong_FromSsize_t()                                  = error("Not implemented.")
PyLong_FromString()                                   = error("Not implemented.")
PyLong_FromUnicode()                                  = error("Not implemented.")
PyLong_FromUnicodeObject()                            = error("Not implemented.")
PyLong_FromUnsignedLong()                             = error("Not implemented.")
PyLong_FromUnsignedLongLong()                         = error("Not implemented.")
PyLong_FromVoidPtr()                                  = error("Not implemented.")
PyLong_GetInfo()                                      = error("Not implemented.")
PyMapping_Check()                                     = error("Not implemented.")
PyMapping_GetItemString()                             = error("Not implemented.")
PyMapping_HasKey()                                    = error("Not implemented.")
PyMapping_HasKeyString()                              = error("Not implemented.")
PyMapping_Items()                                     = error("Not implemented.")
PyMapping_Keys()                                      = error("Not implemented.")
PyMapping_Length()                                    = error("Not implemented.")
PyMapping_SetItemString()                             = error("Not implemented.")
PyMapping_Size()                                      = error("Not implemented.")
PyMapping_Values()                                    = error("Not implemented.")
PyMarshal_Init()                                      = error("Not implemented.")
PyMarshal_ReadLastObjectFromFile()                    = error("Not implemented.")
PyMarshal_ReadLongFromFile()                          = error("Not implemented.")
PyMarshal_ReadObjectFromFile()                        = error("Not implemented.")
PyMarshal_ReadObjectFromString()                      = error("Not implemented.")
PyMarshal_ReadShortFromFile()                         = error("Not implemented.")
PyMarshal_WriteLongToFile()                           = error("Not implemented.")
PyMarshal_WriteObjectToFile()                         = error("Not implemented.")
PyMarshal_WriteObjectToString()                       = error("Not implemented.")
PyMem_Calloc()                                        = error("Not implemented.")
PyMem_Free()                                          = error("Not implemented.")
PyMem_GetAllocator()                                  = error("Not implemented.")
PyMem_Malloc()                                        = error("Not implemented.")
PyMem_RawCalloc()                                     = error("Not implemented.")
PyMem_RawFree()                                       = error("Not implemented.")
PyMem_RawMalloc()                                     = error("Not implemented.")
PyMem_RawRealloc()                                    = error("Not implemented.")
PyMem_Realloc()                                       = error("Not implemented.")
PyMem_SetAllocator()                                  = error("Not implemented.")
PyMem_SetupDebugHooks()                               = error("Not implemented.")
PyMember_GetOne()                                     = error("Not implemented.")
PyMember_SetOne()                                     = error("Not implemented.")
PyMemoryView_FromBuffer()                             = error("Not implemented.")
PyMemoryView_FromMemory()                             = error("Not implemented.")
PyMemoryView_FromObject()                             = error("Not implemented.")
PyMemoryView_GetContiguous()                          = error("Not implemented.")
PyMethod_ClearFreeList()                              = error("Not implemented.")
PyMethod_Fini()                                       = error("Not implemented.")
PyMethod_Function()                                   = error("Not implemented.")
PyMethod_New()                                        = error("Not implemented.")
PyMethod_Self()                                       = error("Not implemented.")
PyModuleDef_Init()                                    = error("Not implemented.")
PyModule_AddFunctions()                               = error("Not implemented.")
PyModule_AddIntConstant()                             = error("Not implemented.")
PyModule_AddObject()                                  = error("Not implemented.")
PyModule_AddStringConstant()                          = error("Not implemented.")
PyModule_Create2()                                    = error("Not implemented.")
PyModule_ExecDef()                                    = error("Not implemented.")
PyModule_FromDefAndSpec2()                            = error("Not implemented.")
PyModule_GetDef()                                     = error("Not implemented.")
PyModule_GetDict()                                    = error("Not implemented.")
PyModule_GetFilename()                                = error("Not implemented.")
PyModule_GetFilenameObject()                          = error("Not implemented.")
PyModule_GetName()                                    = error("Not implemented.")
PyModule_GetNameObject()                              = error("Not implemented.")
PyModule_GetState()                                   = error("Not implemented.")
PyModule_GetWarningsModule()                          = error("Not implemented.")
PyModule_New()                                        = error("Not implemented.")
PyModule_NewObject()                                  = error("Not implemented.")
PyModule_SetDocString()                               = error("Not implemented.")
PyNode_AddChild()                                     = error("Not implemented.")
PyNode_Compile()                                      = error("Not implemented.")
PyNode_Free()                                         = error("Not implemented.")
PyNode_ListTree()                                     = error("Not implemented.")
PyNode_New()                                          = error("Not implemented.")
PyNumber_Absolute()                                   = error("Not implemented.")
PyNumber_Add()                                        = error("Not implemented.")
PyNumber_And()                                        = error("Not implemented.")
PyNumber_AsOff_t()                                    = error("Not implemented.")
PyNumber_AsSsize_t()                                  = error("Not implemented.")
PyNumber_Check()                                      = error("Not implemented.")
PyNumber_Divmod()                                     = error("Not implemented.")
PyNumber_Float()                                      = error("Not implemented.")
PyNumber_FloorDivide()                                = error("Not implemented.")
PyNumber_InMatrixMultiply()                           = error("Not implemented.")
PyNumber_InPlaceAdd()                                 = error("Not implemented.")
PyNumber_InPlaceAnd()                                 = error("Not implemented.")
PyNumber_InPlaceFloorDivide()                         = error("Not implemented.")
PyNumber_InPlaceLshift()                              = error("Not implemented.")
PyNumber_InPlaceMatrixMultiply()                      = error("Not implemented.")
PyNumber_InPlaceMultiply()                            = error("Not implemented.")
PyNumber_InPlaceOr()                                  = error("Not implemented.")
PyNumber_InPlacePower()                               = error("Not implemented.")
PyNumber_InPlaceRemainder()                           = error("Not implemented.")
PyNumber_InPlaceRshift()                              = error("Not implemented.")
PyNumber_InPlaceSubtract()                            = error("Not implemented.")
PyNumber_InPlaceTrueDivide()                          = error("Not implemented.")
PyNumber_InPlaceXor()                                 = error("Not implemented.")
PyNumber_Index()                                      = error("Not implemented.")
PyNumber_Invert()                                     = error("Not implemented.")
PyNumber_Long()                                       = error("Not implemented.")
PyNumber_Lshift()                                     = error("Not implemented.")
PyNumber_MatrixMultiply()                             = error("Not implemented.")
PyNumber_Multiply()                                   = error("Not implemented.")
PyNumber_Negative()                                   = error("Not implemented.")
PyNumber_Or()                                         = error("Not implemented.")
PyNumber_Positive()                                   = error("Not implemented.")
PyNumber_Power()                                      = error("Not implemented.")
PyNumber_Remainder()                                  = error("Not implemented.")
PyNumber_Rshift()                                     = error("Not implemented.")
PyNumber_Subtract()                                   = error("Not implemented.")
PyNumber_ToBase()                                     = error("Not implemented.")
PyNumber_TrueDivide()                                 = error("Not implemented.")
PyNumber_Xor()                                        = error("Not implemented.")
PyODict_DelItem()                                     = error("Not implemented.")
PyODict_New()                                         = error("Not implemented.")
PyODict_SetItem()                                     = error("Not implemented.")
PyOS_AfterFork()                                      = error("Not implemented.")
PyOS_AfterFork_Child()                                = error("Not implemented.")
PyOS_AfterFork_Parent()                               = error("Not implemented.")
PyOS_BeforeFork()                                     = error("Not implemented.")
PyOS_FSPath()                                         = error("Not implemented.")
PyOS_FiniInterrupts()                                 = error("Not implemented.")
PyOS_InitInterrupts()                                 = error("Not implemented.")
PyOS_InterruptOccurred()                              = error("Not implemented.")
PyOS_Readline()                                       = error("Not implemented.")
PyOS_StdioReadline()                                  = error("Not implemented.")
PyOS_double_to_string()                               = error("Not implemented.")
PyOS_getsig()                                         = error("Not implemented.")
PyOS_mystricmp()                                      = error("Not implemented.")
PyOS_mystrnicmp()                                     = error("Not implemented.")
PyOS_setsig()                                         = error("Not implemented.")
PyOS_snprintf()                                       = error("Not implemented.")
PyOS_string_to_double()                               = error("Not implemented.")
PyOS_strtol()                                         = error("Not implemented.")
PyOS_strtoul()                                        = error("Not implemented.")
PyOS_vsnprintf()                                      = error("Not implemented.")
PyObject_ASCII()                                      = error("Not implemented.")
PyObject_AsCharBuffer()                               = error("Not implemented.")
PyObject_AsFileDescriptor()                           = error("Not implemented.")
PyObject_AsReadBuffer()                               = error("Not implemented.")
PyObject_AsWriteBuffer()                              = error("Not implemented.")
PyObject_Bytes()                                      = error("Not implemented.")
PyObject_Call()                                       = error("Not implemented.")
PyObject_CallFinalizer()                              = error("Not implemented.")
PyObject_CallFinalizerFromDealloc()                   = error("Not implemented.")
PyObject_CallFunction()                               = error("Not implemented.")
PyObject_CallFunctionObjArgs()                        = error("Not implemented.")
PyObject_CallMethod()                                 = error("Not implemented.")
PyObject_CallMethodObjArgs()                          = error("Not implemented.")
PyObject_CallObject()                                 = error("Not implemented.")
PyObject_Calloc()                                     = error("Not implemented.")
PyObject_CheckReadBuffer()                            = error("Not implemented.")
PyObject_ClearWeakRefs()                              = error("Not implemented.")
PyObject_CopyData()                                   = error("Not implemented.")
PyObject_DelItem()                                    = error("Not implemented.")
PyObject_DelItemString()                              = error("Not implemented.")
PyObject_Dir()                                        = error("Not implemented.")
PyObject_Format()                                     = error("Not implemented.")
PyObject_Free()                                       = error("Not implemented.")
PyObject_GC_Del()                                     = error("Not implemented.")
PyObject_GC_Track()                                   = error("Not implemented.")
PyObject_GC_UnTrack()                                 = error("Not implemented.")
PyObject_GenericGetAttr()                             = error("Not implemented.")
PyObject_GenericGetDict()                             = error("Not implemented.")
PyObject_GenericSetAttr()                             = error("Not implemented.")
PyObject_GenericSetDict()                             = error("Not implemented.")
PyObject_GetArenaAllocator()                          = error("Not implemented.")
PyObject_GetAttr()                                    = error("Not implemented.")
PyObject_GetAttrString()                              = error("Not implemented.")
PyObject_GetBuffer()                                  = error("Not implemented.")
PyObject_GetItem()                                    = error("Not implemented.")
PyObject_GetIter()                                    = error("Not implemented.")
PyObject_HasAttr()                                    = error("Not implemented.")
PyObject_HasAttrString()                              = error("Not implemented.")
PyObject_Hash()                                       = error("Not implemented.")
PyObject_HashNotImplemented()                         = error("Not implemented.")
PyObject_Init()                                       = error("Not implemented.")
PyObject_InitVar()                                    = error("Not implemented.")
PyObject_IsInstance()                                 = error("Not implemented.")
PyObject_IsSubclass()                                 = error("Not implemented.")
PyObject_IsTrue()                                     = error("Not implemented.")
PyObject_Length()                                     = error("Not implemented.")
PyObject_LengthHint()                                 = error("Not implemented.")
PyObject_Malloc()                                     = error("Not implemented.")
PyObject_Not()                                        = error("Not implemented.")
PyObject_Print()                                      = error("Not implemented.")
PyObject_Realloc()                                    = error("Not implemented.")
PyObject_Repr()                                       = error("Not implemented.")
PyObject_RichCompare()                                = error("Not implemented.")
PyObject_RichCompareBool()                            = error("Not implemented.")
PyObject_SelfIter()                                   = error("Not implemented.")
PyObject_SetArenaAllocator()                          = error("Not implemented.")
PyObject_SetAttr()                                    = error("Not implemented.")
PyObject_SetAttrString()                              = error("Not implemented.")
PyObject_SetItem()                                    = error("Not implemented.")
PyObject_Size()                                       = error("Not implemented.")
PyObject_Str()                                        = error("Not implemented.")
PyObject_Type()                                       = error("Not implemented.")
PyParser_ASTFromFile()                                = error("Not implemented.")
PyParser_ASTFromFileObject()                          = error("Not implemented.")
PyParser_ASTFromString()                              = error("Not implemented.")
PyParser_ASTFromStringObject()                        = error("Not implemented.")
PyParser_AddToken()                                   = error("Not implemented.")
PyParser_ClearError()                                 = error("Not implemented.")
PyParser_Delete()                                     = error("Not implemented.")
PyParser_New()                                        = error("Not implemented.")
PyParser_ParseFile()                                  = error("Not implemented.")
PyParser_ParseFileFlags()                             = error("Not implemented.")
PyParser_ParseFileFlagsEx()                           = error("Not implemented.")
PyParser_ParseFileObject()                            = error("Not implemented.")
PyParser_ParseString()                                = error("Not implemented.")
PyParser_ParseStringFlags()                           = error("Not implemented.")
PyParser_ParseStringFlagsFilename()                   = error("Not implemented.")
PyParser_ParseStringFlagsFilenameEx()                 = error("Not implemented.")
PyParser_ParseStringObject()                          = error("Not implemented.")
PyParser_SetError()                                   = error("Not implemented.")
PyParser_SimpleParseFile()                            = error("Not implemented.")
PyParser_SimpleParseFileFlags()                       = error("Not implemented.")
PyParser_SimpleParseString()                          = error("Not implemented.")
PyParser_SimpleParseStringFilename()                  = error("Not implemented.")
PyParser_SimpleParseStringFlags()                     = error("Not implemented.")
PyParser_SimpleParseStringFlagsFilename()             = error("Not implemented.")
PyRun_AnyFile()                                       = error("Not implemented.")
PyRun_AnyFileEx()                                     = error("Not implemented.")
PyRun_AnyFileExFlags()                                = error("Not implemented.")
PyRun_AnyFileFlags()                                  = error("Not implemented.")
PyRun_File()                                          = error("Not implemented.")
PyRun_FileEx()                                        = error("Not implemented.")
PyRun_FileExFlags()                                   = error("Not implemented.")
PyRun_FileFlags()                                     = error("Not implemented.")
PyRun_InteractiveLoop()                               = error("Not implemented.")
PyRun_InteractiveLoopFlags()                          = error("Not implemented.")
PyRun_InteractiveOne()                                = error("Not implemented.")
PyRun_InteractiveOneFlags()                           = error("Not implemented.")
PyRun_InteractiveOneObject()                          = error("Not implemented.")
PyRun_SimpleFile()                                    = error("Not implemented.")
PyRun_SimpleFileEx()                                  = error("Not implemented.")
PyRun_SimpleFileExFlags()                             = error("Not implemented.")
PyRun_SimpleString()                                  = error("Not implemented.")
PyRun_SimpleStringFlags()                             = error("Not implemented.")
PyRun_String()                                        = error("Not implemented.")
PyRun_StringFlags()                                   = error("Not implemented.")
PyST_GetScope()                                       = error("Not implemented.")
PySeqIter_New()                                       = error("Not implemented.")
PySequence_Check()                                    = error("Not implemented.")
PySequence_Concat()                                   = error("Not implemented.")
PySequence_Contains()                                 = error("Not implemented.")
PySequence_Count()                                    = error("Not implemented.")
PySequence_DelItem()                                  = error("Not implemented.")
PySequence_DelSlice()                                 = error("Not implemented.")
PySequence_Fast()                                     = error("Not implemented.")
PySequence_GetItem()                                  = error("Not implemented.")
PySequence_GetSlice()                                 = error("Not implemented.")
PySequence_In()                                       = error("Not implemented.")
PySequence_InPlaceConcat()                            = error("Not implemented.")
PySequence_InPlaceRepeat()                            = error("Not implemented.")
PySequence_Index()                                    = error("Not implemented.")
PySequence_Length()                                   = error("Not implemented.")
PySequence_List()                                     = error("Not implemented.")
PySequence_Repeat()                                   = error("Not implemented.")
PySequence_SetItem()                                  = error("Not implemented.")
PySequence_SetSlice()                                 = error("Not implemented.")
PySequence_Size()                                     = error("Not implemented.")
PySequence_Tuple()                                    = error("Not implemented.")
PySet_Add()                                           = error("Not implemented.")
PySet_Clear()                                         = error("Not implemented.")
PySet_ClearFreeList()                                 = error("Not implemented.")
PySet_Contains()                                      = error("Not implemented.")
PySet_Discard()                                       = error("Not implemented.")
PySet_Fini()                                          = error("Not implemented.")
PySet_New()                                           = error("Not implemented.")
PySet_Pop()                                           = error("Not implemented.")
PySet_Size()                                          = error("Not implemented.")
PySignal_SetWakeupFd()                                = error("Not implemented.")
PySlice_AdjustIndices()                               = error("Not implemented.")
PySlice_Fini()                                        = error("Not implemented.")
PySlice_GetIndices()                                  = error("Not implemented.")
PySlice_GetIndicesEx()                                = error("Not implemented.")
PySlice_New()                                         = error("Not implemented.")
PySlice_Unpack()                                      = error("Not implemented.")
PyState_AddModule()                                   = error("Not implemented.")
PyState_FindModule()                                  = error("Not implemented.")
PyState_RemoveModule()                                = error("Not implemented.")
PyStaticMethod_New()                                  = error("Not implemented.")
PyStructSequence_GetItem()                            = error("Not implemented.")
PyStructSequence_InitType()                           = error("Not implemented.")
PyStructSequence_InitType2()                          = error("Not implemented.")
PyStructSequence_New()                                = error("Not implemented.")
PyStructSequence_NewType()                            = error("Not implemented.")
PyStructSequence_SetItem()                            = error("Not implemented.")
PySymtable_Build()                                    = error("Not implemented.")
PySymtable_BuildObject()                              = error("Not implemented.")
PySymtable_Free()                                     = error("Not implemented.")
PySymtable_Lookup()                                   = error("Not implemented.")
PySys_AddWarnOption()                                 = error("Not implemented.")
PySys_AddWarnOptionUnicode()                          = error("Not implemented.")
PySys_AddXOption()                                    = error("Not implemented.")
PySys_FormatStderr()                                  = error("Not implemented.")
PySys_FormatStdout()                                  = error("Not implemented.")
PySys_GetObject()                                     = error("Not implemented.")
PySys_GetXOptions()                                   = error("Not implemented.")
PySys_HasWarnOptions()                                = error("Not implemented.")
PySys_ResetWarnOptions()                              = error("Not implemented.")
PySys_SetArgv()                                       = error("Not implemented.")
PySys_SetArgvEx()                                     = error("Not implemented.")
PySys_SetObject()                                     = error("Not implemented.")
PySys_SetPath()                                       = error("Not implemented.")
PySys_WriteStderr()                                   = error("Not implemented.")
PySys_WriteStdout()                                   = error("Not implemented.")
PyThreadState_Clear()                                 = error("Not implemented.")
PyThreadState_Delete()                                = error("Not implemented.")
PyThreadState_DeleteCurrent()                         = error("Not implemented.")
PyThreadState_Get()                                   = error("Not implemented.")
PyThreadState_GetDict()                               = error("Not implemented.")
PyThreadState_New()                                   = error("Not implemented.")
PyThreadState_Next()                                  = error("Not implemented.")
PyThreadState_SetAsyncExc()                           = error("Not implemented.")
PyThreadState_Swap()                                  = error("Not implemented.")
PyThread_GetInfo()                                    = error("Not implemented.")
PyThread_ReInitTLS()                                  = error("Not implemented.")
PyThread_acquire_lock()                               = error("Not implemented.")
PyThread_acquire_lock_timed()                         = error("Not implemented.")
PyThread_allocate_lock()                              = error("Not implemented.")
PyThread_create_key()                                 = error("Not implemented.")
PyThread_delete_key()                                 = error("Not implemented.")
PyThread_delete_key_value()                           = error("Not implemented.")
PyThread_exit_thread()                                = error("Not implemented.")
PyThread_free_lock()                                  = error("Not implemented.")
PyThread_get_key_value()                              = error("Not implemented.")
PyThread_get_stacksize()                              = error("Not implemented.")
PyThread_get_thread_ident()                           = error("Not implemented.")
PyThread_init_thread()                                = error("Not implemented.")
PyThread_release_lock()                               = error("Not implemented.")
PyThread_set_key_value()                              = error("Not implemented.")
PyThread_set_stacksize()                              = error("Not implemented.")
PyThread_start_new_thread()                           = error("Not implemented.")
PyThread_tss_alloc()                                  = error("Not implemented.")
PyThread_tss_create()                                 = error("Not implemented.")
PyThread_tss_delete()                                 = error("Not implemented.")
PyThread_tss_free()                                   = error("Not implemented.")
PyThread_tss_get()                                    = error("Not implemented.")
PyThread_tss_is_created()                             = error("Not implemented.")
PyThread_tss_set()                                    = error("Not implemented.")
PyToken_OneChar()                                     = error("Not implemented.")
PyToken_ThreeChars()                                  = error("Not implemented.")
PyToken_TwoChars()                                    = error("Not implemented.")
PyTokenizer_FindEncoding()                            = error("Not implemented.")
PyTokenizer_FindEncodingFilename()                    = error("Not implemented.")
PyTokenizer_Free()                                    = error("Not implemented.")
PyTokenizer_FromFile()                                = error("Not implemented.")
PyTokenizer_FromString()                              = error("Not implemented.")
PyTokenizer_FromUTF8()                                = error("Not implemented.")
PyTokenizer_Get()                                     = error("Not implemented.")
PyTraceBack_Here()                                    = error("Not implemented.")
PyTraceBack_Print()                                   = error("Not implemented.")
PyTraceMalloc_Track()                                 = error("Not implemented.")
PyTraceMalloc_Untrack()                               = error("Not implemented.")
PyTuple_ClearFreeList()                               = error("Not implemented.")
PyTuple_Fini()                                        = error("Not implemented.")
PyTuple_GetItem()                                     = error("Not implemented.")
PyTuple_GetSlice()                                    = error("Not implemented.")
PyTuple_New()                                         = error("Not implemented.")
PyTuple_Pack()                                        = error("Not implemented.")
PyTuple_SetItem()                                     = error("Not implemented.")
PyTuple_Size()                                        = error("Not implemented.")
PyType_ClearCache()                                   = error("Not implemented.")
PyType_FromSpec(pts::Ptr{PyType_Spec})::Ptr{PyObject} = ccall(:PyType_FromSpec, Ptr{PyObject}, (Ptr{PyType_Spec},), pts)
PyType_FromSpecWithBases()                            = error("Not implemented.")
PyType_GenericAlloc()                                 = error("Not implemented.")
PyType_GenericNew()                                   = error("Not implemented.")
PyType_GetFlags()                                     = error("Not implemented.")
PyType_GetSlot()                                      = error("Not implemented.")
PyType_IsSubtype()                                    = error("Not implemented.")
PyType_Modified()                                     = error("Not implemented.")
PyType_Ready()                                        = error("Not implemented.")
PyUnicodeDecodeError_Create()                         = error("Not implemented.")
PyUnicodeDecodeError_GetEncoding()                    = error("Not implemented.")
PyUnicodeDecodeError_GetEnd()                         = error("Not implemented.")
PyUnicodeDecodeError_GetObject()                      = error("Not implemented.")
PyUnicodeDecodeError_GetReason()                      = error("Not implemented.")
PyUnicodeDecodeError_GetStart()                       = error("Not implemented.")
PyUnicodeDecodeError_SetEnd()                         = error("Not implemented.")
PyUnicodeDecodeError_SetReason()                      = error("Not implemented.")
PyUnicodeDecodeError_SetStart()                       = error("Not implemented.")
PyUnicodeEncodeError_Create()                         = error("Not implemented.")
PyUnicodeEncodeError_GetEncoding()                    = error("Not implemented.")
PyUnicodeEncodeError_GetEnd()                         = error("Not implemented.")
PyUnicodeEncodeError_GetObject()                      = error("Not implemented.")
PyUnicodeEncodeError_GetReason()                      = error("Not implemented.")
PyUnicodeEncodeError_GetStart()                       = error("Not implemented.")
PyUnicodeEncodeError_SetEnd()                         = error("Not implemented.")
PyUnicodeEncodeError_SetReason()                      = error("Not implemented.")
PyUnicodeEncodeError_SetStart()                       = error("Not implemented.")
PyUnicodeTranslateError_Create()                      = error("Not implemented.")
PyUnicodeTranslateError_GetEnd()                      = error("Not implemented.")
PyUnicodeTranslateError_GetObject()                   = error("Not implemented.")
PyUnicodeTranslateError_GetReason()                   = error("Not implemented.")
PyUnicodeTranslateError_GetStart()                    = error("Not implemented.")
PyUnicodeTranslateError_SetEnd()                      = error("Not implemented.")
PyUnicodeTranslateError_SetReason()                   = error("Not implemented.")
PyUnicodeTranslateError_SetStart()                    = error("Not implemented.")
PyUnicode_Append()                                    = error("Not implemented.")
PyUnicode_AppendAndDel()                              = error("Not implemented.")
PyUnicode_AsASCIIString()                             = error("Not implemented.")
PyUnicode_AsCharmapString()                           = error("Not implemented.")
PyUnicode_AsDecodedObject()                           = error("Not implemented.")
PyUnicode_AsDecodedUnicode()                          = error("Not implemented.")
PyUnicode_AsEncodedObject()                           = error("Not implemented.")
PyUnicode_AsEncodedString()                           = error("Not implemented.")
PyUnicode_AsEncodedUnicode()                          = error("Not implemented.")
PyUnicode_AsLatin1String()                            = error("Not implemented.")
PyUnicode_AsRawUnicodeEscapeString()                  = error("Not implemented.")
PyUnicode_AsUCS4()                                    = error("Not implemented.")
PyUnicode_AsUCS4Copy()                                = error("Not implemented.")
PyUnicode_AsUTF16String()                             = error("Not implemented.")
PyUnicode_AsUTF32String()                             = error("Not implemented.")
PyUnicode_AsUTF8()                                    = error("Not implemented.")
PyUnicode_AsUTF8AndSize()                             = error("Not implemented.")
PyUnicode_AsUTF8String()                              = error("Not implemented.")
PyUnicode_AsUnicode()                                 = error("Not implemented.")
PyUnicode_AsUnicodeAndSize()                          = error("Not implemented.")
PyUnicode_AsUnicodeCopy()                             = error("Not implemented.")
PyUnicode_AsUnicodeEscapeString()                     = error("Not implemented.")
PyUnicode_AsWideChar()                                = error("Not implemented.")
PyUnicode_AsWideCharString()                          = error("Not implemented.")
PyUnicode_BuildEncodingMap()                          = error("Not implemented.")
PyUnicode_ClearFreeList()                             = error("Not implemented.")
PyUnicode_Compare()                                   = error("Not implemented.")
PyUnicode_CompareWithASCIIString()                    = error("Not implemented.")
PyUnicode_Concat()                                    = error("Not implemented.")
PyUnicode_Contains()                                  = error("Not implemented.")
PyUnicode_CopyCharacters()                            = error("Not implemented.")
PyUnicode_Count()                                     = error("Not implemented.")
PyUnicode_Decode()                                    = error("Not implemented.")
PyUnicode_DecodeASCII()                               = error("Not implemented.")
PyUnicode_DecodeCharmap()                             = error("Not implemented.")
PyUnicode_DecodeFSDefault()                           = error("Not implemented.")
PyUnicode_DecodeFSDefaultAndSize()                    = error("Not implemented.")
PyUnicode_DecodeLatin1()                              = error("Not implemented.")
PyUnicode_DecodeLocale()                              = error("Not implemented.")
PyUnicode_DecodeLocaleAndSize()                       = error("Not implemented.")
PyUnicode_DecodeRawUnicodeEscape()                    = error("Not implemented.")
PyUnicode_DecodeUTF16()                               = error("Not implemented.")
PyUnicode_DecodeUTF16Stateful()                       = error("Not implemented.")
PyUnicode_DecodeUTF32()                               = error("Not implemented.")
PyUnicode_DecodeUTF32Stateful()                       = error("Not implemented.")
PyUnicode_DecodeUTF7()                                = error("Not implemented.")
PyUnicode_DecodeUTF7Stateful()                        = error("Not implemented.")
PyUnicode_DecodeUTF8()                                = error("Not implemented.")
PyUnicode_DecodeUTF8Stateful()                        = error("Not implemented.")
PyUnicode_DecodeUnicodeEscape()                       = error("Not implemented.")
PyUnicode_Encode()                                    = error("Not implemented.")
PyUnicode_EncodeASCII()                               = error("Not implemented.")
PyUnicode_EncodeCharmap()                             = error("Not implemented.")
PyUnicode_EncodeDecimal()                             = error("Not implemented.")
PyUnicode_EncodeFSDefault()                           = error("Not implemented.")
PyUnicode_EncodeLatin1()                              = error("Not implemented.")
PyUnicode_EncodeLocale()                              = error("Not implemented.")
PyUnicode_EncodeRawUnicodeEscape()                    = error("Not implemented.")
PyUnicode_EncodeUTF16()                               = error("Not implemented.")
PyUnicode_EncodeUTF32()                               = error("Not implemented.")
PyUnicode_EncodeUTF7()                                = error("Not implemented.")
PyUnicode_EncodeUTF8()                                = error("Not implemented.")
PyUnicode_EncodeUnicodeEscape()                       = error("Not implemented.")
PyUnicode_FSConverter()                               = error("Not implemented.")
PyUnicode_FSDecoder()                                 = error("Not implemented.")
PyUnicode_Fill()                                      = error("Not implemented.")
PyUnicode_Find()                                      = error("Not implemented.")
PyUnicode_FindChar()                                  = error("Not implemented.")
PyUnicode_Format()                                    = error("Not implemented.")
PyUnicode_FromEncodedObject()                         = error("Not implemented.")
PyUnicode_FromFormat()                                = error("Not implemented.")
PyUnicode_FromFormatV()                               = error("Not implemented.")
PyUnicode_FromKindAndData()                           = error("Not implemented.")
PyUnicode_FromObject()                                = error("Not implemented.")
PyUnicode_FromOrdinal()                               = error("Not implemented.")
PyUnicode_FromString()                                = error("Not implemented.")
PyUnicode_FromStringAndSize()                         = error("Not implemented.")
PyUnicode_FromUnicode()                               = error("Not implemented.")
PyUnicode_FromWideChar()                              = error("Not implemented.")
PyUnicode_GetDefaultEncoding()                        = error("Not implemented.")
PyUnicode_GetLength()                                 = error("Not implemented.")
PyUnicode_GetMax()                                    = error("Not implemented.")
PyUnicode_GetSize()                                   = error("Not implemented.")
PyUnicode_InternFromString()                          = error("Not implemented.")
PyUnicode_InternImmortal()                            = error("Not implemented.")
PyUnicode_InternInPlace()                             = error("Not implemented.")
PyUnicode_IsIdentifier()                              = error("Not implemented.")
PyUnicode_Join()                                      = error("Not implemented.")
PyUnicode_New()                                       = error("Not implemented.")
PyUnicode_Partition()                                 = error("Not implemented.")
PyUnicode_RPartition()                                = error("Not implemented.")
PyUnicode_RSplit()                                    = error("Not implemented.")
PyUnicode_ReadChar()                                  = error("Not implemented.")
PyUnicode_Replace()                                   = error("Not implemented.")
PyUnicode_Resize()                                    = error("Not implemented.")
PyUnicode_RichCompare()                               = error("Not implemented.")
PyUnicode_Split()                                     = error("Not implemented.")
PyUnicode_Splitlines()                                = error("Not implemented.")
PyUnicode_Substring()                                 = error("Not implemented.")
PyUnicode_Tailmatch()                                 = error("Not implemented.")
PyUnicode_TransformDecimalToASCII()                   = error("Not implemented.")
PyUnicode_Translate()                                 = error("Not implemented.")
PyUnicode_TranslateCharmap()                          = error("Not implemented.")
PyUnicode_WriteChar()                                 = error("Not implemented.")
PyWeakref_GetObject()                                 = error("Not implemented.")
PyWeakref_NewProxy()                                  = error("Not implemented.")
PyWeakref_NewRef()                                    = error("Not implemented.")
PyWrapper_New()                                       = error("Not implemented.")
Py_AddPendingCall()                                   = error("Not implemented.")
Py_AtExit()                                           = error("Not implemented.")
Py_BuildValue()                                       = error("Not implemented.")
Py_CompileString()                                    = error("Not implemented.")
Py_CompileStringExFlags()                             = error("Not implemented.")
Py_CompileStringFlags()                               = error("Not implemented.")
Py_CompileStringObject()                              = error("Not implemented.")
Py_DecRef()                                           = error("Not implemented.")
Py_DecodeLocale()                                     = error("Not implemented.")
Py_EncodeLocale()                                     = error("Not implemented.")
Py_EndInterpreter()                                   = error("Not implemented.")
Py_Exit()                                             = error("Not implemented.")
Py_FatalError()                                       = error("Not implemented.")
Py_FdIsInteractive()                                  = error("Not implemented.")
Py_Finalize()                                         = error("Not implemented.")
Py_FinalizeEx()                                       = error("Not implemented.")
Py_FrozenMain()                                       = error("Not implemented.")
Py_GetArgcArgv()                                      = error("Not implemented.")
Py_GetBuildInfo()                                     = error("Not implemented.")
Py_GetCompiler()                                      = error("Not implemented.")
Py_GetCopyright()                                     = error("Not implemented.")
Py_GetExecPrefix()                                    = error("Not implemented.")
Py_GetPath()                                          = error("Not implemented.")
Py_GetPlatform()                                      = error("Not implemented.")
Py_GetPrefix()                                        = error("Not implemented.")
Py_GetProgramFullPath()                               = error("Not implemented.")
Py_GetProgramName()                                   = error("Not implemented.")
Py_GetPythonHome()                                    = error("Not implemented.")
Py_GetRecursionLimit()                                = error("Not implemented.")
Py_GetVersion()                                       = error("Not implemented.")
Py_IncRef()                                           = error("Not implemented.")
Py_Initialize()                                       = error("Not implemented.")
Py_InitializeEx()                                     = error("Not implemented.")
Py_IsInitialized()                                    = error("Not implemented.")
Py_Main()                                             = error("Not implemented.")
Py_MakePendingCalls()                                 = error("Not implemented.")
Py_NewInterpreter()                                   = error("Not implemented.")
Py_ReprEnter()                                        = error("Not implemented.")
Py_ReprLeave()                                        = error("Not implemented.")
Py_SetPath()                                          = error("Not implemented.")
Py_SetProgramName()                                   = error("Not implemented.")
Py_SetPythonHome()                                    = error("Not implemented.")
Py_SetRecursionLimit()                                = error("Not implemented.")
Py_SetStandardStreamEncoding()                        = error("Not implemented.")
Py_SymtableString()                                   = error("Not implemented.")
Py_SymtableStringObject()                             = error("Not implemented.")
Py_UNICODE_strcat()                                   = error("Not implemented.")
Py_UNICODE_strchr()                                   = error("Not implemented.")
Py_UNICODE_strcmp()                                   = error("Not implemented.")
Py_UNICODE_strcpy()                                   = error("Not implemented.")
Py_UNICODE_strlen()                                   = error("Not implemented.")
Py_UNICODE_strncmp()                                  = error("Not implemented.")
Py_UNICODE_strncpy()                                  = error("Not implemented.")
Py_UNICODE_strrchr()                                  = error("Not implemented.")
Py_UniversalNewlineFgets()                            = error("Not implemented.")
Py_VaBuildValue()                                     = error("Not implemented.")
Py_meta_grammar()                                     = error("Not implemented.")
Py_pgen()                                             = error("Not implemented.")

function PyAST_mod2obj()
    ccall((:PyAST_mod2obj, var"python3.7m"), Ptr{Cint}, ())
end

function PyAST_obj2mod(ast, arena, mode)
    ccall((:PyAST_obj2mod, var"python3.7m"), mod_ty, (Ptr{Cint}, Ptr{Cint}, Cint), ast, arena, mode)
end

function PyAST_Check(obj)
    ccall((:PyAST_Check, var"python3.7m"), Cint, (Ptr{Cint},), obj)
end

function newbitset(nbits)
    ccall((:newbitset, var"python3.7m"), bitset, (Cint,), nbits)
end

function delbitset(bs)
    ccall((:delbitset, var"python3.7m"), Cvoid, (bitset,), bs)
end

function addbit(bs, ibit)
    ccall((:addbit, var"python3.7m"), Cint, (bitset, Cint), bs, ibit)
end

function samebitset(bs1, bs2, nbits)
    ccall((:samebitset, var"python3.7m"), Cint, (bitset, bitset, Cint), bs1, bs2, nbits)
end

function mergebitset(bs1, bs2, nbits)
    ccall((:mergebitset, var"python3.7m"), Cvoid, (bitset, bitset, Cint), bs1, bs2, nbits)
end

function PyAsyncGen_ClearFreeLists()
    ccall((:PyAsyncGen_ClearFreeLists, var"python3.7m"), Cint, ())
end

function newgrammar(start)
    ccall((:newgrammar, var"python3.7m"), Ptr{grammar}, (Cint,), start)
end

function freegrammar(g)
    ccall((:freegrammar, var"python3.7m"), Cvoid, (Ptr{grammar},), g)
end

function adddfa(g, type, name)
    ccall((:adddfa, var"python3.7m"), Ptr{dfa}, (Ptr{grammar}, Cint, Cstring), g, type, name)
end

function addstate(d)
    ccall((:addstate, var"python3.7m"), Cint, (Ptr{dfa},), d)
end

function addarc(d, from, to, lbl)
    ccall((:addarc, var"python3.7m"), Cvoid, (Ptr{dfa}, Cint, Cint, Cint), d, from, to, lbl)
end

function PyGrammar_FindDFA(g, type)
    ccall((:PyGrammar_FindDFA, var"python3.7m"), Ptr{dfa}, (Ptr{grammar}, Cint), g, type)
end

function addlabel(ll, type, str)
    ccall((:addlabel, var"python3.7m"), Cint, (Ptr{labellist}, Cint, Cstring), ll, type, str)
end

function findlabel(ll, type, str)
    ccall((:findlabel, var"python3.7m"), Cint, (Ptr{labellist}, Cint, Cstring), ll, type, str)
end

function PyGrammar_LabelRepr(lb)
    ccall((:PyGrammar_LabelRepr, var"python3.7m"), Cstring, (Ptr{label},), lb)
end

function translatelabels(g)
    ccall((:translatelabels, var"python3.7m"), Cvoid, (Ptr{grammar},), g)
end

function addfirstsets(g)
    ccall((:addfirstsets, var"python3.7m"), Cvoid, (Ptr{grammar},), g)
end

function PyGrammar_AddAccelerators(g)
    ccall((:PyGrammar_AddAccelerators, var"python3.7m"), Cvoid, (Ptr{grammar},), g)
end

function PyGrammar_RemoveAccelerators(arg1)
    ccall((:PyGrammar_RemoveAccelerators, var"python3.7m"), Cvoid, (Ptr{grammar},), arg1)
end

function printgrammar(g, fp)
    ccall((:printgrammar, var"python3.7m"), Cvoid, (Ptr{grammar}, Ptr{Cint}), g, fp)
end

function printnonterminals(g, fp)
    ccall((:printnonterminals, var"python3.7m"), Cvoid, (Ptr{grammar}, Ptr{Cint}), g, fp)
end

function PyInit__imp()
    ccall((:PyInit__imp, var"python3.7m"), Cint, ())
end

function _PyArg_Fini()
    ccall((:_PyArg_Fini, var"python3.7m"), Cvoid, ())
end

function PyObject_Malloc(size)
    ccall((:PyObject_Malloc, var"python3.7m"), Ptr{Cvoid}, (Csize_t,), size)
end

function PyObject_Calloc(nelem, elsize)
    ccall((:PyObject_Calloc, var"python3.7m"), Ptr{Cvoid}, (Csize_t, Csize_t), nelem, elsize)
end

function PyObject_Realloc(ptr, new_size)
    ccall((:PyObject_Realloc, var"python3.7m"), Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), ptr, new_size)
end

function PyObject_Free(ptr)
    ccall((:PyObject_Free, var"python3.7m"), Cvoid, (Ptr{Cvoid},), ptr)
end

function PyObject_Init()
    ccall((:PyObject_Init, var"python3.7m"), Ptr{Cint}, ())
end

function PyObject_InitVar()
    ccall((:PyObject_InitVar, var"python3.7m"), Ptr{Cint}, ())
end

function PyObject_GetArenaAllocator(allocator)
    ccall((:PyObject_GetArenaAllocator, var"python3.7m"), Cvoid, (Ptr{PyObjectArenaAllocator},), allocator)
end

function PyObject_SetArenaAllocator(allocator)
    ccall((:PyObject_SetArenaAllocator, var"python3.7m"), Cvoid, (Ptr{PyObjectArenaAllocator},), allocator)
end

function PyGC_Collect()
    ccall((:PyGC_Collect, var"python3.7m"), Py_ssize_t, ())
end

function PyObject_GC_Track(arg1)
    ccall((:PyObject_GC_Track, var"python3.7m"), Cvoid, (Ptr{Cvoid},), arg1)
end

function PyObject_GC_UnTrack(arg1)
    ccall((:PyObject_GC_UnTrack, var"python3.7m"), Cvoid, (Ptr{Cvoid},), arg1)
end

function PyObject_GC_Del(arg1)
    ccall((:PyObject_GC_Del, var"python3.7m"), Cvoid, (Ptr{Cvoid},), arg1)
end

function meta_grammar()
    ccall((:meta_grammar, var"python3.7m"), Ptr{Cint}, ())
end

function pgen()
    ccall((:pgen, var"python3.7m"), Ptr{Cint}, ())
end

function PyDTrace_LINE(arg0, arg1, arg2)
    ccall((:PyDTrace_LINE, var"python3.7m"), Cvoid, (Cstring, Cstring, Cint), arg0, arg1, arg2)
end

function PyDTrace_FUNCTION_ENTRY(arg0, arg1, arg2)
    ccall((:PyDTrace_FUNCTION_ENTRY, var"python3.7m"), Cvoid, (Cstring, Cstring, Cint), arg0, arg1, arg2)
end

function PyDTrace_FUNCTION_RETURN(arg0, arg1, arg2)
    ccall((:PyDTrace_FUNCTION_RETURN, var"python3.7m"), Cvoid, (Cstring, Cstring, Cint), arg0, arg1, arg2)
end

function PyDTrace_GC_START(arg0)
    ccall((:PyDTrace_GC_START, var"python3.7m"), Cvoid, (Cint,), arg0)
end

function PyDTrace_GC_DONE(arg0)
    ccall((:PyDTrace_GC_DONE, var"python3.7m"), Cvoid, (Cint,), arg0)
end

function PyDTrace_INSTANCE_NEW_START(arg0)
    ccall((:PyDTrace_INSTANCE_NEW_START, var"python3.7m"), Cvoid, (Cint,), arg0)
end

function PyDTrace_INSTANCE_NEW_DONE(arg0)
    ccall((:PyDTrace_INSTANCE_NEW_DONE, var"python3.7m"), Cvoid, (Cint,), arg0)
end

function PyDTrace_INSTANCE_DELETE_START(arg0)
    ccall((:PyDTrace_INSTANCE_DELETE_START, var"python3.7m"), Cvoid, (Cint,), arg0)
end

function PyDTrace_INSTANCE_DELETE_DONE(arg0)
    ccall((:PyDTrace_INSTANCE_DELETE_DONE, var"python3.7m"), Cvoid, (Cint,), arg0)
end

function PyDTrace_IMPORT_FIND_LOAD_START(arg0)
    ccall((:PyDTrace_IMPORT_FIND_LOAD_START, var"python3.7m"), Cvoid, (Cstring,), arg0)
end

function PyDTrace_IMPORT_FIND_LOAD_DONE(arg0, arg1)
    ccall((:PyDTrace_IMPORT_FIND_LOAD_DONE, var"python3.7m"), Cvoid, (Cstring, Cint), arg0, arg1)
end

function PyDTrace_LINE_ENABLED()
    ccall((:PyDTrace_LINE_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_FUNCTION_ENTRY_ENABLED()
    ccall((:PyDTrace_FUNCTION_ENTRY_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_FUNCTION_RETURN_ENABLED()
    ccall((:PyDTrace_FUNCTION_RETURN_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_GC_START_ENABLED()
    ccall((:PyDTrace_GC_START_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_GC_DONE_ENABLED()
    ccall((:PyDTrace_GC_DONE_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_INSTANCE_NEW_START_ENABLED()
    ccall((:PyDTrace_INSTANCE_NEW_START_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_INSTANCE_NEW_DONE_ENABLED()
    ccall((:PyDTrace_INSTANCE_NEW_DONE_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_INSTANCE_DELETE_START_ENABLED()
    ccall((:PyDTrace_INSTANCE_DELETE_START_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_INSTANCE_DELETE_DONE_ENABLED()
    ccall((:PyDTrace_INSTANCE_DELETE_DONE_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_IMPORT_FIND_LOAD_START_ENABLED()
    ccall((:PyDTrace_IMPORT_FIND_LOAD_START_ENABLED, var"python3.7m"), Cint, ())
end

function PyDTrace_IMPORT_FIND_LOAD_DONE_ENABLED()
    ccall((:PyDTrace_IMPORT_FIND_LOAD_DONE_ENABLED, var"python3.7m"), Cint, ())
end

function PySignal_SetWakeupFd(fd)
    ccall((:PySignal_SetWakeupFd, var"python3.7m"), Cint, (Cint,), fd)
end


end # module
