# utils

macro embed(typename)
    t = getfield(@__MODULE__, typename)
    e = Expr(:block, [])
    for (x, y) in zip(fieldnames(t), fieldtypes(t))
        push!(e.args, :($x::$y))
    end
    return esc(e)
end

module CcallMacro

function parsecall(expr::Expr)
    # setup and check for errors
    if !Meta.isexpr(expr, :(::))
        throw(ArgumentError("@ccall needs a function signature with a return type"))
    end
    rettype = expr.args[2]

    call = expr.args[1]
    if !Meta.isexpr(call, :call)
        throw(ArgumentError("@ccall has to take a function call"))
    end

    # get the function symbols
    func = let f = call.args[1]
        f isa Expr ? :(($(f.args[2]), $(f.args[1]))) : QuoteNode(f)
    end

    # detect varargs
    varargs = nothing
    argstart = 2
    callargs = call.args
    if length(callargs) >= 2 && Meta.isexpr(callargs[2], :parameters)
        argstart = 3
        varargs = callargs[2].args
    end

    # collect args and types
    args = []
    types = []

    function pusharg!(arg)
        if !Meta.isexpr(arg, :(::))
            throw(ArgumentError("args in @ccall need type annotations. '$(repr(arg))' doesn't have one."))
        end
        push!(args, arg.args[1])
        push!(types, arg.args[2])
    end

    for i in argstart:length(callargs)
        pusharg!(callargs[i])
    end
    # add any varargs if necessary
    nreq = 0
    if !isnothing(varargs)
        nreq = length(args)
        for a in varargs
            pusharg!(a)
        end
    end

    return func, rettype, types, args, nreq
end

function lower(convention, func, rettype, types, args, nreq)
    lowering = []
    realargs = []
    gcroots = []
    for (i, (arg, type)) in enumerate(zip(args, types))
        sym = Symbol(string("arg", i, "root"))
        sym2 = Symbol(string("arg", i, ))
        earg, etype = esc(arg), esc(type)
        push!(lowering, :($sym = Base.cconvert($etype, $earg)))
        push!(lowering, :($sym2 = Base.unsafe_convert($etype, $sym)))
        push!(realargs, sym2)
        push!(gcroots, sym)
    end
    etypes = Expr(:call, Expr(:core, :svec), types...)
    exp = Expr(:foreigncall,
               esc(func),
               esc(rettype),
               esc(etypes),
               nreq,
               QuoteNode(convention),
               realargs..., gcroots...)
    push!(lowering, exp)

    return Expr(:block, lowering...)
end


"""
    @ccall some_c_function(arg::Type [...])::ReturnType
    @ccall calling_convetion some_c_function(arg::Type)::ReturnType
convert a julia-style function definition to a ccall:
    @ccall link(source::Cstring, dest::Cstring)::Cint
same as:
    ccall(:link, Cint, (Cstring, Cstring), source, dest)
All arguments must have type annotations and the return type must also
be annotated.
varargs are supported with the following convention:
    @ccall printf("%s = %d"::Cstring ; "foo"::Cstring, foo::Cint)::Cint
Mind the semicolon.
Using functions from other libraries is supported by prefixing
the function name with the name of the C library, like this:
    const glib = "libglib-2.0"
    @ccall glib.g_uri_escape_string(uri::Cstring, ":/"::Cstring, true::Cint)::Cstring
The string literal could also be used directly before the symbol of
the function name, if desired `"libglib-2.0".g_uri_escape_string(...`
"""
macro ccall(convention, expr)
    return lower(convention, parsecall(expr)...)
end

macro ccall(expr)
    return lower(:ccall, parsecall(expr)...)
end

end # CcallMacro

using .CcallMacro: @ccall


