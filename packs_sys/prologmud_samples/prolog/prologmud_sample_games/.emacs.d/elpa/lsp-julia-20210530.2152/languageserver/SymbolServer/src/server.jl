module SymbolServer
import Sockets

pipename = length(ARGS) > 1 ? ARGS[2] : nothing

conn = pipename !== nothing ? Sockets.connect(pipename) : nothing

start_time = time_ns()

# Try to lower the priority of this process so that it doesn't block the
# user system.
@static if Sys.iswindows()
    # Get process handle
    p_handle = ccall(:GetCurrentProcess, stdcall, Ptr{Cvoid}, ())

    # Set BELOW_NORMAL_PRIORITY_CLASS
    ret = ccall(:SetPriorityClass, stdcall, Cint, (Ptr{Cvoid}, Culong), p_handle, 0x00004000)
    ret != 1 && @warn "Something went wrong when setting BELOW_NORMAL_PRIORITY_CLASS."
else
    ret = ccall(:nice, Cint, (Cint,), 1)
    # We don't check the return value because it doesn't really matter
end

module LoadingBay
end

# Make sure we can load stdlibs
!in("@stdlib", LOAD_PATH) && push!(LOAD_PATH, "@stdlib")

using Serialization, Pkg, SHA
using Base: UUID

include("faketypes.jl")
include("symbols.jl")
include("utils.jl")

store_path = length(ARGS) > 0 ? ARGS[1] : abspath(joinpath(@__DIR__, "..", "store"))

ctx = try
    Pkg.Types.Context()
catch err
    @info "Package environment can't be read."
    exit()
end
# Add some methods to check whether a package is part of the standard library and so
# won't need recaching.
if isdefined(Pkg.Types, :is_stdlib)
    is_stdlib(uuid::UUID) = Pkg.Types.is_stdlib(uuid)
else
    is_stdlib(uuid::UUID) = uuid in keys(ctx.stdlibs)
end

server = Server(store_path, ctx, Dict{UUID,Package}())

function load_package(c::Pkg.Types.Context, uuid, conn)
    isinmanifest(c, uuid isa String ? Base.UUID(uuid) : uuid) || return
    pe_name = packagename(c, uuid)
    pid = Base.PkgId(uuid isa String ? Base.UUID(uuid) : uuid, pe_name)
    if pid in keys(Base.loaded_modules)
        conn !== nothing && println(conn, "PROCESSPKG;$pe_name;$uuid;noversion")
        LoadingBay.eval(:($(Symbol(pe_name)) = $(Base.loaded_modules[pid])))
        m = getfield(LoadingBay, Symbol(pe_name))
    else
        m = try
            conn !== nothing && println(conn, "STARTLOAD;$pe_name;$uuid;noversion")
            LoadingBay.eval(:(import $(Symbol(pe_name))))
            conn !== nothing && println(conn, "STOPLOAD;$pe_name")
            m = getfield(LoadingBay, Symbol(pe_name))
        catch e
            return
        end
    end
end

function write_cache(name, pkg)
    open(joinpath(server.storedir, name), "w") do io
        serialize(io, pkg)
    end
end

function write_depot(server, ctx, written_caches)
    for (uuid, pkg) in server.depot
        filename = get_filename_from_name(ctx.env.manifest, uuid)
        filename === nothing && continue
        cache_path = joinpath(server.storedir, filename)
        cache_path in written_caches && continue
        push!(written_caches, cache_path)
        @info "Now writing to disc $uuid"
        write_cache(cache_path, pkg)
    end
end
# List of caches that have already been written
written_caches = String[]

# First get a list of all package UUIds that we want to cache
toplevel_pkgs = deps(project(ctx))
packages_to_load = []
# Next make sure the cache is up-to-date for all of these
for (pk_name, uuid) in toplevel_pkgs
    file_name = get_filename_from_name(ctx.env.manifest, uuid)
    # We sometimes have UUIDs in the project file that are not in the
    # manifest file. That seems like something that shouldn't happen, but
    # in practice is not under our control. For now, we just skip these
    # packages
    file_name === nothing && continue
    cache_path = joinpath(server.storedir, file_name)

    if isfile(cache_path)
        if is_package_deved(ctx.env.manifest, uuid)
            cached_version = open(cache_path) do io
                deserialize(io)
            end
            if sha_pkg(frommanifest(ctx.env.manifest, uuid)) != cached_version.sha
                @info "Outdated sha, will recache package $pk_name ($uuid)"
                push!(packages_to_load, uuid)
            else
                @info "Package $pk_name ($uuid) is cached."
            end
        else
            @info "Package $pk_name ($uuid) is cached."
        end
    else
        @info "Will cache package $pk_name ($uuid)"
        push!(packages_to_load, uuid)
    end
end

# Load all packages together
for uuid in packages_to_load
    load_package(ctx, uuid, conn)
end

# Create image of whole package env. This creates the module structure only.
env_symbols = getenvtree()

# Populate the above with symbols, skipping modules that don't need caching.
# symbols (env_symbols)
visited = Base.IdSet{Module}([Base, Core]) # don't need to cache these each time...
for (pid, m) in Base.loaded_modules
    if pid.uuid !== nothing && is_stdlib(pid.uuid) &&
        (file_name = get_filename_from_name(ctx.env.manifest, pid.uuid)) !== nothing &&
        isfile(joinpath(server.storedir, file_name))
        push!(visited, m)
        delete!(env_symbols, Symbol(pid.name))
    end
end

symbols(env_symbols, nothing, SymbolServer.getallns(), visited)

# Wrap the `ModuleStore`s as `Package`s.
for (pkg_name, cache) in env_symbols
    pkg_name = String(pkg_name)
    !isinmanifest(ctx, pkg_name) && continue
    uuid = packageuuid(ctx, String(pkg_name))
    pe = frommanifest(ctx, uuid)
    server.depot[uuid] = Package(String(pkg_name), cache, version(pe), uuid, sha_pkg(pe))
end

# Write to disc
write_depot(server, server.context, written_caches)
end_time = time_ns()

elapsed_time_in_s = (end_time - start_time) / 1e9
@info "Symbol server indexing took $elapsed_time_in_s seconds."

end
