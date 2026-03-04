# Generated from Dhall - DO NOT EDIT
# Haskell toolchain and rules using GHC from Nix.
#
# Uses ghcWithPackages from the Nix devshell, which includes all
# dependencies. The bin/ghc wrapper filters Mercury-specific flags
# that stock GHC doesn't understand.
#
# Paths are read from .buckconfig.local [haskell] section.
#
# Rules:
#   haskell_toolchain  - toolchain definition
#   haskell_library    - compile to .hi/.o with HaskellLibraryInfo
#   haskell_binary     - executable from sources + deps
#   haskell_c_library  - FFI exports callable from C/C++
#   haskell_ffi_binary - Haskell calling C/C++ via FFI
#   haskell_ffi_test   - FFI test executable
#   haskell_script     - single-file scripts
#   haskell_test       - test executable


load("@prelude//haskell:toolchain.bzl", "HaskellToolchainInfo", "HaskellPlatformInfo")

HaskellLibraryInfo = provider(fields = {
    "package_name": provider_field(str),
    "hi_dir": provider_field(Artifact | None, default = None),
    "object_dir": provider_field(Artifact | None, default = None),
    "stub_dir": provider_field(Artifact | None, default = None),
    "hie_dir": provider_field(Artifact | None, default = None),
    "objects": provider_field(list, default = []),
    "modules": provider_field(list, default = []),
    "src_dirs": provider_field(list, default = []),  # Source directories for -i flags
})

HaskellIncludeInfo = provider(fields = {
    "include_dirs": provider_field(list, default = []),
})


# Mandatory compiler flags - applied to all Haskell compilation
# These are non-negotiable and cannot be overridden by targets
MANDATORY_GHC_FLAGS = [
    "-Wall",
    "-Werror",
]

def _get_ghc() -> str:
    return read_root_config("haskell", "ghc", "bin/ghc")

def _get_ghc_pkg() -> str:
    return read_root_config("haskell", "ghc_pkg", "bin/ghc-pkg")

def _get_package_db() -> str | None:
    return read_root_config("haskell", "global_package_db", None)




def _haskell_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    """Haskell toolchain with paths from .buckconfig.local"""
    ghc = read_root_config("haskell", "ghc", "bin/ghc")
    ghc_pkg = read_root_config("haskell", "ghc_pkg", "bin/ghc-pkg")
    haddock = read_root_config("haskell", "haddock", "bin/haddock")

    return [
        DefaultInfo(),
        HaskellToolchainInfo(
            compiler = ghc,
            packager = ghc_pkg,
            linker = ghc,
            haddock = haddock,
            compiler_flags = ctx.attrs.compiler_flags,
            linker_flags = ctx.attrs.linker_flags,
            ghci_script_template = ctx.attrs.ghci_script_template,
            ghci_iserv_template = ctx.attrs.ghci_iserv_template,
            script_template_processor = ctx.attrs.script_template_processor,
            cache_links = True,
            archive_contents = "normal",
            support_expose_package = False,
        ),
        HaskellPlatformInfo(
            name = "x86_64-linux",
        ),
    ]


haskell_toolchain = rule(
    impl = _haskell_toolchain_impl,
    attrs = {
        "compiler_flags": attrs.list(attrs.string(), default = []),
        "linker_flags": attrs.list(attrs.string(), default = []),
        "ghci_script_template": attrs.option(attrs.source(), default = None),
        "ghci_iserv_template": attrs.option(attrs.source(), default = None),
        "script_template_processor": attrs.option(attrs.exec_dep(providers = [RunInfo], ), default = None),
    },
    is_toolchain_rule = True,
)

def _haskell_library_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    package_db = _get_package_db()
    
    if not ctx.attrs.srcs:
        return [
            DefaultInfo(),
            HaskellLibraryInfo(package_name = ctx.attrs.name, modules = []),
        ]
    
    # Output directories
    obj_dir = ctx.actions.declare_output("objs", dir = True)
    hi_dir = ctx.actions.declare_output("hi", dir = True)
    stub_dir = ctx.actions.declare_output("stubs", dir = True)
    
    # Compute source directories from source files
    # The include path is the directory containing the module hierarchy root.
    # For sources specified relative to BUCK file, we need the package directory.
    # In Buck2, ctx.label.package gives us "hs" for //hs:io-uring
    src_dirs = {ctx.label.package: True} if ctx.label.package else {".": True}
    
    # Collect dependency hi directories and src directories for -i flag
    dep_hi_dirs = []
    dep_src_dirs = []
    dep_objects = []
    for dep in ctx.attrs.deps:
        if HaskellLibraryInfo in dep:
            lib_info = dep[HaskellLibraryInfo]
            if lib_info.hi_dir:
                dep_hi_dirs.append(lib_info.hi_dir)
            if lib_info.src_dirs:
                dep_src_dirs.extend(lib_info.src_dirs)
            if lib_info.objects:
                dep_objects.extend(lib_info.objects)
            elif lib_info.object_dir:
                dep_objects.append(lib_info.object_dir)
    
    # Build GHC command
    cmd = cmd_args([ghc])
    cmd.add("-no-link")
    
    # Note: We do NOT use -package-env=- because GHC 9.12 + Nix ghc-with-packages
    # exposes all packages by default, and -package-env=- breaks package resolution.
    # We also don't need -package flags since packages are already exposed.
    
    cmd.add("-odir", obj_dir.as_output())
    cmd.add("-hidir", hi_dir.as_output())
    cmd.add("-stubdir", stub_dir.as_output())
    
    # Generate .hie files for IDE support (go-to-definition, etc.)
    hie_dir = ctx.actions.declare_output("hie", dir = True)
    cmd.add("-fwrite-ide-info")
    cmd.add("-hiedir", hie_dir.as_output())
    
    # Mandatory flags (non-negotiable)
    cmd.add(MANDATORY_GHC_FLAGS)
    
    # Language extensions
    cmd.add("-XGHC2024")
    for ext in ctx.attrs.language_extensions:
        cmd.add("-X{}".format(ext))
    
    # GHC options
    cmd.add(ctx.attrs.ghc_options)
    
    # Include paths for dependencies (hi dirs for precompiled, src dirs for sources)
    for hi_d in dep_hi_dirs:
        cmd.add(cmd_args("-i", hi_d, delimiter = ""))
    for src_d in dep_src_dirs:
        cmd.add(cmd_args("-i", src_d, delimiter = ""))
    
    # Sources
    cmd.add(ctx.attrs.srcs)
    
    ctx.actions.run(cmd, category = "haskell_compile", identifier = ctx.attrs.name)
    
    # Create static library from objects (GHC puts them in module subdirs)
    lib = ctx.actions.declare_output("lib{}.a".format(ctx.attrs.name))
    ar_cmd = cmd_args(
        "/bin/sh", "-c",
        cmd_args("find", obj_dir, "-name '*.o' -exec ar rcs", lib.as_output(), "{} +", delimiter = " "),
    )
    ctx.actions.run(ar_cmd, category = "haskell_archive", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(
            default_output = lib,
            sub_targets = {
                "hi": [DefaultInfo(default_outputs = [hi_dir])],
                "stubs": [DefaultInfo(default_outputs = [stub_dir])],
                "objects": [DefaultInfo(default_outputs = [obj_dir])],
                "hie": [DefaultInfo(default_outputs = [hie_dir])],
            },
        ),
        HaskellLibraryInfo(
            package_name = ctx.attrs.name,
            hi_dir = hi_dir,
            object_dir = lib,
            stub_dir = stub_dir,
            hie_dir = hie_dir,
            objects = [],
            modules = ctx.attrs.srcs,
            src_dirs = list(src_dirs.keys()),
        ),
    ]


haskell_library = rule(
    impl = _haskell_library_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "packages": attrs.list(attrs.string(), default = []),
        "ghc_options": attrs.list(attrs.string(), default = []),
        "language_extensions": attrs.list(attrs.string(), default = []),
    },
)

def _haskell_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    
    out = ctx.actions.declare_output(ctx.attrs.name)
    
    # Output directories for intermediate files (keeps source tree clean)
    obj_dir = ctx.actions.declare_output("objs", dir = True)
    hi_dir = ctx.actions.declare_output("hi", dir = True)
    
    # Collect dependency info
    dep_hi_dirs = []
    dep_src_dirs = []
    dep_libs = []
    dep_sources = []  # For source-based deps
    for dep in ctx.attrs.deps:
        if HaskellLibraryInfo in dep:
            lib_info = dep[HaskellLibraryInfo]
            if lib_info.hi_dir:
                dep_hi_dirs.append(lib_info.hi_dir)
            if lib_info.src_dirs:
                dep_src_dirs.extend(lib_info.src_dirs)
            if lib_info.objects:
                dep_libs.extend(lib_info.objects)
            elif lib_info.object_dir:
                dep_libs.append(lib_info.object_dir)
            # Also collect source modules for source-based compilation
            if lib_info.modules:
                dep_sources.extend(lib_info.modules)
    
    cmd = cmd_args([ghc])
    # Note: Don't use -package-env=-, -package-db, or -package flags
    # GHC 9.12 + Nix ghc-with-packages exposes all packages by default
    cmd.add("-O2")
    
    # Output directories (intermediate .o/.hi files go to buck-out, not source tree)
    cmd.add("-odir", obj_dir.as_output())
    cmd.add("-hidir", hi_dir.as_output())
    
    # Generate .hie files for IDE support (go-to-definition, etc.)
    hie_dir = ctx.actions.declare_output("hie", dir = True)
    cmd.add("-fwrite-ide-info")
    cmd.add("-hiedir", hie_dir.as_output())


    # Mandatory flags (non-negotiable)
    cmd.add(MANDATORY_GHC_FLAGS)
    cmd.add("-XGHC2024")
    
    # Main module
    if ctx.attrs.main:
        cmd.add("-main-is", ctx.attrs.main)
    
    cmd.add("-o", out.as_output())
    
    # Language extensions
    for ext in ctx.attrs.language_extensions:
        cmd.add("-X{}".format(ext))
    
    # GHC options (includes compiler_flags for backwards compat)
    cmd.add(ctx.attrs.ghc_options)
    cmd.add(ctx.attrs.compiler_flags)
    
    # Include paths for dependencies (hi dirs for precompiled, src dirs for sources)
    for hi_d in dep_hi_dirs:
        cmd.add(cmd_args("-i", hi_d, delimiter = ""))
    for src_d in dep_src_dirs:
        cmd.add(cmd_args("-i", src_d, delimiter = ""))
    
    # Sources (our sources + source-based deps)
    cmd.add(ctx.attrs.srcs)
    cmd.add(dep_sources)
    
    # Link against compiled deps
    cmd.add(dep_libs)
    
    ctx.actions.run(cmd, category = "ghc", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(
            default_output = out,
            sub_targets = {
                "hi": [DefaultInfo(default_outputs = [hi_dir])],
                "hie": [DefaultInfo(default_outputs = [hie_dir])],
            },
        ),
        RunInfo(args = cmd_args(out)),
    ]


haskell_binary = rule(
    impl = _haskell_binary_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "main": attrs.option(attrs.string(), default = None),
        "packages": attrs.list(attrs.string(), default = []),
        "ghc_options": attrs.list(attrs.string(), default = []),
        "language_extensions": attrs.list(attrs.string(), default = []),
        "compiler_flags": attrs.list(attrs.string(), default = []),
    },
)

def _haskell_c_library_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    package_db = _get_package_db()
    
    stub_dir = ctx.actions.declare_output("stubs", dir = True)
    lib = ctx.actions.declare_output("lib{}.a".format(ctx.attrs.name))
    
    # Collect dependency hi directories and src directories
    dep_hi_dirs = []
    dep_src_dirs = []
    for dep in ctx.attrs.deps:
        if HaskellLibraryInfo in dep:
            lib_info = dep[HaskellLibraryInfo]
            if lib_info.hi_dir:
                dep_hi_dirs.append(lib_info.hi_dir)
            if lib_info.src_dirs:
                dep_src_dirs.extend(lib_info.src_dirs)
    
    # Compile each source individually to get proper stub generation
    objects = []
    hi_files = []
    
    for src in ctx.attrs.srcs:
        src_path = src.short_path
        if src_path.endswith(".hs"):
            base_name = src_path.replace(".hs", "").split("/")[-1]
            obj = ctx.actions.declare_output("{}.o".format(base_name))
            hi = ctx.actions.declare_output("{}.hi".format(base_name))
            
            cmd = cmd_args([ghc])
            cmd.add("-c")
            # Note: Don't use -package-env=- - breaks package resolution in GHC 9.12 + Nix
            cmd.add("-fPIC")  # Position independent for shared libs
            
            cmd.add("-stubdir", stub_dir.as_output())
            cmd.add("-o", obj.as_output())
            cmd.add("-ohi", hi.as_output())
            
            # Mandatory flags (non-negotiable)
            cmd.add(MANDATORY_GHC_FLAGS)
            
            # Language extensions (ForeignFunctionInterface is required)
            cmd.add("-XGHC2024")
            cmd.add("-XForeignFunctionInterface")
            for ext in ctx.attrs.language_extensions:
                cmd.add("-X{}".format(ext))
            
            cmd.add(ctx.attrs.ghc_options)
            
            # Dependencies (hi dirs for precompiled, src dirs for sources)
            for hi_d in dep_hi_dirs:
                cmd.add(cmd_args("-i", hi_d, delimiter = ""))
            for src_d in dep_src_dirs:
                cmd.add(cmd_args("-i", src_d, delimiter = ""))
            
            for pkg in ctx.attrs.packages:
                cmd.add("-package", pkg)
            
            cmd.add(src)
            
            ctx.actions.run(cmd, category = "haskell_compile", identifier = src_path)
            objects.append(obj)
            hi_files.append(hi)
    
    if not objects:
        return [DefaultInfo()]
    
    # Create hi directory with symlinks
    hi_dir = ctx.actions.declare_output("hi", dir = True)
    hi_symlinks = {hi.basename: hi for hi in hi_files}
    ctx.actions.symlinked_dir(hi_dir, hi_symlinks)
    
    # Archive objects
    ar_cmd = cmd_args("ar", "rcs", lib.as_output())
    ar_cmd.add(objects)
    ctx.actions.run(ar_cmd, category = "haskell_archive", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(
            default_output = lib,
            sub_targets = {
                "stubs": [DefaultInfo(default_outputs = [stub_dir])],
                "hi": [DefaultInfo(default_outputs = hi_files)],
                "objects": [DefaultInfo(default_outputs = objects)],
            },
        ),
        HaskellIncludeInfo(include_dirs = [stub_dir]),
        HaskellLibraryInfo(
            package_name = ctx.attrs.name,
            hi_dir = hi_dir,
            object_dir = lib,
            stub_dir = stub_dir,
            objects = objects,
            modules = [],
        ),
    ]


haskell_c_library = rule(
    impl = _haskell_c_library_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "packages": attrs.list(attrs.string(), default = []),
        "ghc_options": attrs.list(attrs.string(), default = []),
        "language_extensions": attrs.list(attrs.string(), default = []),
    },
)

def _haskell_ffi_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    ghc_pkg = _get_ghc_pkg()
    cxx = read_root_config("cxx", "cxx", "clang++")
    
    # Read library paths from config (for Nix-provided libraries)
    liburing_lib = read_root_config("io-uring", "liburing_lib", "")
    liburing_include = read_root_config("io-uring", "liburing_include", "")
    
    # Extra library dirs from [cxx] config (includes notcurses, etc.)
    cxx_extra_lib_dirs = read_root_config("cxx", "extra_lib_dirs", "")
    
    # C++ stdlib paths for unwrapped clang
    gcc_include = read_root_config("cxx", "gcc_include", "")
    gcc_include_arch = read_root_config("cxx", "gcc_include_arch", "")
    glibc_include = read_root_config("cxx", "glibc_include", "")
    clang_resource_dir = read_root_config("cxx", "clang_resource_dir", "")
    gcc_lib_base = read_root_config("cxx", "gcc_lib_base", "")
    
    out = ctx.actions.declare_output(ctx.attrs.name)
    
    # Step 1: Compile C++ sources
    cxx_compile_flags = ["-std=c++17", "-O2", "-fPIC", "-c"]
    
    if gcc_include:
        cxx_compile_flags.extend(["-isystem", gcc_include])
    if gcc_include_arch:
        cxx_compile_flags.extend(["-isystem", gcc_include_arch])
    if glibc_include:
        cxx_compile_flags.extend(["-isystem", glibc_include])
    if clang_resource_dir:
        cxx_compile_flags.extend(["-resource-dir=" + clang_resource_dir])
    
    cxx_compile_flags.extend(["-I", "."])
    
    # Add user-specified include directories
    for inc_dir in ctx.attrs.include_dirs:
        cxx_compile_flags.extend(["-I", inc_dir])
    
    # Add config-provided include directories (from Nix)
    if liburing_include:
        cxx_compile_flags.extend(["-I", liburing_include])
    
    cxx_objects = []
    for src in ctx.attrs.cxx_srcs:
        obj_name = src.short_path.replace(".cpp", ".o").replace(".c", ".o")
        obj = ctx.actions.declare_output(obj_name)
        
        cmd = cmd_args([cxx] + cxx_compile_flags + ["-o", obj.as_output(), src])
        ctx.actions.run(cmd, category = "cxx_compile", identifier = src.short_path)
        cxx_objects.append(obj)
    
    # Step 2: Compile Haskell and link
    # Output directories for intermediate files (keeps source tree clean)
    obj_dir = ctx.actions.declare_output("hs_objs", dir = True)
    hi_dir = ctx.actions.declare_output("hs_hi", dir = True)
    
    # Use ghc-pkg-id wrapper script to translate -package to -package-id
    # This works around GHC 9.12 bug where -package doesn't expose packages
    # Path comes from config, set by flake module's shellHook
    ghc_wrapper = read_root_config("haskell", "ghc_pkg_wrapper", "bin/ghc-pkg-id")
    ghc_cmd = cmd_args([ghc_wrapper, ghc, ghc_pkg])
    ghc_cmd.add("-O2", "-threaded")
    
    # Output directories (intermediate .o/.hi files go to buck-out, not source tree)
    ghc_cmd.add("-odir", obj_dir.as_output())
    ghc_cmd.add("-hidir", hi_dir.as_output())
    
    # Mandatory flags (non-negotiable)
    ghc_cmd.add(MANDATORY_GHC_FLAGS)
    ghc_cmd.add("-XGHC2024")
    
    # GCC library path for libstdc++
    if gcc_lib_base:
        ghc_cmd.add("-optl", "-L" + gcc_lib_base)
    
    # Extra library directories from attrs
    for lib_dir in ctx.attrs.extra_lib_dirs:
        ghc_cmd.add("-optl", "-L" + lib_dir)
        ghc_cmd.add("-optl", "-Wl,-rpath," + lib_dir)
    
    # Config-provided library directories (from Nix)
    if liburing_lib:
        ghc_cmd.add("-optl", "-L" + liburing_lib)
        ghc_cmd.add("-optl", "-Wl,-rpath," + liburing_lib)
    
    # Extra library directories from [cxx] config (notcurses, openssl, etc.)
    if cxx_extra_lib_dirs:
        for lib_dir in cxx_extra_lib_dirs.split(":"):
            if lib_dir:
                ghc_cmd.add("-optl", "-L" + lib_dir)
                ghc_cmd.add("-optl", "-Wl,-rpath," + lib_dir)
    
    ghc_cmd.add("-o", out.as_output())
    
    # Language extensions
    for ext in ctx.attrs.language_extensions:
        ghc_cmd.add("-X{}".format(ext))
    
    # GHC options from attrs
    ghc_cmd.add(ctx.attrs.ghc_options)
    
    # Include directories for CApi FFI headers
    for inc_dir in ctx.attrs.include_dirs:
        ghc_cmd.add("-optc", "-I" + inc_dir)
    
    # Packages
    for pkg in ctx.attrs.packages:
        ghc_cmd.add("-package", pkg)
    
    ghc_cmd.add(ctx.attrs.compiler_flags)
    ghc_cmd.add(ctx.attrs.hs_srcs)
    ghc_cmd.add(cxx_objects)
    
    # Link libraries AFTER object files (linker order matters for symbol resolution)
    ghc_cmd.add("-lstdc++")
    
    # Link against extra libraries
    for lib in ctx.attrs.extra_libs:
        ghc_cmd.add("-l" + lib)
    
    # Extra linker flags
    for flag in ctx.attrs.linker_flags:
        ghc_cmd.add("-optl", flag)
    
    ctx.actions.run(ghc_cmd, category = "ghc_link", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(default_output = out),
        RunInfo(args = [out]),
    ]


haskell_ffi_binary = rule(
    impl = _haskell_ffi_binary_impl,
    attrs = {
        "hs_srcs": attrs.list(attrs.source(), default = []),
        "cxx_srcs": attrs.list(attrs.source(), default = []),
        "cxx_headers": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "packages": attrs.list(attrs.string(), default = []),
        "compiler_flags": attrs.list(attrs.string(), default = []),
        "language_extensions": attrs.list(attrs.string(), default = []),
        "ghc_options": attrs.list(attrs.string(), default = []),
        "extra_libs": attrs.list(attrs.string(), default = []),
        "extra_lib_dirs": attrs.list(attrs.string(), default = []),
        "include_dirs": attrs.list(attrs.string(), default = []),
        "linker_flags": attrs.list(attrs.string(), default = []),
    },
)

def _haskell_ffi_test_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    ghc_pkg = _get_ghc_pkg()
    cxx = read_root_config("cxx", "cxx", "clang++")
    
    # Read library paths from config (for Nix-provided libraries)
    liburing_lib = read_root_config("io-uring", "liburing_lib", "")
    liburing_include = read_root_config("io-uring", "liburing_include", "")
    
    # C++ stdlib paths for unwrapped clang
    gcc_include = read_root_config("cxx", "gcc_include", "")
    gcc_include_arch = read_root_config("cxx", "gcc_include_arch", "")
    glibc_include = read_root_config("cxx", "glibc_include", "")
    clang_resource_dir = read_root_config("cxx", "clang_resource_dir", "")
    gcc_lib_base = read_root_config("cxx", "gcc_lib_base", "")
    
    out = ctx.actions.declare_output(ctx.attrs.name)
    
    # Step 1: Compile C++ sources
    cxx_compile_flags = ["-std=c++17", "-O2", "-fPIC", "-c"]
    
    if gcc_include:
        cxx_compile_flags.extend(["-isystem", gcc_include])
    if gcc_include_arch:
        cxx_compile_flags.extend(["-isystem", gcc_include_arch])
    if glibc_include:
        cxx_compile_flags.extend(["-isystem", glibc_include])
    if clang_resource_dir:
        cxx_compile_flags.extend(["-resource-dir=" + clang_resource_dir])
    
    cxx_compile_flags.extend(["-I", "."])
    
    # Add user-specified include directories
    for inc_dir in ctx.attrs.include_dirs:
        cxx_compile_flags.extend(["-I", inc_dir])
    
    # Add config-provided include directories (from Nix)
    if liburing_include:
        cxx_compile_flags.extend(["-I", liburing_include])
    
    cxx_objects = []
    for src in ctx.attrs.cxx_srcs:
        obj_name = src.short_path.replace(".cpp", ".o").replace(".c", ".o")
        obj = ctx.actions.declare_output(obj_name)
        
        cmd = cmd_args([cxx] + cxx_compile_flags + ["-o", obj.as_output(), src])
        ctx.actions.run(cmd, category = "cxx_compile", identifier = src.short_path)
        cxx_objects.append(obj)
    
    # Step 2: Compile Haskell and link
    # Output directories for intermediate files (keeps source tree clean)
    obj_dir = ctx.actions.declare_output("hs_objs", dir = True)
    hi_dir = ctx.actions.declare_output("hs_hi", dir = True)
    
    # Use ghc-pkg-id wrapper script to translate -package to -package-id
    # This works around GHC 9.12 bug where -package doesn't expose packages
    # Path comes from config, set by flake module's shellHook
    ghc_wrapper = read_root_config("haskell", "ghc_pkg_wrapper", "bin/ghc-pkg-id")
    ghc_cmd = cmd_args([ghc_wrapper, ghc, ghc_pkg])
    ghc_cmd.add("-O2", "-threaded")
    
    # Output directories (intermediate .o/.hi files go to buck-out, not source tree)
    ghc_cmd.add("-odir", obj_dir.as_output())
    ghc_cmd.add("-hidir", hi_dir.as_output())
    
    # Mandatory flags (non-negotiable)
    ghc_cmd.add(MANDATORY_GHC_FLAGS)
    ghc_cmd.add("-XGHC2024")
    
    # GCC library path for libstdc++
    if gcc_lib_base:
        ghc_cmd.add("-optl", "-L" + gcc_lib_base)
    
    # Extra library directories from attrs
    for lib_dir in ctx.attrs.extra_lib_dirs:
        ghc_cmd.add("-optl", "-L" + lib_dir)
        ghc_cmd.add("-optl", "-Wl,-rpath," + lib_dir)
    
    # Config-provided library directories (from Nix)
    if liburing_lib:
        ghc_cmd.add("-optl", "-L" + liburing_lib)
        ghc_cmd.add("-optl", "-Wl,-rpath," + liburing_lib)
    
    ghc_cmd.add("-o", out.as_output())
    
    # Language extensions
    for ext in ctx.attrs.language_extensions:
        ghc_cmd.add("-X{}".format(ext))
    
    # GHC options from attrs
    ghc_cmd.add(ctx.attrs.ghc_options)
    
    # Include directories for CApi FFI headers
    for inc_dir in ctx.attrs.include_dirs:
        ghc_cmd.add("-optc", "-I" + inc_dir)
    
    # Packages
    for pkg in ctx.attrs.packages:
        ghc_cmd.add("-package", pkg)
    
    ghc_cmd.add(ctx.attrs.compiler_flags)
    ghc_cmd.add(ctx.attrs.hs_srcs)
    ghc_cmd.add(cxx_objects)
    
    # Link libraries AFTER object files (linker order matters for symbol resolution)
    ghc_cmd.add("-lstdc++")
    
    # Link against extra libraries
    for lib in ctx.attrs.extra_libs:
        ghc_cmd.add("-l" + lib)
    
    # Extra linker flags
    for flag in ctx.attrs.linker_flags:
        ghc_cmd.add("-optl", flag)
    
    ctx.actions.run(ghc_cmd, category = "ghc_link", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(default_output = out),
        RunInfo(args = [out]),
    ]


haskell_ffi_test = rule(
    impl = _haskell_ffi_test_impl,
    attrs = {
        "hs_srcs": attrs.list(attrs.source(), default = []),
        "cxx_srcs": attrs.list(attrs.source(), default = []),
        "cxx_headers": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "packages": attrs.list(attrs.string(), default = []),
        "compiler_flags": attrs.list(attrs.string(), default = []),
        "language_extensions": attrs.list(attrs.string(), default = []),
        "ghc_options": attrs.list(attrs.string(), default = []),
        "extra_libs": attrs.list(attrs.string(), default = []),
        "extra_lib_dirs": attrs.list(attrs.string(), default = []),
        "include_dirs": attrs.list(attrs.string(), default = []),
        "linker_flags": attrs.list(attrs.string(), default = []),
    },
)

def _haskell_script_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    
    out = ctx.actions.declare_output(ctx.attrs.name)
    
    # Output directories for intermediate files (keeps source tree clean)
    obj_dir = ctx.actions.declare_output("objs", dir = True)
    hi_dir = ctx.actions.declare_output("hi", dir = True)
    
    cmd = cmd_args([ghc])
    
    # Output directories (intermediate .o/.hi files go to buck-out, not source tree)
    cmd.add("-odir", obj_dir.as_output())
    cmd.add("-hidir", hi_dir.as_output())
    
    # Mandatory flags (non-negotiable)
    cmd.add(MANDATORY_GHC_FLAGS)
    cmd.add("-XGHC2024")
    
    cmd.add(ctx.attrs.compiler_flags)
    cmd.add("-o", out.as_output())
    
    for include_path in ctx.attrs.include_paths:
        cmd.add("-i" + include_path)
    
    for pkg in ctx.attrs.packages:
        cmd.add("-package", pkg)
    
    cmd.add(ctx.attrs.srcs)
    
    ctx.actions.run(cmd, category = "haskell_script", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(default_output = out),
        RunInfo(args = [out]),
    ]


haskell_script = rule(
    impl = _haskell_script_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "include_paths": attrs.list(attrs.string(), default = []),
        "compiler_flags": attrs.list(attrs.string(), default = []),
        "packages": attrs.list(attrs.string(), default = []),
    },
)

def _haskell_test_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    ghc = _get_ghc()
    package_db = _get_package_db()
    
    out = ctx.actions.declare_output(ctx.attrs.name)
    
    # Output directories for intermediate files (keeps source tree clean)
    obj_dir = ctx.actions.declare_output("objs", dir = True)
    hi_dir = ctx.actions.declare_output("hi", dir = True)
    
    # Collect dependency info
    dep_hi_dirs = []
    dep_src_dirs = []
    dep_libs = []
    dep_sources = []  # For source-based deps
    for dep in ctx.attrs.deps:
        if HaskellLibraryInfo in dep:
            lib_info = dep[HaskellLibraryInfo]
            if lib_info.hi_dir:
                dep_hi_dirs.append(lib_info.hi_dir)
            if lib_info.src_dirs:
                dep_src_dirs.extend(lib_info.src_dirs)
            if lib_info.objects:
                dep_libs.extend(lib_info.objects)
            elif lib_info.object_dir:
                dep_libs.append(lib_info.object_dir)
            # Also collect source modules for source-based compilation
            if lib_info.modules:
                dep_sources.extend(lib_info.modules)
    
    cmd = cmd_args([ghc])
    # Note: Don't use -package-env=- - breaks package resolution in GHC 9.12 + Nix
    cmd.add("-O2")
    
    # Output directories (intermediate .o/.hi files go to buck-out, not source tree)
    cmd.add("-odir", obj_dir.as_output())
    cmd.add("-hidir", hi_dir.as_output())
    
    # Generate .hie files for IDE support (go-to-definition, etc.)
    hie_dir = ctx.actions.declare_output("hie", dir = True)
    cmd.add("-fwrite-ide-info")
    cmd.add("-hiedir", hie_dir.as_output())


    # Mandatory flags (non-negotiable)
    cmd.add(MANDATORY_GHC_FLAGS)
    cmd.add("-XGHC2024")
    
    # Note: Don't use -package-db or -package flags
    # GHC 9.12 + Nix ghc-with-packages exposes all packages by default
    
    # Main module
    if ctx.attrs.main:
        cmd.add("-main-is", ctx.attrs.main)
    
    cmd.add("-o", out.as_output())
    
    # Language extensions
    for ext in ctx.attrs.language_extensions:
        cmd.add("-X{}".format(ext))
    
    # GHC options (includes compiler_flags for backwards compat)
    cmd.add(ctx.attrs.ghc_options)
    cmd.add(ctx.attrs.compiler_flags)
    
    # Note: No -package flags needed - GHC 9.12 + Nix ghc-with-packages exposes all
    
    # Include paths for dependencies (hi dirs for precompiled, src dirs for sources)
    for hi_d in dep_hi_dirs:
        cmd.add(cmd_args("-i", hi_d, delimiter = ""))
    for src_d in dep_src_dirs:
        cmd.add(cmd_args("-i", src_d, delimiter = ""))
    
    # Sources (our sources + source-based deps)
    cmd.add(ctx.attrs.srcs)
    cmd.add(dep_sources)
    
    # Link against compiled deps
    cmd.add(dep_libs)
    
    ctx.actions.run(cmd, category = "ghc", identifier = ctx.attrs.name)
    
    return [
        DefaultInfo(
            default_output = out,
            sub_targets = {
                "hi": [DefaultInfo(default_outputs = [hi_dir])],
                "hie": [DefaultInfo(default_outputs = [hie_dir])],
            },
        ),
        RunInfo(args = cmd_args(out)),
    ]


haskell_test = rule(
    impl = _haskell_test_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "main": attrs.option(attrs.string(), default = None),
        "packages": attrs.list(attrs.string(), default = []),
        "ghc_options": attrs.list(attrs.string(), default = []),
        "language_extensions": attrs.list(attrs.string(), default = []),
        "compiler_flags": attrs.list(attrs.string(), default = []),
    },
)

