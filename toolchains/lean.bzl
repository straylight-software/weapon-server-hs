# Generated from Dhall - DO NOT EDIT
# Lean 4 compilation rules for Buck2 with Nix toolchain integration
#
# Lean 4 compiles to C, which we then compile with our C++ toolchain.
# This enables proof-carrying code: Lean theorems constrain generated C,
# which links into Rust/Haskell/Python via FFI.
#
# Key features:
#   - lean_library: Build a Lean library (.olean files + C extraction)
#   - lean_binary: Build a Lean executable
#   - lean_c_library: Extract C code from Lean for FFI linking




LeanLibraryInfo = provider(fields = {
    "olean_dir": provider_field(Artifact | None, default = None),
    "c_dir": provider_field(Artifact | None, default = None),
    "lib_name": provider_field(str, default = ""),
    "deps": provider_field(list, default = []),
})

LeanCLibraryInfo = provider(fields = {
    "c_sources": provider_field(list[Artifact], default = []),
    "include_dir": provider_field(Artifact | None, default = None),
    "objects": provider_field(list[Artifact], default = []),
    "archive": provider_field(Artifact | None, default = None),
})

LeanToolchainInfo = provider(fields = {
    "lean": provider_field(str),
    "leanc": provider_field(str),
    "lean_lib_dir": provider_field(str | None, default = None),
    "lean_include_dir": provider_field(str | None, default = None),
})


def _get_lean() -> str:
    """Get lean compiler path from config."""
    path = read_root_config("lean", "lean", None)
    if path == None:
        fail("""
lean compiler not configured.

Configure your toolchain via Nix:

    [lean]
    lean = /nix/store/.../bin/lean
    leanc = /nix/store/.../bin/leanc
    lean_lib_dir = /nix/store/.../lib/lean
    lean_include_dir = /nix/store/.../include

Then run: nix develop
""")
    return path

def _get_leanc() -> str:
    """Get leanc (Lean C compiler wrapper) path from config."""
    path = read_root_config("lean", "leanc", None)
    if path == None:
        fail("leanc not configured. See [lean] section in .buckconfig")
    return path

def _get_lean_lib_dir() -> str | None:
    """Get Lean standard library directory."""
    return read_root_config("lean", "lean_lib_dir", None)

def _get_lean_include_dir() -> str | None:
    """Get Lean C headers directory."""
    return read_root_config("lean", "lean_include_dir", None)




def _lean_library_impl(ctx: AnalysisContext) -> list[Provider]:
    """Build a Lean library preserving module hierarchy.
    
    Lean modules use hierarchical imports (e.g., `import Cornell.Basic`).
    We copy all sources to scratch preserving hierarchy, then compile
    the root module which pulls in dependencies via --make semantics.
    """
    lean = _get_lean()
    lean_lib_dir = _get_lean_lib_dir()
    
    if not ctx.attrs.srcs:
        return [DefaultInfo(), LeanLibraryInfo()]
    
    # Output directories
    olean_dir = ctx.actions.declare_output("olean", dir = True)
    c_dir = ctx.actions.declare_output("c", dir = True) if ctx.attrs.extract_c else None
    
    # Collect dependency olean directories
    dep_paths = []
    for dep in ctx.attrs.deps:
        if LeanLibraryInfo in dep:
            info = dep[LeanLibraryInfo]
            if info.olean_dir:
                dep_paths.append(info.olean_dir)
    
    # Find the root module (shortest path, typically "Cornell.lean" not "Cornell/Basic.lean")
    root_module = None
    for src in ctx.attrs.srcs:
        if "/" not in src.short_path:
            root_module = src
            break
    
    # Build script
    script_parts = ["set -e"]
    script_parts.append("mkdir -p $OLEAN_DIR")
    if c_dir:
        script_parts.append("mkdir -p $C_DIR")
    
    # Build LEAN_PATH from dependencies and stdlib
    lean_path_parts = ["$OLEAN_DIR", "$BUCK_SCRATCH_PATH"]
    if lean_lib_dir:
        lean_path_parts.append(lean_lib_dir)
    for dep_path in dep_paths:
        lean_path_parts.append(cmd_args(dep_path))
    
    script_parts.append(cmd_args(
        "export LEAN_PATH=",
        cmd_args(lean_path_parts, delimiter = ":"),
        delimiter = "",
    ))
    
    # Step 1: Copy all source files preserving directory structure
    for src in ctx.attrs.srcs:
        src_path = src.short_path
        # Get directory part for mkdir
        dir_part = "/".join(src_path.split("/")[:-1])
        if dir_part:
            script_parts.append("mkdir -p $BUCK_SCRATCH_PATH/{}".format(dir_part))
        script_parts.append(cmd_args("cp", src, "$BUCK_SCRATCH_PATH/{}".format(src_path), delimiter = " "))
    
    # Step 2: Compile ALL .lean files at once
    # Lean will resolve dependencies automatically when given all files
    # We use a simple approach: compile each file but output to scratch first,
    # then copy oleans to output dir. This lets Lean resolve deps within scratch.
    
    # Set output dirs to be within scratch for dependency resolution
    script_parts.append("export LEAN_OLEAN_DIR=$BUCK_SCRATCH_PATH/.olean")
    script_parts.append("mkdir -p $LEAN_OLEAN_DIR")
    if c_dir:
        script_parts.append("export LEAN_C_DIR=$BUCK_SCRATCH_PATH/.c")
        script_parts.append("mkdir -p $LEAN_C_DIR")
    
    # Update LEAN_PATH to include the scratch olean dir
    script_parts.append("export LEAN_PATH=$LEAN_OLEAN_DIR:$LEAN_PATH")
    
    # Compile each file in order (files without deps first, then with deps)
    # Simple heuristic: files without "/" compile first, then with "/"
    non_root_srcs = [s for s in ctx.attrs.srcs if "/" in s.short_path]
    root_srcs = [s for s in ctx.attrs.srcs if "/" not in s.short_path]
    
    # Within each group, sort by path depth (shallower first for deps)
    def sort_key(src):
        return (src.short_path.count("/"), src.short_path)
    
    non_root_srcs_sorted = sorted(non_root_srcs, key = sort_key)
    all_srcs = non_root_srcs_sorted + root_srcs
    
    # Compile in multiple passes to handle dependencies
    # Each pass tries all files; files with missing deps fail silently
    # With 3 passes, we can handle dep chains up to 3 levels deep
    script_parts.append("# Compile in 5 passes to handle dependencies")
    for pass_num in range(5):
        script_parts.append("echo 'Pass {} of 5'".format(pass_num + 1))
        for src in all_srcs:
            src_path = src.short_path
            module_path = src_path.removesuffix(".lean")
            
            # Compute directory part in Starlark
            parts = module_path.split("/")
            if len(parts) > 1:
                dir_part = "/".join(parts[:-1])
                script_parts.append("mkdir -p $LEAN_OLEAN_DIR/{}".format(dir_part))
                if c_dir:
                    script_parts.append("mkdir -p $LEAN_C_DIR/{}".format(dir_part))
            
            # Skip if already compiled (olean exists)
            script_parts.append("if [ ! -f $LEAN_OLEAN_DIR/{}.olean ]; then".format(module_path))
            
            compile_cmd = [lean, "--root=$BUCK_SCRATCH_PATH"]
            compile_cmd.extend(ctx.attrs.lean_flags)
            compile_cmd.append("-o")
            compile_cmd.append("$LEAN_OLEAN_DIR/{}.olean".format(module_path))
            
            if c_dir:
                compile_cmd.append("--c=$LEAN_C_DIR/{}.c".format(module_path))
            
            compile_cmd.append("$BUCK_SCRATCH_PATH/{}".format(src_path))
            
            # Use || true to continue on failure (deps might not be ready yet)
            script_parts.append(cmd_args(cmd_args(compile_cmd, delimiter = " "), " || true", delimiter = ""))
            script_parts.append("fi")
    
    # Step 3: Copy compiled outputs to final output dirs
    script_parts.append("cp -r $LEAN_OLEAN_DIR/* $OLEAN_DIR/")
    if c_dir:
        script_parts.append("cp -r $LEAN_C_DIR/* $C_DIR/")
    
    # Assemble full command
    script = cmd_args(script_parts, delimiter = "\n")
    
    outputs = [olean_dir.as_output()]
    env_parts = ["OLEAN_DIR=", olean_dir.as_output()]
    
    if c_dir:
        outputs.append(c_dir.as_output())
        env_parts.extend([" C_DIR=", c_dir.as_output()])
    
    cmd = cmd_args(
        "/bin/sh", "-c",
        cmd_args(env_parts, " && ", script, delimiter = ""),
    )
    
    # Hidden inputs for dependency tracking
    hidden = list(ctx.attrs.srcs)
    for dep_path in dep_paths:
        hidden.append(dep_path)
    
    ctx.actions.run(
        cmd_args(cmd, hidden = hidden),
        category = "lean_compile",
        identifier = ctx.attrs.name,
        local_only = True,  # Lean compilation needs consistent LEAN_PATH
    )
    
    sub_targets = {"olean": [DefaultInfo(default_outputs = [olean_dir])]}
    if c_dir:
        sub_targets["c"] = [DefaultInfo(default_outputs = [c_dir])]
    
    return [
        DefaultInfo(
            default_output = olean_dir,
            sub_targets = sub_targets,
        ),
        LeanLibraryInfo(
            olean_dir = olean_dir,
            c_dir = c_dir,
            lib_name = ctx.attrs.name,
            deps = ctx.attrs.deps,
        ),
    ]


lean_library = rule(
    impl = _lean_library_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "lean_flags": attrs.list(attrs.string(), default = []),
        "extract_c": attrs.bool(default = False),
    },
)

def _lean_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    """Build a Lean executable.
    
    This handles binaries that import from lean_library dependencies.
    The key challenge is that Lean requires hierarchical imports (e.g., `import Cornell.Nix`)
    to find oleans at the corresponding path (e.g., `Cornell/Nix.olean`).
    
    We use a combined compilation approach where the binary's source is compiled
    against the dependency's olean directory directly in LEAN_PATH.
    """
    lean = _get_lean()
    leanc = _get_leanc()
    lean_lib_dir = _get_lean_lib_dir()
    
    if not ctx.attrs.srcs:
        fail("lean_binary requires at least one source file")
    
    # Output
    exe = ctx.actions.declare_output(ctx.attrs.name)
    olean_dir = ctx.actions.declare_output("olean", dir = True)
    c_dir = ctx.actions.declare_output("c", dir = True)
    
    # Collect dependency olean and c directories
    dep_olean_dirs = []
    dep_c_dirs = []
    for dep in ctx.attrs.deps:
        if LeanLibraryInfo in dep:
            info = dep[LeanLibraryInfo]
            if info.olean_dir:
                dep_olean_dirs.append(info.olean_dir)
            if info.c_dir:
                dep_c_dirs.append(info.c_dir)
    
    # Script: setup, compile to C, then link
    script_parts = ["set -e"]
    script_parts.append("mkdir -p $OLEAN_DIR $C_DIR")
    
    # Build LEAN_PATH with dependency olean dirs FIRST (so imports resolve)
    # Order matters: deps first, then local, then stdlib
    lean_path_parts = []
    for dep_path in dep_olean_dirs:
        lean_path_parts.append(cmd_args(dep_path))
    lean_path_parts.extend(["$OLEAN_DIR", "$BUCK_SCRATCH_PATH"])
    if lean_lib_dir:
        lean_path_parts.append(lean_lib_dir)
    
    script_parts.append(cmd_args(
        "export LEAN_PATH=",
        cmd_args(lean_path_parts, delimiter = ":"),
        delimiter = "",
    ))
    
    # Process sources - copy to scratch and compile
    c_files = []
    
    for src in ctx.attrs.srcs:
        # Preserve directory structure from source path
        src_path = src.short_path
        module_name = src.basename.removesuffix(".lean")
        
        # Get directory part for mkdir
        dir_part = "/".join(src_path.split("/")[:-1])
        if dir_part:
            script_parts.append("mkdir -p $BUCK_SCRATCH_PATH/{}".format(dir_part))
            script_parts.append("mkdir -p $OLEAN_DIR/{}".format(dir_part))
            script_parts.append("mkdir -p $C_DIR/{}".format(dir_part))
        
        # Paths
        dest_path = "$BUCK_SCRATCH_PATH/{}".format(src_path)
        module_path = src_path.removesuffix(".lean")
        c_file = "$C_DIR/{}.c".format(module_path)
        olean_file = "$OLEAN_DIR/{}.olean".format(module_path)
        
        c_files.append(c_file)
        
        # Copy source preserving structure
        script_parts.append(cmd_args("cp", src, dest_path, delimiter = " "))
        
        # Compile
        compile_cmd = [
            lean,
            "--root=$BUCK_SCRATCH_PATH",
            "-o", olean_file,
            "--c={}".format(c_file),
        ]
        compile_cmd.extend(ctx.attrs.lean_flags)
        compile_cmd.append(dest_path)
        
        script_parts.append(cmd_args(compile_cmd, delimiter = " "))
    
    # Link with leanc
    link_cmd = [leanc, "-o", exe.as_output()]
    link_cmd.extend(ctx.attrs.link_flags)
    
    # Add all C files from this binary
    for c_file in c_files:
        link_cmd.append(c_file)
    
    # Add dependency C files (recursively find all .c files in each dep dir)
    for dep_c_dir in dep_c_dirs:
        link_cmd.append(cmd_args("$(find", dep_c_dir, "-name '*.c')", delimiter = " "))
    
    script_parts.append(cmd_args(link_cmd, delimiter = " "))
    
    script = cmd_args(script_parts, delimiter = "\n")
    
    cmd = cmd_args(
        "/bin/sh", "-c",
        cmd_args(
            "OLEAN_DIR=", olean_dir.as_output(),
            " C_DIR=", c_dir.as_output(),
            " && ", script,
            delimiter = "",
        ),
    )
    
    hidden = list(ctx.attrs.srcs)
    hidden.extend(dep_olean_dirs)
    hidden.extend(dep_c_dirs)
    
    ctx.actions.run(
        cmd_args(cmd, hidden = hidden),
        category = "lean_link",
        identifier = ctx.attrs.name,
        local_only = True,
    )
    
    return [
        DefaultInfo(default_output = exe),
        RunInfo(args = cmd_args(exe)),
    ]


lean_binary = rule(
    impl = _lean_binary_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "root_module": attrs.option(attrs.string(), default = None),
        "lean_flags": attrs.list(attrs.string(), default = []),
        "link_flags": attrs.list(attrs.string(), default = []),
    },
)

def _lean_c_library_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    lean = _get_lean()
    lean_include_dir = _get_lean_include_dir()
    lean_lib_dir = _get_lean_lib_dir()
    
    if not ctx.attrs.srcs:
        return [DefaultInfo(), LeanCLibraryInfo()]
    
    # Outputs
    c_dir = ctx.actions.declare_output("c", dir = True)
    olean_dir = ctx.actions.declare_output("olean", dir = True)
    include_dir = ctx.actions.declare_output("include", dir = True)
    obj_dir = ctx.actions.declare_output("obj", dir = True)
    archive = ctx.actions.declare_output("lib{}.a".format(ctx.attrs.name))
    
    # Collect dependencies
    dep_paths = []
    for dep in ctx.attrs.deps:
        if LeanLibraryInfo in dep:
            info = dep[LeanLibraryInfo]
            if info.olean_dir:
                dep_paths.append(info.olean_dir)
    
    # Build LEAN_PATH - use env var since olean_dir is output
    lean_path_parts = ["$OLEAN_DIR"]
    if lean_lib_dir:
        lean_path_parts.append(lean_lib_dir)
    for dep_path in dep_paths:
        lean_path_parts.append(cmd_args(dep_path))
    
    # Get C compiler from cxx config (we use our Clang, not leanc's default)
    cc = read_root_config("cxx", "cxx", "clang++")
    
    script_parts = ["set -e"]
    script_parts.append("mkdir -p $OLEAN_DIR $C_DIR $INCLUDE_DIR $OBJ_DIR")
    
    script_parts.append(cmd_args(
        "export LEAN_PATH=",
        cmd_args(lean_path_parts, delimiter = ":"),
        delimiter = "",
    ))
    
    # Compile Lean to C
    # Lean requires sources to be in --root directory, so we copy to scratch
    c_files = []
    for src in ctx.attrs.srcs:
        module_name = src.basename.removesuffix(".lean")
        c_file = "{}.c".format(module_name)
        c_files.append(c_file)
        
        # Copy source to scratch dir (Lean's --root requirement)
        script_parts.append(cmd_args("cp", src, "$BUCK_SCRATCH_PATH/", delimiter = " "))
        
        compile_cmd = [
            lean,
            "--root=$BUCK_SCRATCH_PATH",
            "-o", "$OLEAN_DIR/{}.olean".format(module_name),
            "--c=$C_DIR/{}".format(c_file),
        ]
        compile_cmd.extend(ctx.attrs.lean_flags)
        compile_cmd.append("$BUCK_SCRATCH_PATH/{}".format(src.basename))
        
        script_parts.append(cmd_args(compile_cmd, delimiter = " "))
    
    # Generate header file for FFI exports
    # Lean generates lean.h style headers; we create a wrapper
    header_content = [
        "// Generated by lean_c_library: {}".format(ctx.attrs.name),
        "#pragma once",
        "#include <lean/lean.h>",
        "",
        "// Exported functions from Lean",
    ]
    for export in ctx.attrs.exports:
        header_content.append("extern lean_object* {}(lean_object*);".format(export))
    
    script_parts.append(cmd_args(
        "cat > $INCLUDE_DIR/{}.h << 'LEAN_HEADER_EOF'\n{}\nLEAN_HEADER_EOF".format(
            ctx.attrs.name,
            "\n".join(header_content),
        ),
    ))
    
    # Compile C to objects
    for c_file in c_files:
        obj_file = c_file.removesuffix(".c") + ".o"
        
        cc_cmd = [cc, "-c", "-O2", "-fPIC"]
        if lean_include_dir:
            cc_cmd.extend(["-I", lean_include_dir])
        cc_cmd.extend(["-I", "$INCLUDE_DIR"])
        cc_cmd.extend(ctx.attrs.cflags)
        cc_cmd.extend(["-o", "$OBJ_DIR/{}".format(obj_file)])
        cc_cmd.append("$C_DIR/{}".format(c_file))
        
        script_parts.append(cmd_args(cc_cmd, delimiter = " "))
    
    # Archive objects
    script_parts.append(cmd_args(
        "ar rcs", archive.as_output(), "$OBJ_DIR/*.o",
        delimiter = " ",
    ))
    
    script = cmd_args(script_parts, delimiter = "\n")
    
    cmd = cmd_args(
        "/bin/sh", "-c",
        cmd_args(
            "OLEAN_DIR=", olean_dir.as_output(),
            " C_DIR=", c_dir.as_output(),
            " INCLUDE_DIR=", include_dir.as_output(),
            " OBJ_DIR=", obj_dir.as_output(),
            " && ", script,
            delimiter = "",
        ),
    )
    
    hidden = list(ctx.attrs.srcs)
    hidden.extend(dep_paths)
    
    ctx.actions.run(
        cmd_args(cmd, hidden = hidden),
        category = "lean_c_extract",
        identifier = ctx.attrs.name,
        local_only = True,
    )
    
    return [
        DefaultInfo(
            default_output = archive,
            sub_targets = {
                "c": [DefaultInfo(default_outputs = [c_dir])],
                "include": [DefaultInfo(default_outputs = [include_dir])],
                "olean": [DefaultInfo(default_outputs = [olean_dir])],
            },
        ),
        LeanLibraryInfo(
            olean_dir = olean_dir,
            c_dir = c_dir,
            lib_name = ctx.attrs.name,
            deps = ctx.attrs.deps,
        ),
        LeanCLibraryInfo(
            c_sources = [],  # We don't track individual files in dir output
            include_dir = include_dir,
            objects = [],
            archive = archive,
        ),
    ]


lean_c_library = rule(
    impl = _lean_c_library_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "lean_flags": attrs.list(attrs.string(), default = []),
        "cflags": attrs.list(attrs.string(), default = []),
        "exports": attrs.list(attrs.string(), default = []),
    },
)

def _lean_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    """Lean toolchain with paths from .buckconfig.local"""
    # Read from config, fall back to attrs
    lean = read_root_config("lean", "lean", ctx.attrs.lean)
    leanc = read_root_config("lean", "leanc", ctx.attrs.leanc)
    lean_lib_dir = read_root_config("lean", "lean_lib_dir", ctx.attrs.lean_lib_dir)
    lean_include_dir = read_root_config("lean", "lean_include_dir", ctx.attrs.lean_include_dir)

    return [
        DefaultInfo(),
        LeanToolchainInfo(
            lean = lean,
            leanc = leanc,
            lean_lib_dir = lean_lib_dir,
            lean_include_dir = lean_include_dir,
        ),
    ]


lean_toolchain = rule(
    impl = _lean_toolchain_impl,
    attrs = {
        "lean": attrs.string(default = "lean"),
        "leanc": attrs.string(default = "leanc"),
        "lean_lib_dir": attrs.option(attrs.string(), default = None),
        "lean_include_dir": attrs.option(attrs.string(), default = None),
    },
    is_toolchain_rule = True,
)

def _system_lean_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    fail("""
system_lean_toolchain is disabled.

Zeitschrift does not support fallback toolchains.
Configure your Lean toolchain via Nix:

    [lean]
    lean = /nix/store/.../bin/lean
    leanc = /nix/store/.../bin/leanc
    lean_lib_dir = /nix/store/.../lib/lean
    lean_include_dir = /nix/store/.../include

Then run: nix develop

If you see this error, your .buckconfig.local is missing or stale.
""")


system_lean_toolchain = rule(
    impl = _system_lean_toolchain_impl,
    attrs = {

    },
    is_toolchain_rule = True,
)

def _lean_lake_build_impl(ctx: AnalysisContext) -> list[Provider]:
    """"""
    fail("""
lean_lake_build is disabled.

Lake introduces non-hermetic builds that bypass Buck2's caching and
content-addressed derivation model. Mathlib downloads (~2GB) and
.lake/ caches are not tracked.

Options:
  1. Use lean_library/lean_binary for standalone Lean code (no Lake deps)
  2. Manage Mathlib via Nix overlay (recommended for large proofs)
  3. Build outside Buck2 with 'lake build' directly

See: toolchains/lean.bzl for lean_library, lean_binary, lean_c_library
""")


lean_lake_build = rule(
    impl = _lean_lake_build_impl,
    attrs = {
        "srcs": attrs.list(attrs.source(), default = []),
        "lakefile": attrs.option(attrs.source(), default = None),
        "toolchain_file": attrs.option(attrs.source(), default = None),
    },
)

