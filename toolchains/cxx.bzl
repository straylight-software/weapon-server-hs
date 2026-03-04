# Generated from Dhall - DO NOT EDIT
# LLVM C++ toolchain using hermetic Nix store paths.
# No GCC. No nvcc. Ever.


load("@prelude//cxx:cxx_toolchain_types.bzl", "BinaryUtilitiesInfo", "CCompilerInfo", "CvtresCompilerInfo", "CxxCompilerInfo", "CxxInternalTools", "CxxPlatformInfo", "CxxToolchainInfo", "LinkerInfo", "LinkerType", "PicBehavior", "RcCompilerInfo", "ShlibInterfacesMode")
load("@prelude//cxx:headers.bzl", "HeaderMode")
load("@prelude//linking:link_info.bzl", "LinkStyle")
load("@prelude//linking:lto.bzl", "LtoMode")





def _run_info(args):
    return None if args == None else RunInfo(args = [args])



def _llvm_toolchain_impl(ctx: AnalysisContext) -> list[Provider]:
    """LLVM 22 toolchain with paths from .buckconfig.local"""
    # Read tool paths from config (fall back to PATH lookup)
    cc = read_root_config("cxx", "cc", "clang")
    cxx = read_root_config("cxx", "cxx", "clang++")
    ar = read_root_config("cxx", "ar", "llvm-ar")
    ld = read_root_config("cxx", "ld", "ld.lld")

    # Read Turing Registry flags from config
    config_c_flags_str = read_root_config("cxx.flags", "c_flags", "")
    config_cxx_flags_str = read_root_config("cxx.flags", "cxx_flags", "")
    config_c_flags = config_c_flags_str.split() if config_c_flags_str else []
    config_cxx_flags = config_cxx_flags_str.split() if config_cxx_flags_str else []

    # Build include flags from config paths
    include_flags = []

    clang_resource_dir = read_root_config("cxx", "clang_resource_dir", None)
    if clang_resource_dir:
        include_flags.append("-resource-dir=" + clang_resource_dir)
        include_flags.append("-isystem" + clang_resource_dir + "/include")

    gcc_include = read_root_config("cxx", "gcc_include", None)
    if gcc_include:
        include_flags.append("-isystem" + gcc_include)

    gcc_include_arch = read_root_config("cxx", "gcc_include_arch", None)
    if gcc_include_arch:
        include_flags.append("-isystem" + gcc_include_arch)

    glibc_include = read_root_config("cxx", "glibc_include", None)
    if glibc_include:
        include_flags.append("-isystem" + glibc_include)

    mdspan_include = read_root_config("cxx", "mdspan_include", None)
    if mdspan_include:
        include_flags.append("-isystem" + mdspan_include)

    # Extra include dirs from cxx.libraries (colon-separated)
    extra_include_dirs = read_root_config("cxx", "extra_include_dirs", None)
    if extra_include_dirs:
        for dir in extra_include_dirs.split(":"):
            if dir:
                include_flags.append("-isystem" + dir)

    # Build link flags from config paths
    llvm_bin_dir = ld.rsplit("/", 1)[0] if "/" in ld else None
    extra_link_flags = []
    if llvm_bin_dir:
        extra_link_flags.append("-B" + llvm_bin_dir)
    extra_link_flags.append("-fuse-ld=lld")

    glibc_lib = read_root_config("cxx", "glibc_lib", None)
    if glibc_lib:
        extra_link_flags.append("-B" + glibc_lib)
        extra_link_flags.append("-L" + glibc_lib)
        extra_link_flags.append("-Wl,-rpath," + glibc_lib)

    gcc_lib = read_root_config("cxx", "gcc_lib", None)
    if gcc_lib:
        extra_link_flags.append("-B" + gcc_lib)
        extra_link_flags.append("-L" + gcc_lib)
        extra_link_flags.append("-Wl,-rpath," + gcc_lib)

    gcc_lib_base = read_root_config("cxx", "gcc_lib_base", None)
    if gcc_lib_base:
        extra_link_flags.append("-L" + gcc_lib_base)
        extra_link_flags.append("-Wl,-rpath," + gcc_lib_base)

    # Extra lib dirs from cxx.libraries (colon-separated)
    extra_lib_dirs = read_root_config("cxx", "extra_lib_dirs", None)
    if extra_lib_dirs:
        for dir in extra_lib_dirs.split(":"):
            if dir:
                extra_link_flags.append("-L" + dir)
                extra_link_flags.append("-Wl,-rpath," + dir)

    # Combine flags
    c_flags = include_flags + config_c_flags + ctx.attrs.c_extra_flags
    cxx_flags = include_flags + config_cxx_flags + ctx.attrs.cxx_extra_flags
    link_flags = extra_link_flags + ctx.attrs.link_flags

    return [
        DefaultInfo(),
        CxxToolchainInfo(
            internal_tools = ctx.attrs._internal_tools[CxxInternalTools],
            linker_info = LinkerInfo(
                linker = _run_info(cxx),
                linker_flags = link_flags,
                post_linker_flags = [],
                archiver = _run_info(ar),
                archiver_type = "gnu",
                archiver_supports_argfiles = True,
                generate_linker_maps = False,
                lto_mode = LtoMode("none"),
                type = LinkerType("gnu"),
                link_binaries_locally = True,
                link_libraries_locally = True,
                archive_objects_locally = True,
                use_archiver_flags = True,
                static_dep_runtime_ld_flags = [],
                static_pic_dep_runtime_ld_flags = [],
                shared_dep_runtime_ld_flags = [],
                independent_shlib_interface_linker_flags = [],
                shlib_interfaces = ShlibInterfacesMode("disabled"),
                link_style = LinkStyle(ctx.attrs.link_style),
                link_weight = 1,
                binary_extension = "",
                object_file_extension = "o",
                shared_library_name_default_prefix = "lib",
                shared_library_name_format = "{}.so",
                shared_library_versioned_name_format = "{}.so.{}",
                static_library_extension = "a",
                force_full_hybrid_if_capable = False,
                is_pdb_generated = False,
                link_ordering = None,
            ),
            bolt_enabled = False,
            binary_utilities_info = BinaryUtilitiesInfo(
                nm = RunInfo(args = ["llvm-nm"]),
                objcopy = RunInfo(args = ["llvm-objcopy"]),
                objdump = RunInfo(args = ["llvm-objdump"]),
                ranlib = RunInfo(args = ["llvm-ranlib"]),
                strip = RunInfo(args = ["llvm-strip"]),
                dwp = None,
                bolt_msdk = None,
            ),
            cxx_compiler_info = CxxCompilerInfo(
                compiler = _run_info(cxx),
                preprocessor_flags = [],
                compiler_flags = cxx_flags,
                compiler_type = "clang",
            ),
            c_compiler_info = CCompilerInfo(
                compiler = _run_info(cc),
                preprocessor_flags = [],
                compiler_flags = c_flags,
                compiler_type = "clang",
            ),
            as_compiler_info = CCompilerInfo(
                compiler = _run_info(cc),
                compiler_type = "clang",
            ),
            asm_compiler_info = CCompilerInfo(
                compiler = _run_info(cc),
                compiler_type = "clang",
            ),
            header_mode = HeaderMode("symlink_tree_only"),
            cpp_dep_tracking_mode = "makefile",
            pic_behavior = PicBehavior("supported"),
            llvm_link = RunInfo(args = ["llvm-link"]),
        ),
        CxxPlatformInfo(name = "x86_64"),
    ]


llvm_toolchain = rule(
    impl = _llvm_toolchain_impl,
    attrs = {
        "c_extra_flags": attrs.list(attrs.string(), default = []),
        "cxx_extra_flags": attrs.list(attrs.string(), default = []),
        "link_flags": attrs.list(attrs.string(), default = []),
        "link_style": attrs.string(default = "static"),
        "_internal_tools": attrs.dep(default = "prelude//cxx/tools:internal_tools"),
    },
    is_toolchain_rule = True,
)

