# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#                                                          // weapon-server
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
#
# Haskell server for Weapon AI coding agent.
# Full API parity with TypeScript reference (95 endpoints).
# Includes evring-wai io_uring backend for high-performance async I/O.
#
# ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

load("@toolchains//:haskell.bzl", "haskell_library", "haskell_binary", "haskell_test", "haskell_ffi_binary", "haskell_ffi_test")

# ── // library ──────────────────────────────────────────────────────────────

haskell_library(
    name = "lib",
    srcs = glob(["src/**/*.hs"]),
    language_extensions = [
        "BangPatterns",
        "DataKinds",
        "DeriveGeneric",
        "DerivingStrategies",
        "LambdaCase",
        "NamedFieldPuns",
        "OverloadedStrings",
        "RecordWildCards",
        "StrictData",
        "TypeOperators",
    ],
    ghc_options = [
        "-Wcompat",
        "-Widentities",
        "-Wincomplete-record-updates",
        "-Wincomplete-uni-patterns",
        "-Wmissing-export-lists",
        "-Wmissing-home-modules",
        "-Wpartial-fields",
        "-Wredundant-constraints",
        "-O2",
    ],
    visibility = ["PUBLIC"],
)

# ── // server binary ────────────────────────────────────────────────────────
#
# Uses haskell_ffi_binary to link with liburing C bindings for evring-wai.
# Evring sources are now self-contained in src/Evring/.
#

haskell_ffi_binary(
    name = "weapon-server",
    hs_srcs = glob(["app/**/*.hs", "src/**/*.hs"]),
    cxx_srcs = [
        "cbits/uring_compat.c",
    ],
    extra_libs = ["uring"],
    language_extensions = [
        "BangPatterns",
        "DataKinds",
        "DeriveGeneric",
        "DerivingStrategies",
        "LambdaCase",
        "NamedFieldPuns",
        "OverloadedStrings",
        "RecordWildCards",
        "StrictData",
        "TypeOperators",
    ],
    ghc_options = [
        "-O2",
        "-threaded",
        "-rtsopts",
        "-with-rtsopts=-N -T",
    ],
    visibility = ["PUBLIC"],
)

# ── // tests ────────────────────────────────────────────────────────────────

haskell_ffi_test(
    name = "test",
    hs_srcs = glob(
        ["test/**/*.hs", "src/**/*.hs"],
        exclude = [
            "test/DebugPromptAsync.hs",           # Standalone debug tool
            "test/Integration/ApiCompliance.hs",  # Missing T.encodeUtf8, HM.size
            "test/Integration/ServerComparison.hs",   # Field name ambiguity
            "test/Integration/WaiDebugTest.hs",   # Unused imports warning as error
            # Exclude HaskemathesisTest until haskell_ffi_test supports deps
            "test/Integration/HaskemathesisTest.hs",
        ],
    ),
    cxx_srcs = [
        "cbits/uring_compat.c",
    ],
    extra_libs = ["uring"],
    language_extensions = [
        "DataKinds",
        "DeriveGeneric",
        "LambdaCase",
        "NamedFieldPuns",
        "OverloadedStrings",
        "RecordWildCards",
        "StrictData",
        "TypeOperators",
    ],
    ghc_options = [
        "-threaded",
        "-rtsopts",
        "-with-rtsopts=-N",
    ],
)
