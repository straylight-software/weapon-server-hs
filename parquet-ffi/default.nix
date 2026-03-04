{
  lib,
  rustPlatform,
  pkg-config,
}:

rustPlatform.buildRustPackage rec {
  pname = "parquet-ffi";
  version = "0.1.0";

  src = ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
  };

  nativeBuildInputs = [ pkg-config ];

  # Output both static and dynamic libraries
  # rustPlatform.buildRustPackage installs to $out/lib automatically for cdylib/staticlib
  postInstall = ''
    mkdir -p $out/include $out/lib
    cp include/parquet_ffi.h $out/include/

    # Ensure libraries are in lib/ (buildRustPackage should do this, but be explicit)
    if [ -f target/release/libparquet_ffi.so ]; then
      cp target/release/libparquet_ffi.so $out/lib/
    fi
    if [ -f target/release/libparquet_ffi.a ]; then
      cp target/release/libparquet_ffi.a $out/lib/
    fi
    # macOS
    if [ -f target/release/libparquet_ffi.dylib ]; then
      cp target/release/libparquet_ffi.dylib $out/lib/
    fi
  '';

  meta = with lib; {
    description = "Parquet FFI for Haskell telemetry";
    license = licenses.mit;
  };
}
