{
  mkDerivation,
  aeson,
  base,
  base16-bytestring,
  base64-bytestring,
  bytestring,
  case-insensitive,
  containers,
  crypton,
  crypton-connection,
  data-default-class,
  dhall,
  dhall-json,
  directory,
  filepath,
  haskemathesis ? null, # Optional: only needed for tests
  haskemathesis-tasty ? null, # Optional: only needed for tests
  hedgehog ? null,
  hspec ? null,
  hspec-core ? null,
  hspec-hedgehog ? null,
  http-client,
  http-client-tls,
  http-types,
  katip,
  lib,
  memory,
  mtl,
  network,
  openapi3 ? null,
  parquet-ffi,
  posix-pty,
  primitive,
  process,
  random,
  regex-pcre ? null,
  stan ? null,
  servant,
  servant-server,
  stm,
  tasty ? null,
  tasty-hedgehog ? null,
  tasty-hspec ? null,
  temporary ? null,
  text,
  time,
  tls,
  transformers,
  unix,
  unliftio-core,
  unordered-containers,
  vault,
  vector,
  wai,
  wai-extra ? null,
  wai-websockets,
  warp,
  websockets,
  pkgs,
}:
let
  sourceFiles = lib.fileset.difference ./. (
    lib.fileset.unions [
      ./nix
      ./flake.nix
      ./flake.lock
    ]
  );
in
mkDerivation {
  pname = "weapon-server";
  version = "0.1.0.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = sourceFiles;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    base64-bytestring
    bytestring
    case-insensitive
    containers
    crypton
    crypton-connection
    data-default-class
    dhall
    dhall-json
    directory
    filepath
    http-client
    http-client-tls
    http-types
    katip
    memory
    mtl
    network
    posix-pty
    primitive
    process
    random
    servant
    servant-server
    stm
    text
    time
    tls
    unix
    unliftio-core
    vault
    vector
    wai
    wai-websockets
    warp
    websockets
  ];
  librarySystemDepends = [
    pkgs.liburing
    parquet-ffi
  ];

  # Point GHC to parquet-ffi library and headers
  # The cabal file has relative paths that work for local cargo builds,
  # but nix builds need absolute paths to the nix store
  preConfigure = ''
    export NIX_LDFLAGS="$NIX_LDFLAGS -L${parquet-ffi}/lib"
    export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${parquet-ffi}/include"
  '';

  executableHaskellDepends = [
    aeson
    base
    bytestring
    containers
    directory
    filepath
    http-types
    katip
    process
    servant-server
    stm
    text
    time
    wai
    wai-websockets
    warp
    websockets
  ];
  # Enable production mode for nix builds (INFO log level instead of DEBUG)
  configureFlags = [ "-f production" ];
  # Skip tests for now - dhall schema/Haskell field name mismatch causes test failures
  # TODO: Fix test setup to properly locate dhall files in nix build environment
  doCheck = false;
  testHaskellDepends = lib.filter (x: x != null) [
    aeson
    base
    base64-bytestring
    bytestring
    containers
    directory
    filepath
    haskemathesis
    haskemathesis-tasty
    hedgehog
    hspec
    hspec-core
    hspec-hedgehog
    http-client
    http-client-tls
    http-types
    katip
    openapi3
    process
    regex-pcre
    servant
    servant-server
    stm
    tasty
    tasty-hedgehog
    tasty-hspec
    temporary
    text
    transformers
    unix
    unordered-containers
    wai
    wai-extra
  ];
  testToolDepends = lib.filter (x: x != null) [
    pkgs.ripgrep
    pkgs.fd
    pkgs.git
    stan
  ];
  preCheck = ''
    export HOME="$(mktemp -d)"
  '';
  postCheck = lib.optionalString (stan != null) ''
    report="$(stan 2>$1)"

    printf '%s\n' "$report"
    printf '%s\n' "$report" | grep -F "Stan did not find any observations at the moment"
  '';
  homepage = "https://github.com/straylight-software/weapon-server-hs";
  description = "Haskell server for Weapon AI coding agent";
  license = lib.licensesSpdx."MIT";
  mainProgram = "weapon-server";
}
