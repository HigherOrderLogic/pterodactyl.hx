{
  lib,
  stdenv,
  rustPlatform,
  fd,
}: let
  inherit (stdenv) buildPlatform;
in
  rustPlatform.buildRustPackage (finalAttrs: {
    name = "pterodactyl";

    src = lib.fileset.toSource {
      root = ./.;
      fileset = lib.fileset.unions [./src ./Cargo.toml ./Cargo.lock ./cog.scm ./term.scm];
    };

    cargoLock = {
      allowBuiltinFetchGit = true;
      lockFile = "${finalAttrs.src}/Cargo.lock";
    };

    cargoBuildFlags = ["--lib"];

    nativeBuildInputs = [fd];

    installPhase = ''
      runHook preInstall

      fd -t f -e scm -x install -Dm 644 '{}' -t "$out/lib/steel/cogs/${finalAttrs.name}/{//}"
      for file in target/${buildPlatform.rust.cargoShortTarget}/release/*${buildPlatform.extensions.sharedLibrary}; do
        install -Dm 755 "$file" -t $out/lib/steel/native/
      done

      runHook postInstall
    '';

    dontCargoInstall = true;

    passthru.pluginEntrypoint = "term.scm";
  })
