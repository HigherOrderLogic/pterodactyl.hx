{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    inherit (nixpkgs) lib;

    forEachSystem = fn: lib.genAttrs lib.systems.flakeExposed (system: fn system nixpkgs.legacyPackages.${system});
  in {
    formatter = forEachSystem (system: pkgs: let
      rustfmt' = pkgs.rustfmt.override {asNightly = true;};
    in
      pkgs.writeShellApplication {
        name = "fmt";
        runtimeInputs = builtins.attrValues {
          inherit (pkgs) alejandra fd cargo;
          inherit rustfmt';
        };
        text = ''
          fd "$@" -t f -e nix -X alejandra -q '{}'
          cargo fmt --all
        '';
      });

    packages = forEachSystem (system: pkgs: {default = pkgs.callPackage ./package.nix {};});

    devShells = forEachSystem (system: pkgs: let
      rustfmt' = pkgs.rustfmt.override {asNightly = true;};
    in {
      default = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.packages.${system};
        packages = builtins.attrValues {
          inherit (pkgs) rustc cargo clippy;
          inherit rustfmt';
        };
      };
    });
  };
}
