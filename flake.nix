{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    denv = {
      url = "github:iliayar/env.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, denv, ... }:
    flake-parts.lib.mkFlake { inputs = denv.inputs; } {
      imports = [ denv.flakeModules.default ];
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, lib, ... }: {
        packages = rec {
            cbf = pkgs.haskellPackages.mkDerivation {
                pname = "cbf";
                version = "1.0.0";
                src = ./.;

                isExecutable = true;
                buildDepends = with pkgs.haskellPackages; [
                    HUnit
                    megaparsec
                    neat-interpolation
                    parser-combinators
                    vector
                    data-default
                ];
                license = lib.licenses.wtfpl;
            };
            default = cbf;
        };
      };
    };
}
