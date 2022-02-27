{
  description = "Pokédex para disciplina de PFPA 2022";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    let
      name = "pokedex-pfpa";
      overlay = final: prev: {
        ${name} = final.haskellPackages.callCabal2nix name ./. { };
      };
      overlays = [ overlay ];
    in
    rec {
      inherit overlay overlays;

      nixosModules."${name}" = import ./module.nix;
      nixosModule = nixosModules."${name}";
    } //
    (utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system overlays; };
      in
      rec {
        # nix build
        packages.${name} = pkgs.${name};
        defaultPackage = packages.${name};

        # nix run
        apps.${name} = utils.lib.mkApp { drv = packages.${name}; };
        defaultApp = apps.${name};

        # nix develop
        devShell = pkgs.mkShell {
          inputsFrom = [ defaultPackage ];
          buildInputs = with pkgs; [
            zlib.dev
            haskell-language-server
            cabal-install
            postgresql
            httpie
          ];
        };
      }));
}

