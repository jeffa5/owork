{
  description = "owork";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            system = system;
          };
        in
        rec
        {
          packages = {
            owork = pkgs.ocamlPackages.buildDunePackage {
              pname = "owork";
              version = "0.1.0";
              src = ./.;
              useDune2 = true;
              buildInputs = with pkgs.ocamlPackages; [
                lwt
                fmt
                logs
                cmdliner
                duration
                ppx_expect
                ppx_deriving
                astring
              ];
            };
          };

          defaultPackage = packages.owork;

          apps = {
            owork = flake-utils.lib.mkApp {
              drv = packages.owork;
            };
          };

          defaultApp = apps.owork;

          devShell =
            pkgs.mkShell {
              buildInputs = with pkgs; [
                ocaml
                (with ocamlPackages; [
                  dune_2
                  astring
                  cmdliner
                  duration
                  fmt
                  logs
                  lwt
                  ppx_deriving
                  ppx_expect
                  findlib
                ])

                nixpkgs-fmt
                rnix-lsp
              ];
            };
        });
}
