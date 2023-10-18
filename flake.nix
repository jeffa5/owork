{
  description = "owork";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
    };
  in {
    packages.${system} = {
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

    overlays.default = _final: _prev: self.packages.${system};

    devShells.${system}.default = pkgs.mkShell {
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
      ];
    };
  };
}
