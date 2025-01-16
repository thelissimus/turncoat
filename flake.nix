{
  description = "turncoat - track unfollowers";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { localSystem = { inherit system; }; };
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          zlib
        ];
      };
    });
}
