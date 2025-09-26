{
  description = "Taurus Decoder dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskell.compiler.ghc96
          haskellPackages.cabal-install
          gmp
        ];

        # Navigate to the project directory when entering the shell
        shellHook = ''
          cd taurus-decoder
          echo "Haskell devShell"
        '';
      };
    };
}

