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
          rdkafka
        ];

        # Navigate to the project directory when entering the shell
        shellHook = ''
           PATH=~/.cabal/bin:$PATH
           LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
           export LIBRARY_PATH=${pkgs.rdkafka}/lib
           export C_INCLUDE_PATH=${pkgs.rdkafka}/include

           # export KAFKA_TEST_BROKER=$(ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p' | head -n 1)
          cd taurus-decoder
          echo "Haskell devShell"
        '';
      };
    };
}

