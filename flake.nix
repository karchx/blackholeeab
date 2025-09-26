{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    taurus-decoder.url = "./taurus-decoder";
  };

  outputs = { self, nixpkgs, taurus-decoder, ... }:
        let
            system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
        in {
            devShells.${system}.default = pkgs.mkShell {
                buildInputs = with pkgs; [
                    kcat
                ];

                shellHook = ''
                    alias kls="kcat -L -b localhost:9092,localhost:9093,localhost:9094"
                '';
        };
        taurus-decoder = taurus-decoder.devShells.${system}.default;
  };
}
