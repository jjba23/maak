{
  description = "Maak - The infinitely extensible command runner à la Make (Guile Scheme)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          maak = pkgs.callPackage ./default.nix {};
          default = self.packages.${system}.maak;
        });

      apps = forAllSystems (system: {
        maak = {
          type = "app";
          program = "${self.packages.${system}.maak}/bin/maak";
        };
        default = self.apps.${system}.maak;
      });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.maak ];
            packages = with pkgs; [
              guile
              coreutils
              util-linux
            ];
          };
        });
    };
}
