{
  description = "";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs/041c867bad68dfe34b78b2813028a2e2ea70a23c";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            keydoku = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9101";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                ghcid
                ormolu
                pkg-config
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.keydoku.flake { };
        windowsPackages =
          if pkgs.stdenv.hostPlatform.isLinux then
            let
              pkgsWindows = import nixpkgs {
                localSystem = system;
                crossSystem = { config = "x86_64-w64-mingw32"; };
                inherit overlays;
                inherit (haskellNix) config;
              };
              keydokuWindows = pkgsWindows.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc984";
                projectFileName = "cabal.project.windows";
              };
              windowsExe = keydokuWindows.hsPkgs.keydoku.components.exes.keydoku;
            in {
              windows = windowsExe;
              windows-zip = pkgs.runCommand "keydoku-windows.zip"
                { nativeBuildInputs = [ pkgs.zip ]; }
                ''
                  mkdir -p staging
                  cp -r ${windowsExe}/bin staging/keydoku
                  chmod -R u+w staging
                  (
                    cd staging
                    zip -r "$out" keydoku
                  )
                '';
            }
          else
            { };
      in flake // {
        packages = flake.packages // windowsPackages // {
          default = flake.packages."keydoku:exe:keydoku";
          container = pkgs.dockerTools.buildLayeredImage {
            name = "keydoku";
            # tag = self.rev or "dirty";
            tag = "latest"; # TODO: Better tagging
            contents = [ flake.packages."keydoku:exe:keydoku" ./web ];
            config = {
              Cmd = [ ];
              Entrypoint = [ "/bin/keydoku" ];
            };
          };
        };
      });
}
