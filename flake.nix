{
  description = "virtual-dts-mode";

  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, devshell, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; overlays = [ devshell.overlay ]; };
      in
      {
        devShells = {
          default = pkgs.callPackage ./shell.nix { };
        };
      });

  # Automatic nix.conf settings (accepted automatically when 'accept-flake-config = true')
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [ "https://virtual-dts-mode.cachix.org" ];
    extra-trusted-public-keys = [ "virtual-dts-mode.cachix.org-1:yrlePU8YmjBgLD4tK5wPeMrTKeRLSPKmSnbndRgksIE=" ];
  };
}