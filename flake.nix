{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        haskellProjects.default = {
          # projectRoot = builtins.toString (lib.fileset.toSource {
          #   root = ./.;
          #   fileset = lib.fileset.unions [ ./src ./haskell-template.cabal ];
          # });
          settings = { buildInputs = [ pkgs.zlib ]; };
          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              fourmolu = hp.fourmolu;
              cabal-fmt = hp.cabal-fmt;
              # ghcid = null;
              zlib = pkgs.zlib;
            };

            hlsCheck.enable = true;
            mkShellArgs.shellHook = ''
              export STAN_USE_DEFAULT_CONFIG=True
            '';
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.project-templates;
      };
    };
}
