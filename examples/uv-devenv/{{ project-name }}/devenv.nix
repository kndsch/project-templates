{ pkgs, lib, config, inputs, ... }:

{
  dotenv.disableHint = true;

  languages.python.enable = true;
  languages.python.uv = {
    enable = true;
    sync.enable = true;
    # uv.sync.allExtras = true;
    uv.sync.allGroups = true;
  };

  tasks = {
    "venv:patchelf-ruff" = {
      description =
        "Patch ruff binary. It is dynamically linked to ld, which will not work with Nix";
      exec = ''
        echo "Patching ruff binary"
        ruff="$VIRTUAL_ENV/bin/ruff"
        patchelf="${pkgs.patchelf}/bin/patchelf"
        interpreter="${pkgs.stdenv.cc.bintools.dynamicLinker}"
        "$patchelf" --set-interpreter "$interpreter" "$ruff"
      '';
      status =
        "! (command -v ruff >/dev/null 2>&1 && ! ruff --version >/dev/null 2>&1)";
      # after = [ "devenv:python:poetry" ];
      before = [ "devenv:enterShell" ];
    };
  };
}
