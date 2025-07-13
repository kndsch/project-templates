# Project Templates

This is a simple cli utility to bootstrap projects from templates, similar to [cookiecutter](https://github.com/cookiecutter/cookiecutter) or
[copier](https://github.com/copier-org/copier).

A common use case not covered by these options is supplementing an existing code base or leveraging tool-specific workflows like `cabal init` or `uv init`.

In addition to ordinary template expansion, it supports running hooks before or after project creation and adding additional files to the current directory.

See the templates in ./examples for how this works.


## Installing

The easiest way to use this is via nix:

```bash
# Run directly from GitHub
nix run github:kndsch/project-templates -- <template>

# Or install to your profile
nix profile install github:kndsch/project-templates
```
