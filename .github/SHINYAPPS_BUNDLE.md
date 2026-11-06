# ShinyApps bundle: build and release contract

This directory contains a CI-only workflow. It does not change application code.

## What the artifact is

Each platform produces `<app-id>-<version>-<platform>.zip` containing:

- `app/` — the Shiny application directory;
- `rlib/` — the pinned R package closure (R 4.6.1);
- `manifest.json` — identity, entry, platform and package inventory.

It is an **app bundle for the ShinyApps desktop shell**, not a standalone R
installation and not a desktop launcher. The shell supplies the R runtime.

## Reproducibility and versioning

- A release is built from exactly one tag; the workflow checks out that tag.
- The tag (without `v`) must equal `DESCRIPTION` `Version`; the build fails on a mismatch.
- CRAN inputs are pinned to the `CRAN_SNAPSHOT` date via a Posit Package Manager snapshot.
- The resolved package set is written to `deps-<platform>.json` and published with the release.
- `meta-<platform>.json` records the source repository, commit, tag, R version, Pandoc version, snapshot date, zip size and SHA-256.

## Dependency policy

- A required dependency missing from the configured repositories is a hard failure.
- Transitive dependencies are resolved recursively; unresolved ones fail the build.
- The smoke test runs with `R_LIBS_USER`/`R_LIBS_SITE` empty and only `rlib/` plus base R on `.libPaths()`, so runner libraries cannot mask a missing bundle dependency.

## Report toolchain

`exmort` renders reports with R Markdown. The CI job verifies that `pandoc` is
available and records its version. The bundle does **not** contain Pandoc or a
LaTeX distribution.

- HTML/Word report rendering requires a Pandoc installation on the user's machine.
- PDF output additionally requires a LaTeX installation (for example TinyTeX).

These are documented as runtime prerequisites rather than silently assumed.

## Release safety

- Only tag pushes (or a manual run with an explicit tag) publish.
- The release job alone has `contents: write`; build jobs are read-only.
- Existing release assets are never overwritten. If an asset with the same name
  already exists, the release fails.
- Existing release notes/metadata are not rewritten; checksums and metadata are
  added as separate assets.

## Testing boundary

CI verifies that the bundle can be assembled and that the app object loads from
`rlib/` in a clean library. It does **not** exercise the actual desktop shell on
Windows/macOS, and it does not simulate a full user session (data import, model
run, report download). Those require a shell-level end-to-end test and are
reported separately.
