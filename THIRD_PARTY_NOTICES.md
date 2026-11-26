# Third-party software and licenses

FluoR's original R/Shiny source code is distributed under the MIT License.

The standalone Windows distribution incorporates third-party software that remains subject to its respective licenses.

## Main components

| Component | Version | License | Upstream project |
|---|---|---|---|
| R | 4.6.1 | GPL-2 or GPL-3 | [R Project](https://www.r-project.org/) |
| Portable R for Windows | 4.6.1 | See component licenses | [Portable R](https://github.com/portable-r/portable-r-windows) |
| shinyelectron | 0.2.1 | AGPL-3.0-or-later | [shinyelectron](https://github.com/coatless-rpkg/shinyelectron) |
| Electron | 41.10.7 | MIT and third-party licenses | [Electron](https://github.com/electron/electron) |
| electron-builder (build tool) | 26.18.0 | MIT | [electron-builder](https://github.com/electron-userland/electron-builder) |

## R packages

The standalone distribution includes R packages obtained through the R package ecosystem. These packages retain their individual licenses and copyright notices.

## Electron and Chromium

Electron incorporates Chromium, Node.js and additional third-party components.

Their license information is provided in the following files in the desktop distribution:

- `LICENSE.electron.txt`
- `LICENSES.chromium.html`

## Source code

The original FluoR source code is available at:

https://github.com/jjsanchezgil/FluoR

Upstream source repositories are listed above. Version-specific dependency information and corresponding source materials must accompany the relevant binary distribution as required by the applicable licenses.

The MIT License applicable to FluoR does not supersede the licenses of third-party components.

## Version-specific source materials (v0.1.0)

- [R 4.6.1 source code](https://cran.r-project.org/src/base/R-4/R-4.6.1.tar.gz)
- [Portable R 4.6.1 for Windows](https://github.com/portable-r/portable-r-windows/releases/tag/v4.6.1)
- [Portable R build repository](https://github.com/portable-r/portable-r-windows)
- [Electron 41.10.7](https://github.com/electron/electron/tree/v41.10.7)
- [electron-builder 26.18.0](https://github.com/electron-userland/electron-builder/tree/v26.18.0)
- [shinyelectron](https://github.com/coatless-rpkg/shinyelectron)

The R package source archives, dependency manifest and generated Electron application source files are provided alongside the Windows installer on the FluoR Releases page.
