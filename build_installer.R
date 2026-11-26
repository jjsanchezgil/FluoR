
# ============================================================
# FluoR - Windows desktop installer
# Version 0.1
# ============================================================

app_version <- "0.1.0"

# Run from the FluoR project root in RStudio
app_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

# Verify project structure
required_files <- c(
  "app.R",
  "R/standards_editor.R",
  "R/samples_editor.R",
  "R/pipetting_order.R",
  "R/quantification.R",
  "icon/fluor-icon.ico"
)

stopifnot(
  all(file.exists(file.path(app_dir, required_files)))
)

# Install packaging tools if necessary
if (!requireNamespace("shinyelectron", quietly = TRUE)) {
  install.packages("shinyelectron")
}

# Check dependencies needed by this build script
for (pkg in c("yaml", "jsonlite", "processx")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing build dependency: ", pkg)
  }
}

# Check application dependencies
print(
  shinyelectron::app_dependencies(
    app_dir,
    app_type = "r-shiny"
  )
)

# Build outside the source directory
build_dir <- file.path(
  dirname(app_dir),
  "FluoR-build"
)

# ============================================================
# Application metadata
# ============================================================

config_file <- file.path(app_dir, "_shinyelectron.yml")

config <- if (file.exists(config_file)) {
  yaml::read_yaml(config_file)
} else {
  list()
}

if (is.null(config)) config <- list()
if (is.null(config$app)) config$app <- list()

config$app$name <- "FluoR"
config$app$version <- app_version

yaml::write_yaml(config, config_file)

# ============================================================
# Build Windows installer with bundled R
# ============================================================

shinyelectron::export(
  appdir = app_dir,
  destdir = build_dir,
  app_name = "FluoR",
  app_type = "r-shiny",
  runtime_strategy = "bundled",
  icon = file.path(app_dir, "icon", "fluor-icon.ico"),
  platform = "win",
  arch = "x64",
  overwrite = TRUE,
  build = TRUE,
  sign = FALSE
)

# ============================================================
# Configure the final Windows executable name
# ============================================================

electron_dir <- file.path(build_dir, "electron-app")
pkg_path <- file.path(electron_dir, "package.json")

stopifnot(file.exists(pkg_path))

pkg <- jsonlite::fromJSON(
  pkg_path,
  simplifyVector = FALSE
)

# Verify Electron received the correct version
if (!identical(pkg$version, app_version)) {
  stop(
    "Incorrect Electron version: ",
    pkg$version
  )
}

# Application display name
pkg$productName <- "FluoR"
pkg$build$productName <- "FluoR"

# Windows executable name
pkg$build$win$executableName <- "FluoR"

# Desktop and Start Menu shortcut names
pkg$build$nsis$shortcutName <- "FluoR"

writeLines(
  jsonlite::toJSON(
    pkg,
    auto_unbox = TRUE,
    pretty = TRUE
  ),
  pkg_path
)

# ============================================================
# Rebuild Electron with the final executable name
# ============================================================

# Reuse the Node.js version already installed for FluoR
node_dir <- shinyelectron::install_nodejs(
  version = "26.13.1",
  verbose = FALSE
)

npm_cmd <- file.path(node_dir, "npm.cmd")

stopifnot(file.exists(npm_cmd))

node_path <- paste(
  node_dir,
  Sys.getenv("PATH"),
  sep = .Platform$path.sep
)

processx::run(
  npm_cmd,
  c("run", "build-win-x64"),
  wd = electron_dir,
  env = c("current", PATH = node_path),
  echo = TRUE,
  error_on_status = TRUE
)

# ============================================================
# Verify executable
# ============================================================

dist_dir <- file.path(electron_dir, "dist")

exe_path <- file.path(
  dist_dir,
  "win-unpacked",
  "FluoR.exe"
)

if (!file.exists(exe_path)) {
  stop("FluoR.exe was not generated.")
}

# ============================================================
# Rename the FINAL installer
# ============================================================

installer <- list.files(
  dist_dir,
  pattern = "Setup.*\\.exe$",
  ignore.case = TRUE,
  full.names = TRUE
)

# Exclude an already-renamed installer, if present
installer <- installer[
  !grepl("^FluoR-Setup-", basename(installer))
]

if (length(installer) != 1L) {
  stop("Expected exactly one generated Windows installer.")
}

new_installer <- file.path(
  dist_dir,
  paste0("FluoR-Setup-", app_version, ".exe")
)

if (file.exists(new_installer)) {
  stop("Target installer already exists: ", new_installer)
}

if (!file.rename(installer, new_installer)) {
  stop("Could not rename the installer.")
}

stopifnot(file.exists(new_installer))

# ============================================================
# Finished
# ============================================================

message("Windows executable: ", exe_path)
message("Windows installer: ", new_installer)
message("FluoR version: ", app_version)
message("Build completed successfully.")
