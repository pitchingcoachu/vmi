# School-specific overrides for the shared app.
# Copy this file to another repo and keep the same structure when you need to customize colors, logos, APIs, etc.

parse_auth_db_config <- function(path = "auth_db_config.yml") {
  if (!file.exists(path)) return(list())
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  cfg <- list()
  for (line in lines) {
    if (!grepl(":", line)) next
    parts <- strsplit(line, ":", fixed = TRUE)[[1]]
    key <- trimws(parts[1])
    value <- trimws(paste(parts[-1], collapse = ":"))
    cfg[[key]] <- value
  }
  cfg
}

load_neon_config <- function(team_code = "VMI", yaml_path = "auth_db_config.yml") {
  yaml_cfg <- parse_auth_db_config(yaml_path)
  pick <- function(env_name, yaml_key, default = "") {
    val <- Sys.getenv(env_name, "")
    if (nzchar(val)) return(val)
    yval <- yaml_cfg[[yaml_key]]
    if (!is.null(yval) && nzchar(yval)) return(yval)
    default
  }

  list(
    host = pick("NEON_HOST", "host"),
    port = pick("NEON_PORT", "port", "5432"),
    dbname = pick("NEON_DB", "dbname"),
    user = pick("NEON_USER", "user"),
    pass = pick("NEON_PASSWORD", "password"),
    sslmode = pick("NEON_SSLMODE", "sslmode", "require"),
    schema = pick("NEON_SCHEMA", "schema", "public"),
    table_prefix = pick("NEON_TABLE_PREFIX", "table_prefix", tolower(team_code))
  )
}

default_team_code <- "VMI"
school_config <- list(
  team_code = default_team_code,
  # Player filters
  allowed_pitchers = c(
  "Jones, Andrew",
  "Driscoll, Clark",
  "Biernot, Gavin",
  "Sipe, Hunter",
  "Douthat, Jim",
  "Spiegel, Justin",
  "Williamson, Nolan",
  "Riley, Owen",
  "Dhein, Peyton",
  "Velasquez, Roberto",
  "Taylor, Carson",
  "Tyndall, Eli",
  "Ahrens, Gary",
  "Chevalier, George",
  "Melescu, Miles",
  "Lafine, Noah",
  "Monroe, Trace",
  "Bassett, Tyler"
  ),
  allowed_hitters = c(
    "Wentworth, TP",
    "LeBlanc, Bryce",
    "Lund, Ethan",
    "Fyke, Kai",
    "Rhodes, Stormy",
    "Wech, Noah",
    "Brown, Matthew",
    "Phillips, Brennan",
    "Blake, Drew",
    "Glendinning, Lucas",
    "Golden, Josiah",
    "Kennedy, Jake",
    "Barrett, Hudson",
    "Zagar, Kyler",
    "Albright, Gaige",
    "Sramek, Caden",
    "Jennings, Parker",
    "Burns, Zane",
    "Winslow, Drew",
    "Pearcy, Kyle",
    "Turner, Cael",
    "Pesca, Mario",
    "Watkins, Hunter",
    "Thompson, Brock",
    "Meola, Aidan",
    "Bowen, Terrance",
    "Smithwick, Campbell",
    "Shull, Garrett",
    "Indomenico, Remo",
    "Ortiz, Avery",
    "Wallace, Danny",
    "Brueggemann, Colin",
    "Ritchie, Kollin",
    "Conover, Alex",
    "Norman, Sebastian",
    "Essex, Ezra",
    "Saunders, Evan",
    "Pladson, Cole",
    "Schambow, Quinn",
    "Kennedy, Ty",
    "Francisco, Brady",
    "Pomeroy, Deacon"
  ),
  allowed_campers = c(
    "Bowman, Brock",
    "Daniels, Tyke",
    "Pearson, Blake",
    "Rodriguez, Josiah",
    "James, Brody",
    "Nevarez, Matthew",
    "Nunes, Nolan",
    "Parks, Jaeden",
    "Hill, Grant",
    "McGinnis, Ayden",
    "Morton, Ryker",
    "McGuire, John",
    "Willson, Brandon",
    "Lauterbach, Camden",
    "Turnquist, Dylan",
    "Bournonville, Tanner",
    "Evans, Lincoln",
    "Gnirk, Will",
    "Mann, Tyson",
    "Neneman, Chase",
    "Warmus, Joaquin",
    "Kapadia, Taylor",
    "Stoner, Timothy",
    "Bergloff, Cameron",
    "Hamm, Jacob",
    "Hofmeister, Ben",
    "Moo, Eriksen",
    "Peltz, Zayden",
    "Huff, Tyler",
    "Moseman, Cody"
  ),
  colors = list(
    primary             = "#000000",   # black from logo
    accent              = "#ffd619",   # bright yellow highlight
    accent_secondary    = "#ae122a",   # red gradient stop
    background          = "#ffffff",   # clean light page base
    background_secondary= "#f4f4f4"    # subtle off-white
    
  ),
  logo = "VMIlogo.png",
  coaches_emails = c(
    "crosbyac@vmi.edu"
  ),
  notes_api = list(
    base_url = "https://script.google.com/macros/s/AKfycbxE-gkSNBlnEmwf2VZVEkYlZ5YcsonF-Ap7A4v6y3HuaxPB4ikFe6Pc3HoEQ3UYstPkmw/exec",
    token = "vmibaseball"
  ),
  extra = list(
    school_name = "VMI",
    ftp_folder = "trackman",
    cloudinary_folder = "trackman"
  ),
  neon = load_neon_config(team_code = default_team_code)
)

colorize_css <- function(css, accent, accent_secondary, background, background_secondary) {
  accent_rgb <- paste(grDevices::col2rgb(accent), collapse = ",")
  accent_secondary_rgb <- paste(grDevices::col2rgb(accent_secondary), collapse = ",")
  css <- gsub("#e35205", accent, css, fixed = TRUE)
  css <- gsub("#ff8c1a", accent_secondary, css, fixed = TRUE)
  css <- gsub("rgba(227,82,5", paste0("rgba(", accent_rgb), css, fixed = TRUE)
  css <- gsub("rgba(227, 82, 5", paste0("rgba(", accent_rgb), css, fixed = TRUE)
  css <- gsub("rgba(255,140,26", paste0("rgba(", accent_secondary_rgb), css, fixed = TRUE)
  css <- gsub("rgba(255, 140, 26", paste0("rgba(", accent_secondary_rgb), css, fixed = TRUE)
  css <- gsub("#f5f7fa", background, css, fixed = TRUE)
  css <- gsub("#e8ecf1", background_secondary, css, fixed = TRUE)
  css
}
