# --- Git commit + push (repo-wide, deploy-key via SSH) -------------------------
git_commit_push_daily <- function(
  repo_path,
  commit_prefix = "Daily update",
  tz = "America/Los_Angeles"
) {
  stopifnot(dir.exists(repo_path))
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(repo_path)

  # Helpful for cron: ensure git is available
  if (system("git --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("git not found in PATH (cron may have a limited PATH).")
  }

  # Make sure we’re in a git repo
  if (!file.exists(".git")) {
    stop("Not a git repository: ", repo_path)
  }

  # (Optional but nice) ensure remote is SSH
  remote <- suppressWarnings(system("git remote get-url origin", intern = TRUE))
  if (length(remote) == 0 || !grepl("^git@github\\.com:", remote[1])) {
    message("Warning: origin is not an SSH URL: ", remote[1])
  }

  # Stage everything (new files, modified files, deletions)
  system("git add -A")

  # If nothing staged, bail
  has_changes <- system("git diff --cached --quiet") != 0
  if (!has_changes) {
    message("No changes to commit.")
    return(invisible(FALSE))
  }

  # Daily marker commit message
  day_label <- format(as.POSIXct(Sys.time(), tz = tz), "%B %-d, %Y")
  commit_msg <- paste0(commit_prefix, " - ", day_label)

  # Commit
  commit_cmd <- sprintf('git commit -m "%s"', commit_msg)
  rc_commit <- system(commit_cmd)
  if (rc_commit != 0) stop("git commit failed.")

  # Push current branch to origin
  # (avoids guessing main/master; uses whatever branch is checked out)
  rc_push <- system("git push origin HEAD")
  if (rc_push != 0) stop("git push failed.")

  message("Pushed: ", commit_msg)
  invisible(TRUE)
}

git_commit_push_daily(repo_path = "~/GitHub/Russia-Ukraine", commit_prefix = "Daily marker")
