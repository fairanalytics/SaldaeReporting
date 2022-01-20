#' Saldae Report Publisher
#' @author Farid Azouaou
#' @param target_repo target_repository
#' @param github_url Github remote repository
#' @export

saldae_publihser_f <- function(target_report = "Saldae_main_report.html", github_url = "git@github.com:fairanalytics/saldae_dashboard.git"){

  local_path <- "./reporting/to_publish/"

  publishing_info <- update_publisher(new_report_name = target_report, repo_path = local_path)


  #

  #. initialize the repository
  local_repo <- git2r::init(paste0(local_path,"_site"), branch = "main")
  #. add conifguration for git
  git2r::config(repo = local_repo,global = TRUE,user.name = "Fair Analytics",user.email = "fairanalytics@outlook.com" )
  repo_status <- git2r::status(repo = local_repo)

  untracked_files <- unlist(repo_status$untracked)
  git2r::add(repo = local_repo , path = untracked_files)
  git2r::commit(repo = local_repo, message = paste("publishing report:",target_report))

  git2r::remote_add(repo = local_repo, name = "origin", url = github_url)

  Sys.setenv(SSH_KEY = "AZOfar06*")
  warning("make sure to move SSH key to ENV variable")
  my_ssh_key <- git2r::cred_ssh_key(
    publickey = "C:\\Users\\Lenovo\\.ssh\\id_ed25519.pub",
    privatekey = "C:\\Users\\Lenovo\\.ssh\\id_ed25519",
    passphrase = Sys.getenv("SSH_KEY")
  )
  git2r::fetch(repo = local_repo, name = "origin", credentials = my_ssh_key)
  # git2r::pull(repo = ".", credentials = my_ssh_key)
  git2r::push(object = local_repo , name = "origin", "refs/heads/main",
              credentials  = my_ssh_key, set_upstream = TRUE)

}
#' Generate or Update publisher(Web site)

update_publisher <- function(new_report_name = NULL, repo_path = NULL){
  output <- list()
  #
  if(!dir.exists(repo_path)){
    template_folder <- "publisher"
    R.utils::copyDirectory(system.file(template_folder, package = "SaldaeReporting"), repo_path)
  }
  # copy the new report
  file.copy(from = paste0(gsub("to_publish/","",repo_path),new_report_name), to = repo_path )
  #
  site_yml_file <- paste0(repo_path,"/_site.yml")
  publisher_yml <- yaml::read_yaml(site_yml_file)
  #
  available_reports <- publisher_yml$navbar$left

  available_report_names <- lapply(available_reports, function(x)x$text)

  new_report_name <- gsub(".html", "", new_report_name)
  if(new_report_name%in%available_report_names){
    output$is_new <- FALSE
    report_index <- which(available_report_names==new_report_name)
  }else{
    output$is_new <- TRUE
    report_index <- length(available_reports)+1
    available_reports[[report_index]] <- list()
    available_reports[[report_index]][["text"]] <- new_report_name
    available_reports[[report_index]][["href"]] <- new_report_name
    publisher_yml$navbar$left  <- available_reports
    yaml::write_yaml(x = publisher_yml, file = site_yml_file)
  }

  rmarkdown::render_site(input  = repo_path, encoding = 'UTF-8')
  print("Publisher site successfully run")
  return(output)
}
