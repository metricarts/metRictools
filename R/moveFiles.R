#' Link Files folder
#'
#' Move files to Blob
#' @export
#' @author Daniel Fischer

moveFiles = function(){
  safeLibrary("rstudioapi")
  #createAwsFilesRepo(path = path)
  
  
  current_repo = basename(getwd())
  param = c(proj = current_repo)
  dir.create(sqlGsub('/media/blob/files_project/@proj@/',param),recursive = T)
  cmd = sqlGsub('mv ./files /media/blob/files_project/@proj@/',param)
  print(cmd)
  system(cmd)
}