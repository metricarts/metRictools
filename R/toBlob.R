#' Link Files folder
#'
#' Move files to Blob
#' @export
#' @author Daniel Fischer

linkFiles = function(){
  if(version$os == "mingw32"){
    print('linux AWS only function')
    return(FALSE)
  }
  repo_folder = paste0("/media/blob/files_project/")
  current_repo = basename(getwd())
  avivable_repos = basename(list.dirs(repo_folder,recursive = F))
  if(current_repo %in% avivable_repos){
    print("Repository avivable, just link with linkBlob()")
  }else{
    file.copy("./files",paste0(repo_folder,current_repo,"/files"))
  }
  
}