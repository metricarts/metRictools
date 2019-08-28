#' Link Files folder
#'
#' Move files to Blob
#' @export
#' @author Daniel Fischer

toBlob = function(){
  if(version$os == "mingw32"){
    print('linux AWS only function')
    return(FALSE)
  }
  repo_folder = paste0("/media/blob/files_project/")
  current_repo = basename(getwd())
  avivable_repos = basename(list.dirs(repo_folder,recursive = F))
  proyect_path = paste0(repo_folder,current_repo)
  if(current_repo %in% avivable_repos){
    print("Repository avivable, just link with linkBlob()")
  }else{
    dir.create(proyect_path,recursive = T)
    file.copy("./files",proyect_path,recursive = T)
    print("Remove files folder and then run linkBlob()")
  }
  
}