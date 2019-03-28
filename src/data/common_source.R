participants = c("p01" = "974933df-1681-4c2d-8006-20a59327e81a",
                 "p02" = "87be2da7-8413-4345-bbd4-687e83c369ad",
                 "p03" = "900459ad-aea2-45d3-9a2b-930c677d0795",
                 "p04" = "19156017-dafb-437e-a371-844caf6452c6",
                 "p05" = "b26a9b78-17c9-447a-83b5-9a511ec52ae6",
                 "p06" = "e2e8a376-9dbb-4b87-9c4d-30ff11d7f22c",
                 "p07" = "34ccaa7f-3ad4-4a5c-bfc9-f07ee3191b90",#429c8e47-9b9e-40e8-9722-4369892f6fea (first install)
                 "p08" = "e5981cc8-d738-465f-89a4-cbe9b1a54071",
                 "p09" = "d4cfded4-3845-4e9b-8cfd-fecf0c8b644e",
                 "p10" = "8a7ca54e-7db8-458e-8b87-b024561cd719",
                 "p11" = "da4b523b-55a2-4040-ae05-b8a8baf1e9e1",
                 "p12" = "b1c2e555-2829-40eb-a77a-d8b4b0a99751",
                 "p13" = "66a0ff30-728c-40e3-8f42-5ca745cb3a53") 

tables_names = c("aware_device","ball_game", "health", "medication", "notification_data", "consent")

get_file_path <- function(p_id, folder, parent_folders = "../data/processed"){
  return(paste0(parent_folders,"/",folder,"/", p_id, ".csv"))
}

get_tz <- function(p_id){
  if(p_id %in% c("p01","p02","p03","p04","p05","p06","p07"))
    return("Europe/Helsinki")
  else
    return("Europe/London")
}