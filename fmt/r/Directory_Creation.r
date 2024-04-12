#Directory_Creation 

dir.create("app")
dir.create("r")
dir.create("py")
dir.create("data")
dir.create("tests")
dir.create("www")

file.create(c("app/server.R", "app/ui.R", "app/global.R", "app/app.R",
              "r/functions.R", 
              "py/icp_alignment.py",
              "Dockerfile",
              ".gitignore",
              "README.md",
              "requirements.txt",
              "DESCRIPTION"))