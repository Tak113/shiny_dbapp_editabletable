## Get started

1. clone git
   ```
   # shell script
   
   git clone <this remote repositely>
   ```

1. Create `config.yml` file right under directoly setting up database environment
   ```
   #config.yml file
   
   #local environment (unix)
   default:
    dataconnection:
     driver: "PostgreSQL Driver"
     database: "postgres"
     uid: "postgres"
     pwd: "takawspostgres"
     server: "taktestdbinstance.ctu5c4ueoiyt.us-west-2.rds.amazonaws.com"
     port: 5432
   
   #remove environment (linux)
   shinyapps:
    dataconnection:
     driver: "PostgreSQL"
     database: "postgres"
     uid: "postgres"
     pwd: "takawspostgres"
     server: "taktestdbinstance.ctu5c4ueoiyt.us-west-2.rds.amazonaws.com"
     port: 5432
   ```

1. In local, use `default` while `shinyapp` in production at shinyapp.io
   ```
   # R script at app.R
   
   # local
   Sys.setenv(R_CONFIG_ACTIVE = 'default')
   
   # remote(production)
   Sys.setenv(R_CONFIG_ACTIVE = 'shinyapps')
   ```

1. product is designed to use shinyapps.io which is based on linux based shiny server. odbc driver is set for linux in config.yml

1. You need an shinyapps.io account (you have free 5 deploy)

1. Open app.R using Rstudio IDE, pushing 'publish' button. In publish popup window, push 'publish'
