## Get started

1. clone git
```
# shell script

git clone <this remote repositely>
```

1. see config.yml. In local, use `default` while `shinyapp` in production at shinyapp.io
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
