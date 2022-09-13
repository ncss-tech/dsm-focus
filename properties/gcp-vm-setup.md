---
title: "Google Cloud Platform (GCP) Virtual Machine (VM) Setup"
author: "Stephen Roecker"
date: "6/22/2022"
output: word_document
---

## GCP VM setup

This document was based on the following [Medium](https://medium.com/analytics-vidhya/running-r-rstudio-in-a-gcp-vm-21a8458ef086) article.

GCP offers a variety of machine types and operating systems when configuring VMs. For testing purpose, the configuration listed below was used. Other options maybe more preferable depending on the computing resources required. When setting up a new VM, be sure to give a descriptive name that indicates the primary user and other relevant information, such as in the following example.

vm name = vm_smroecker_e2-standard-32_ubuntu18-rstudio machine type = e2-standard-32 operating systems = Ubuntu 18.04

```{bash}

gcloud compute instances create vm-smroecker-ubuntu20-rstudio-test --project=ncss-30m-covariates --zone=us-west4-a --machine-type=e2-medium --network-interface=network-tier=PREMIUM,subnet=default --maintenance-policy=TERMINATE --provisioning-model=STANDARD --service-account=189168473314-compute@developer.gserviceaccount.com --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --tags=http-server --create-disk=auto-delete=yes,boot=yes,device-name=vm-smroecker-ubuntu20-rstudio-test,image=projects/ubuntu-os-cloud/global/images/ubuntu-1804-bionic-v20220610,mode=rw,size=10,type=projects/ncss-30m-covariates/zones/us-west4-a/diskTypes/pd-balanced --no-shielded-secure-boot --shielded-vtpm --shielded-integrity-monitoring --reservation-affinity=any

```

After the VM is setup it is still necessary to install additional software applications. This can be done by starting the VM instanse, opening the SSH, and by entering the bash commands in later steps.

### Configure the VM firewall

In order to make RStudio Server available via an external IP, the VM firewall needs to be configured. Go to the VPC Network tab Firewall settings. Edit the 'default allow-http' firewall settings. Set Action on match to "Allow", the Protocols and Ports to "8787", and the Target tag to "http-server".

### Update OS and install package library

```{bash}
sudo apt-get update
sudo apt -y install gdebi-core
```

### Installs applications required for GIS R packages (e.g. sf, terra)

```{bash}
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libmysqlclient-dev 
```

### Install current version of R (<https://cloud.r-project.org/>)

```{bash}
# R install https://cloud.r-project.org/
# update indices
sudo apt update -qq
# install two helper packages we need
sudo apt install --no-install-recommends software-properties-common dirmngr
# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt install --no-install-recommends r-base
```

### Install software necessary for the tidyverse

```{bash}
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
```

### Install RStudio 

-   Check the following [link](https://www.rstudio.com/products/rstudio/download-server/debian-ubuntu/) for the most recent version of RStudio Server

```{bash}
# download
wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-2022.02.3-492-amd64.deb

# install
sudo gdebi rstudio-server-2022.02.3-492-amd64.deb

# create username and password
sudo adduser smroecker
```

### Log into RStudio Server

-   Start the VM
-   Copy the External IP, paste into a browser window, and append the IP address with ":8787"
-   Login with the username and password you created

```{r}
auth <- "../ncss-30m-covariates-a4b6d9ca3712.json"
prj  <- "ncss-30m-covariates"
zone <- "us-west4-b"
pwd  <- ""

Sys.setenv("GCS_AUTH_FILE" = auth
#            "GCS_DEFAULT_PROJECT_ID" = prj,
#            "GCS_DEFAULT_ZONE" = zone
)

library(googleCloudStorageR)
library(raster)

gcs_get_bucket("100m-variables-all")
gcs_global_bucket("100m-variables-all")
objects <- gcs_list_objects()


test <- gcs_get_object("covariates/BARL10.tif", parseFunction = raster) # From Dave White


idx <- grepl(".tif$", objects$name) & grepl("^covariates/", objects$name)
objects$name[idx]
```


## Other GCP options

### RStudio Workbench via the Marketplace

This option was explored, but wasn't unsuccessful because the auto-generated username and password didn't work.

### RStudio Server via googleComputeEngineR R package

This option was explored, but it turns out the Docker image used doesn't allow changes to be saved. Thus R packages need to be re-installed each time the VM is restarted. Also, it wasn't configured to allow the installation of the sf and terra R packages.

```{r}
auth <- "/Users/stephen.roecker/OneDrive - USDA/code/gce_covariates_smroecker.json"
prj  <- "ncss-30m-covariates"
zone <- "us-west4-b"
pwd  <- ""

Sys.setenv("GCE_AUTH_FILE" = auth,
           "GCE_DEFAULT_PROJECT_ID" = prj,
           "GCE_DEFAULT_ZONE" = zone
           )

library(googleComputeEngineR)

# gce_global_project(prj)
# gce_global_zone(zone)

# create a virtual machine
# vm <- gce_vm(template = "rstudio",
#              name = "rstudio-server",
#              username = "smroecker", 
#              password = pwd,
#              predefined_type = "n2d-highmem-96"
#              )

vm <- gce_vm("rstudio-server")

## add custom SSH keys to the VM object with assumed RStudio defaults
vm <- gce_ssh_setup(vm, 
                    # key.pub = "C://.ssh/id_rsa.pub",
                    key.private = "http://34.125.146.143/"
                    )

## test ssh
gce_ssh(vm, "echo foo")

## push your rstudio image to container registry
gce_push_registry(vm, "rstudio-server", container_name = "rstudio-server")

## launch another rstudio instance with your settings
vm2 <- gce_vm(template = "rstudio",
              name = "rstudio-server-2",
              username = "smroecker", password = pwd,
              predefined_type = "n2d-highmem-96",
              dynamic_image = gce_tag_container("rstudio-server"))

```

### Windows VM

Colby was able to get this option to work by using a Windows 2022 Datacenter OS. It opens up just like a regular Windows desktop environment.


### Vertex AI

Vertex AI offers the ability use R via Python Notebooks (.pynb), but similar to the other RStudio options, the raster R package could not be installed due to missing software.

