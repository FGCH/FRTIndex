# Set up RStudio, JAGS, and Stan on an Amazon EC2 instance
# Using Ubuntu 64-bit
# Partially from http://blog.yhathq.com/posts/r-in-the-cloud-part-1.html
# See yhat for EC2 instance set up

# Navigate to key pair
# ssh -i YOUR_KEYPAIR.pem ubuntu@PUBLIC_DNS

# Add a user/password.
# This will become your RStudio username and password.
sudo adduser USER_NAME

# Get all programs up to date
sudo apt-get update

# Install R and JAGSS
sudo add-apt-repository ppa:marutter/rrutter
sudo apt-get install r-base-dev jags r-cran-rjags

# Check that you have the latest R instal
## see also: http://askubuntu.com/a/352438
sudo apt-get update
apt-cache showpkg r-base

sudo apt-get install -f r-base= PACKAGE_VERSION

# Install git
sudo apt-get install git
sudo apt-get update

# Configure git
git config --global user.name GIT_USER_NAME
git config --global user.email GIT_USER_EMAIL

# Install g++ (needed for Stan, and generally useful)
sudo apt-get install g++

# Install RStudio
## for latest version of RStudio see http://www.rstudio.com/ide/download/server
sudo apt-get install gdebi-core
sudo apt-get install libapparmor1
wget http://download2.rstudio.org/rstudio-server-0.98.945-amd64.deb
sudo gdebi rstudio-server-0.98.945-amd64.deb

# Verify RStudio installation
sudo rstudio-server verify-installation

#### Git clone your repo as usual, ideally into /home/USER_NAME ####
# This is where RStudio server looks.

# Change owner of the repo to USER_NAME
sudo chown -R USER_NAME /home/USER_NAME/REPO_NAME


# Install Stan
## See: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
## In the command line check the number of cores
nproc

## In R set number of course in Sys.setenv
# Sys.setenv(MAKEFLAGS = "-j4")

# source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)

# Access RStudio Server with http://PUBLIC_DNS:8787
