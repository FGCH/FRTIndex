# Set up RStudio and JAGS on an Amazon EC2 instance
# Partially from http://blog.yhathq.com/posts/r-in-the-cloud-part-1.html 
# See yhat for EC2 instance set up

# Navigate to key pair
# ssh -i {yourkey pair}.pem ubuntu@ PUBLIC DNS

# Add a user
sudo adduser name

# Get all programs up to date
sudo apt-get update

# Install R and JAGSS
sudo add-apt-repository ppa:marutter/rrutter
sudo apt-get install r-base-dev jags r-cran-rjags

# Install git
sudo apt-get install git

# Configure git
git config --global user.name 'USER NAME'
git config --global user.email 'USER EMAIL'

# Install RStudio
## for latest version of RStudio see http://www.rstudio.com/ide/download/server
sudo apt-get install gdebi-core
sudo apt-get install libapparmor1
wget http://download2.rstudio.org/rstudio-server-0.98.501-amd64.deb
sudo gdebi rstudio-server-0.98.501-amd64.deb

# Verify RStudio installation
sudo rstudio-server verify-installation


# Access with http:// PUBLIC DNS :8787