# One time process when you install Python for the first time
#install.packages("reticulate")
library(reticulate)
# Create a new environment
version <- "3.13.2" # Go to https://www.python.org/downloads/ to find the latest version of Python
install_python(version = version)
virtualenv_create("my-python", python_version = version)


# Run this every time before you want to use Python
library(reticulate)
use_virtualenv("my-python", required = TRUE)


# Run these commands when you want to install a package
virtualenv_install(envname = "my-python",
                   "numpy",
                   ignore_installed = FALSE,
                   pip_options = character())

virtualenv_install(envname = "my-python",
                   "pandas",
                   ignore_installed = FALSE,
                   pip_options = character())

virtualenv_install(envname = "my-python",
                   "matplotlib",
                   ignore_installed = FALSE,
                   pip_options = character())

