############################################################################
#
# Author: Daniel M. Griffith
# Date:
# Description:
#   converting nice Python code into ugly R code.
#   original code from:
#     File: 3pg.py
#     Author: joeyzhou1984@gmail.com
#     Description: the entry point of 3-PG model
#     Created: 2012-01-02
#     LastModified: 2012-01-02
############################################################################


##import sys
### YIK update on 8/2/16: PC
##sys.path.append('../lib')
### YIK update on 8/2/16: Mac
###sys.path.append('lib/')

##from Model3PG import Model3PG

##read_control_file <- function(fpath){
##  model <- Model3PG(fpath)
##  model.initialize() # DMG need work
##  return(model)
##}

run_3pg <- function(config, climate = NULL, output = NULL){
    # read the control file

    ##    print '%s is not a valid control file.' % fpath_control
    ##    return
    ##model_3pg.run()

  output_list <- list()
  #TEST DMG
   # config <- "test_data/Test_EO_cfg.cfg"
   # climate <- "test_data/Test_EO_in.txt"
   # output <- "test_tmp.txt"

    output_list <- instance3PG(config, climate, output)
    
   #if(length(climate) == length(config) & length(config) == length(output)){
      ###for(cfg in 1:length(config)){
      ###  output_list[[basename(config[cfg])]] <- instance3PG(config[cfg], climate[cfg], output[cfg])
      ###}
  #} else if(length(config) > 1 & length(climate) == 1){
  #    for(cfg in 1:length(config)){
  #      output[basename(config[cfg])] <- instance3PG(config[cfg], climate)
  #    }
  #} else {
  #    print("Number of configuration and climate files does not make sense.")
  #}
  return(output_list)
}

##main <- function(argv){ # DMG need work - basically just needs to be writend so that multiple config files can be written.
##    if(length(argv) == 1){
##        print('Please provide at least one control file for running the model')
##        # run_3pg('run1.yaml')
##    } else {
##        for(fpath in argv){
##          run_3pg(fpath)
##        }
##    }
##}

##if __name__ == '__main__':
##    main(sys.argv)
