#initial clean up
rm(list = ls())
graphics.off()
cat("\f")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(GLM3r)
library(glmtools)
library(ggplot2)
library(devtools)

init_param <<- data.frame('pars' = as.character(c('wind_factor','lw_factor')),
           'lb' = c(0.7,0.7),
           'ub' = c(2,2),
           'x0' = c(1,1),
           'sd' = c(0.56, 0.67))

pars <<- init_param$pars
obs <<- read_field_obs('bcs/ME_observed.csv')
nml.file <<- 'glm3.nml'
path <<- getwd() 

mod2obs <- function(mod_nc, obs, reference = 'surface', var){
  deps = unique(obs[,2])
  #tim = unique(obs[,1])
  mod <- glmtools::get_var(file = mod_nc,var,reference = reference, z_out = deps)
  mod <- match.tstep(obs, mod) #From gotm_functions.R
  mod <- reshape2::melt(mod, id.vars = 1)
  mod[,2] <- as.character(mod[,2])
  mod[,2] <- as.numeric(gsub(paste(var,"_",sep=''),'',mod[,2]))
  colnames(mod) <- c('DateTime', 'Depth', var)
  mod <- mod[order(mod$DateTime, mod$Depth),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1,2), all.x = T)
    #mod <- merge(obs, mod, by = c(1,2), all = T)
    mod <- mod[order(mod$DateTime, mod$Depth),]
    mod <- mod[,c(1,2,4)]
    colnames(mod) <- c('DateTime', 'Depth', var)
  }
  return(mod)
}

match.tstep <- function(df1, df2){
  if(df1[1,1] == df1[2,1]){
    df = data.frame(DateTime = unique(df1[,1]))
    df = merge(df, df2, by = 1)
    return(df)
  }else{
    df = df2[(df2[,1] %in% df1[,1]),]
    return(df)
  }
}

likelihood <- function(p){
  eg_nml <- read_nml(nml.file)
  
  for(i in 1:length(pars[!duplicated(pars)])){
    if (any(pars[!duplicated(pars)][i] == pars[duplicated(pars)])){
      eg_nml <- set_nml(eg_nml, pars[!duplicated(pars)][i], 
                        p[which(pars[!duplicated(pars)][i] == pars)])
    } else {
      eg_nml <- set_nml(glm_nml = eg_nml,arg_name = as.character(pars[!duplicated(pars)][i]),p[!duplicated(pars)][i])
    }
  }
  
  write_path <- nml.file
  write_nml(eg_nml, file = write_path) 
  
  error <- try(run_glm(verbose = FALSE))
  while (error != 0){
    error >- try(run_glm(verbose = FALSE))
  }
  
  mod <- mod2obs(mod_nc = paste0(path,'/output/output.nc'), obs = obs, reference = 'surface', 'temp')
  
  singlelikelihoods <- dnorm(obs[,3], mean = mod[,3], log = T)
  sumll = sum(singlelikelihoods, na.rm = TRUE)
  return(sumll)
}

prior <- function(p){
  priors <- 0
  for (ii in 1:length(p)){
   priors <- priors + dnorm(p[ii], sd=  init_param$sd[ii], log = T) 
  }
  return(priors)
}

posterior <- function(p){
  return(likelihood(p) + prior(p))
}

proposalfunction <- function(p){
  return(rnorm(2,mean= p, sd = c(0.55,0.55)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim= c(iterations+1,2))
  chain[1,] = startvalue
  for (i in 1:iterations){
    print(paste0(i,'/',iterations))
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    } else {
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

startvalue = c(1,1)
chain = run_metropolis_MCMC(startvalue, iterations = 900)
burnIn = 450
acceptance = 1 -mean(duplicated(chain[-(1:burnIn),]))

df_chain <- data.frame('iter' = seq(1,901,1), 'wind_factor' = chain[,1], 'lw_factor' = chain[,2])

ggplot(df_chain, aes(iter, wind_factor, col = 'wind_factor')) +
  geom_line() +
  geom_line(aes(iter, lw_factor, col = 'lw_factor')) +
  theme_bw()
