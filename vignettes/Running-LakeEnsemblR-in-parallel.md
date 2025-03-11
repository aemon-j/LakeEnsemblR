## Running LakeEnsemblR in parallel

It is possible to run the `run_ensemble` and `cali_ensemble` functions in parallel, with each model assigned to a separate core, using the `parallel = TRUE` argument. This will speed up the computation, although the runtime depends on the slowest model. It is currently not possible to divide the tasks of these functions even more efficiently over cores. 

However, if you are planning to run LakeEnsemblR repeatedly for multiple lakes/forcings/parameter sets and you work in Rstudio, it is possible to use Rstudio's "jobs" to have these repeated runs done in parallel. There are probably other ways of doing something similar, but here we'll give some instructions on how to set this up. 

### 1. Divide tasks over cores
There are many ways of doing this, but if you can estimate how long each individual run takes, you should try to divide these tasks equally over the amount of cores that you want to use. A good option might be to order the tasks from long to short, loop through these tasks, and put each task on the core that is currently most empty. You will now have a data.frame with all the tasks and the core/job that you want to run it on. We'll call this data.frame "df_tasks". 

### 2. Make a script that runs LakeEnsemblR on a single core
You now need to make a script that runs LakeEnsemblR on a single core; let's call it "run_LER_on_core.R". In Step 3, we'll call this script in each separate job. This script will be able to access the global environment of your main script (you will have to reload libraries), but it should only run the tasks set for this specific job/core. So, a good idea for a first line is `df_tasks <- df_tasks[df_tasks$Core == core_job,]`, where `Core` is the column in df_tasks that states on what core the task in that row should be run. `core_job` we'll define in Step 3. 

After this, you need to write a loop over the rows in df_tasks, and this row should contain the information needed to run LakeEnsemblR. For example, the row can point to a folder where you have all the necessary files. In that case, your next lines should be setwd(...), export_config(...), run_ensemble(...). Because it is a loop, if you have multiple tasks on one core, these will be done after each other. 

### 3. Start the separate cores
In Step 1 you have decided how many cores you want to use, let's call this "num_cores". In this case, the code to run your simulations will look like this:

```
for(i in seq_len(num_cores)){
  core_job <- i
  rstudioapi::jobRunScript(path = "run_LER_on_core.R",
                           importEnv = TRUE,
                           name = paste0("job_", i))
}
```
In this case, the "run_LER_on_core.R" script is in your working directory. 

This will start your run on multiple cores. 

(Inspiration for this set-up came from this [blog post](https://edwinth.github.io/blog/parallel-jobs/).) 
