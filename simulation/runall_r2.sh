dos2unix ~/VT/Chuyu/Scripts/run_vt_sim.sh


# SIX SCENARIOS ###############################################################
#### linear covariate effects, no TEH
sbatch --export=c_name='R102',dg=lin_null_dg_r2,vt1=i.rf.fast,sims=20 --job-name=R10.rf -t 40:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='R104',dg=lin_null_dg_r2,vt1=i.super.fast,sims=20 --job-name=R10.super -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### linear covariate effects, linear TEH
sbatch --export=c_name='R112',dg=lin_lin_dg_r2,vt1=i.rf.fast,sims=20 --job-name=R11.rf -t 40:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='R114',dg=lin_lin_dg_r2,vt1=i.super.fast,sims=20 --job-name=R11.super -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### linear covariate effects, nonlinear TEH
sbatch --export=c_name='R122',dg=lin_nonlin_dg_r2,vt1=i.rf.fast,sims=20 --job-name=R12.rf -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='R124',dg=lin_nonlin_dg_r2,vt1=i.super.fast,sims=20 --job-name=R12.super -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### nonlinear covariate effects, no TEH
sbatch --export=c_name='R202',dg=nonlin_null_dg_r2,vt1=i.rf.fast,sims=20 --job-name=R20.rf -t 40:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='R204',dg=nonlin_null_dg_r2,vt1=i.super.fast,sims=20 --job-name=R20.super -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### nonlinear covariate effects, linear TEH
sbatch --export=c_name='R212',dg=nonlin_lin_dg_r2,vt1=i.rf.fast,sims=20 --job-name=R21.rf -t 40:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='R214',dg=nonlin_lin_dg_r2,vt1=i.super.fast,sims=20 --job-name=R21.super -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh

#### nonlinear covariate effects, nonlinear TEH
sbatch --export=c_name='R222',dg=nonlin_nonlin_dg_r2,vt1=i.rf.fast,sims=20 --job-name=R22.rf -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
sbatch --export=c_name='R224',dg=nonlin_nonlin_dg_r2,vt1=i.super.fast,sims=20 --job-name=R22.super -t 48:00:00 --array=1-100 ~/VT/Chuyu/Scripts/run_vt_sim.sh
